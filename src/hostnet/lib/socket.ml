open Lwt.Infix

let src =
  let src = Logs.Src.create "socket" ~doc:"Unix socket connections" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let broadcast = Ipaddr.V4.of_string_exn "255.255.255.255"

module Datagram = struct
  type reply = Cstruct.t -> unit Lwt.t

  type flow = {
    description: string;
    fd: Uwt.Udp.t;
    mutable last_use: float;
    (* For protocols like NTP the source port keeps changing, so we send
       replies to the last source port we saw. *)
    mutable reply: reply;
  }

  (* Look up by dst * dst_port *)
  let table = Hashtbl.create 7

  let _ =
    let rec loop () =
      Uwt.Timer.sleep 60_000
      >>= fun () ->
      let snapshot = Hashtbl.copy table in
      let now = Unix.gettimeofday () in
      Hashtbl.iter (fun k flow ->
          if now -. flow.last_use > 60. then begin
            Log.debug (fun f -> f "Socket.Datagram %s: expiring UDP NAT rule" flow.description);
            Lwt.async (fun () ->
              Lwt.catch (fun () ->
                let _ = Uwt.Udp.close flow.fd in
                (* FIXME(djs55): error handling *)
                Lwt.return ()
              ) (fun e ->
                Log.err (fun f -> f "Socket.Datagram %s: caught %s while closing UDP socket" flow.description (Printexc.to_string e));
                Lwt.return ()
              )
            );
            Hashtbl.remove table k
          end
        ) snapshot;
      loop () in
    loop ()

  let input ~reply ~src:_ ~dst:(dst, dst_port) ~payload =
    let remote_sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.V4.to_string dst, dst_port) in
    (if Hashtbl.mem table (dst, dst_port) then begin
        Lwt.return (Some (Hashtbl.find table (dst, dst_port)))
      end else begin
       let description = Ipaddr.V4.to_string dst ^ ":" ^ (string_of_int dst_port) in
       if Ipaddr.V4.compare dst broadcast = 0 then begin
         Log.debug (fun f -> f "Socket.Datagram.input %s: ignoring broadcast packet" description);
         Lwt.return None
       end else begin
         Log.debug (fun f -> f "Socket.Datagram.input %s: creating UDP NAT rule" description);
         let fd = Uwt.Udp.init () in
         let sockaddr =
           let unix = Unix.ADDR_INET(Unix.inet_addr_any, 0) in
           (* FIXME(djs55): another possible exception *)
           Uwt_base.Conv.of_unix_sockaddr_exn unix in
          (* FIXME(djs55): exceptions *)
         Uwt.Udp.bind_exn fd ~addr:sockaddr ();
         let last_use = Unix.gettimeofday () in
         let flow = { description; fd; last_use; reply} in
         Hashtbl.replace table (dst, dst_port) flow;
         (* Start a listener *)
         let buf = Cstruct.create 1500 in
         let rec loop () =
           Lwt.catch
             (fun () ->
                Uwt.Udp.recv_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len ~buf:buf.Cstruct.buffer fd
                >>= fun recv ->
                if recv.Uwt.Udp.is_partial then begin
                  Log.err (fun f -> f "Socket.Datagram.input %s: dropping partial response" description);
                  Lwt.return true
                end else if recv.Uwt.Udp.sockaddr = None then begin
                  Log.err (fun f -> f "Socket.Datagram.input %s: dropping response from unknown sockaddr" description);
                  Lwt.return true
                end else begin
                  flow.reply (Cstruct.sub buf 0 recv.Uwt.Udp.recv_len)
                  >>= fun () ->
                  Lwt.return true
                end
             ) (function
                 | Unix.Unix_error(Unix.EBADF, _, _) ->
                   (* fd has been closed by the GC *)
                   Log.debug (fun f -> f "Socket.Datagram.input %s: shutting down listening thread" description);
                   Lwt.return false
                 | e ->
                   Log.err (fun f -> f "Socket.Datagram.input %s: caught unexpected exception %s" description (Printexc.to_string e));
                   Lwt.return false
               )
           >>= function
           | false -> Lwt.return ()
           | true -> loop () in
         Lwt.async loop;
         Lwt.return (Some flow)
       end
     end) >>= function
    | None -> Lwt.return ()
    | Some flow ->
      flow.reply <- reply;
      Lwt.catch
        (fun () ->
           (* FIXME(djs55): of_unix_sockaddr_exn *)
           let remote_sockaddr = Uwt_base.Conv.of_unix_sockaddr_exn remote_sockaddr in
           Uwt.Udp.send_ba ~pos:payload.Cstruct.off ~len:payload.Cstruct.len ~buf:payload.Cstruct.buffer flow.fd remote_sockaddr
           >>= fun () ->
           flow.last_use <- Unix.gettimeofday ();
           Lwt.return ()
        ) (fun e ->
            Log.err (fun f -> f "Socket.Datagram.input %s: Lwt_bytes.send caught %s" flow.description (Printexc.to_string e));
            Lwt.return ()
          )

end

module Stream = struct

  type flow = {
    description: string;
    fd: Uwt.Tcp.t;
    read_buffer_size: int;
    mutable read_buffer: Cstruct.t;
    mutable closed: bool;
  }

  type error = [
    | `Msg of string
  ]

  let error_message = function
    | `Msg x -> x

  let errorf fmt = Printf.ksprintf (fun s -> Lwt.return (`Error (`Msg s))) fmt

  let of_fd ?(read_buffer_size = 65536) ~description fd =
    let read_buffer = Cstruct.create read_buffer_size in
    let closed = false in
    { description; fd; read_buffer; read_buffer_size; closed }

  let connect_v4 ?(read_buffer_size = 65536) ip port =
    let fd = Uwt.Tcp.init () in
    let description = Ipaddr.V4.to_string ip ^ ":" ^ (string_of_int port) in
    Lwt.catch
      (fun () ->
         Log.debug (fun f -> f "Socket.TCPV4.connect_v4 %s: connecting" description);
         let sockaddr =
           let unix = Unix.ADDR_INET (Unix.inet_addr_of_string @@ Ipaddr.V4.to_string ip, port) in
           Uwt_base.Conv.of_unix_sockaddr_exn unix in
         Uwt.Tcp.connect fd ~addr:sockaddr
         >>= fun () ->
         Lwt.return (`Ok (of_fd ~read_buffer_size ~description fd))
      )
      (fun e ->
         (* FIXME(djs55): error handling *)
         let _ = Uwt.Tcp.close fd in
         errorf "Socket.TCPV4.connect_v4 %s: Lwt_unix.connect: caught %s" description (Printexc.to_string e)
      )

  let shutdown_read _ =
    Lwt.return ()

  let shutdown_write { description; fd; closed; _ } =
    try
      if not closed then Uwt.Tcp.shutdown fd else Lwt.return ()
    with
    | Unix.Unix_error(Unix.ENOTCONN, _, _) -> Lwt.return ()
    | e ->
      Log.err (fun f -> f "Socket.TCPV4.shutdown_write %s: caught %s returning Eof" description (Printexc.to_string e));
      Lwt.return ()

  let read t =
    (if Cstruct.len t.read_buffer = 0 then t.read_buffer <- Cstruct.create t.read_buffer_size);
    Lwt.catch
      (fun () ->
         Uwt.Tcp.read_ba ~pos:t.read_buffer.Cstruct.off ~len:t.read_buffer.Cstruct.len t.fd ~buf:t.read_buffer.Cstruct.buffer
         >>= function
         | 0 -> Lwt.return `Eof
         | n ->
           let results = Cstruct.sub t.read_buffer 0 n in
           t.read_buffer <- Cstruct.shift t.read_buffer n;
           Lwt.return (`Ok results)
      ) (fun e ->
          Log.err (fun f -> f "Socket.TCPV4.read %s: caught %s returning Eof" t.description (Printexc.to_string e));
          Lwt.return `Eof
        )

  let write t buf =
    Lwt.catch
      (fun () ->
         Uwt.Tcp.write_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len t.fd ~buf:buf.Cstruct.buffer
         >>= fun () ->
         Lwt.return (`Ok ())
      ) (fun e ->
          Log.err (fun f -> f "Socket.TCPV4.write %s: caught %s returning Eof" t.description (Printexc.to_string e));
          Lwt.return `Eof
        )

  let writev t bufs =
    Lwt.catch
      (fun () ->
         let rec loop = function
           | [] -> Lwt.return (`Ok ())
           | buf :: bufs ->
             Uwt.Tcp.write_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len t.fd ~buf:buf.Cstruct.buffer
             >>= fun () ->
             loop bufs in
         loop bufs
      ) (fun e ->
          Log.err (fun f -> f "Socket.TCPV4.writev %s: caught %s returning Eof" t.description (Printexc.to_string e));
          Lwt.return `Eof
        )

  let close t =
    if not t.closed then begin
      t.closed <- true;
      (* FIXME(djs55): errors *)
      let _ = Uwt.Tcp.close t.fd in
      Lwt.return ()
    end else Lwt.return ()

  (* FLOW boilerplate *)
  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t

end
