open Lwt
open Astring

let src =
  let src = Logs.Src.create "HV" ~doc:"proxy HV sockets" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let log_exception_continue description f =
  Lwt.catch
    (fun () -> f ())
    (fun e ->
       Log.err (fun f -> f "%s: caught %s" description (Printexc.to_string e));
       Lwt.return ()
    )

let default d = function None -> d | Some x -> x

let ethernet_serviceid = "30D48B34-7D27-4B0B-AAAF-BBBED334DD59"
let ports_serviceid = "0B95756A-9985-48AD-9470-78E060895BE7"

module HV = Flow_lwt_unix_hvsock

let rec hvsock_connect vmid serviceid =
  let sockaddr = { Hvsock.vmid; serviceid } in
  let socket = HV.Hvsock.create () in
  Lwt.catch
    (fun () ->
      HV.Hvsock.connect socket sockaddr
      >>= fun () ->
      Lwt.return socket
    ) (function
      | Unix.Unix_error(_, _, _) ->
        HV.Hvsock.close socket
        >>= fun () ->
        Lwt_unix.sleep 1.
        >>= fun () ->
        hvsock_connect vmid serviceid
      | _ ->
        HV.Hvsock.close socket
        >>= fun () ->
        Lwt_unix.sleep 1.
        >>= fun () ->
        hvsock_connect vmid serviceid
    )

let rec named_pipe_accept_forever path callback =
  let open Lwt.Infix in
  let p = Named_pipe_lwt.Server.create path in
  Named_pipe_lwt.Server.connect p >>= function
  | false ->
    Log.err (fun f -> f "Nmaed-pipe connection failed on %s" path);
    Lwt.return ()
  | true ->
    let _ = (* background thread *)
      let fd = Named_pipe_lwt.Server.to_fd p in
      callback fd
    in
    named_pipe_accept_forever path callback

let proxy named_pipe =
  named_pipe_accept_forever named_pipe
    (fun fd ->
      let module LocalChannel = Channel.Make(Flow_lwt_unix) in
      let local = LocalChannel.create (Flow_lwt_unix.connect fd) in
      LocalChannel.write_line local "Enter address to connect using 'VMuuid-SERVICEuuid\\n'";
      LocalChannel.flush local
      >>= fun () ->
      LocalChannel.read_line local
      >>= fun bufs ->
      let message = String.trim (String.concat ~sep:"" (List.map Cstruct.to_string bufs)) in
      Log.info (fun f -> f "Read [%s]" message);
      match String.cut ~sep:"." message with
      | Some (vmid, serviceid) ->
        hvsock_connect (Hvsock.Id vmid) serviceid
        >>= fun hv ->
        Lwt.finalize
          (fun () ->
            let module RemoteChannel = Channel.Make(HV) in
            let remote = RemoteChannel.create (HV.connect hv) in
            let local2remote =
              let rec loop () =
                LocalChannel.read_some local
                >>= fun buf ->
                if Cstruct.len buf = 0
                then Lwt.return_unit
                else begin
                  RemoteChannel.write_buffer remote buf;
                  RemoteChannel.flush remote
                  >>= fun () ->
                  loop ()
                end in
              loop () in
            let remote2local =
              let rec loop () =
                RemoteChannel.read_some remote
                >>= fun buf ->
                if Cstruct.len buf = 0
                then Lwt.return_unit
                else begin
                  LocalChannel.write_buffer local buf;
                  LocalChannel.flush local
                  >>= fun () ->
                  loop ()
                end in
              loop () in
            Lwt.join [ local2remote; remote2local ]
          ) (fun () ->
            HV.Hvsock.close hv
            >>= fun () ->
            Lwt_unix.close fd
          )
      | None ->
        Log.err (fun f -> f "Failed to parse request, expected VMID.SERVICEID");
        Lwt.return_unit
    ) 

let main_t ethernet_pipe_name port_pipe_name =
  Logs.set_reporter (Logs_fmt.reporter ());
  Printexc.record_backtrace true;

  Lwt.async_exception_hook := (fun exn ->
    Log.err (fun f -> f "Lwt.async failure %s: %s"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ())
    )
  );

  Lwt.async (fun () ->
    log_exception_continue "ethernet proxy"
      (fun () ->
        proxy ethernet_pipe_name
      )
  );
  log_exception_continue "port proxy"
    (fun () ->
      proxy port_pipe_name 
    )

open Cmdliner

let ethernet =
  let doc =
    Arg.info ~doc:
      "The address on the host for the VM ethernet connection."
    [ "ethernet" ]
  in
  Arg.(value & opt string "\\\\.\\pipe\\dockerethernet" doc)

let port =
  let doc =
    Arg.info ~doc:
      "The address on the host for the VM port forwarding."
    [ "port" ]
  in
  Arg.(value & opt string "\\\\.\\pipe\\dockerport" doc)

let command =
  let doc = "proxy Named pipe connections over Hyper-V sockets" in
  let man =
    [`S "DESCRIPTION";
     `P "Accepts named pipe connections forever and forwards to the VM"]
  in
  Term.(pure main_t $ ethernet $ port),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"None" ~doc ~man

let () =
  Printexc.record_backtrace true;
  match Term.eval command with
  | `Error _ -> exit 1
  | _ -> exit 0
