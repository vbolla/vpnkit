open Lwt
open Hostnet

let src =
  let src = Logs.Src.create "usernet" ~doc:"Mirage TCP/IP <-> socket proxy" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let log_exception_continue description f =
  Lwt.catch
    (fun () -> f ())
    (fun e ->
       Log.err (fun f -> f "%s: caught %s" description (Printexc.to_string e));
       Lwt.return ()
    )

let or_failwith = function
  | Result.Error (`Msg m) -> failwith m
  | Result.Ok x -> x

module Forward = Forward.Make(Connect)(Bind)

let listen path =
  let startswith prefix x =
    let prefix' = String.length prefix in
    let x' = String.length x in
    prefix' <= x' && (String.sub x 0 prefix' = prefix) in
  if startswith "fd:" path then begin
    let i = String.sub path 3 (String.length path - 3) in
    let x = try int_of_string i with _ -> failwith (Printf.sprintf "Failed to parse command-line argument [%s]" path) in
    let fd = Unix_representations.file_descr_of_int x in
    Lwt.return (Uwt.Pipe.openpipe_exn fd)
  end else begin
    ( match Unix.unlink path with
      | exception Unix.Unix_error(Unix.ENOENT, _, _) -> Lwt.return ()
      | exception e -> Lwt.fail e
      | () -> Lwt.return () )
    >>= fun () ->
    let s = Uwt.Pipe.init () in
    Uwt.Pipe.bind_exn s ~path;
    Lwt.return s
  end

let with_pipe_accept pipe f =
  Uwt.Pipe.listen_exn pipe ~max:5 ~cb:(fun server x ->
    if Uwt.Int_result.is_error x then
      ignore(Uwt_io.printl "listen error")
    else
      let client = Uwt.Pipe.init () in
      let t = Uwt.Pipe.accept_raw ~server ~client in
      if Uwt.Int_result.is_error t then begin
        ignore(Uwt_io.printl "accept error");
      end else begin
        let conn = Conn_uwt_pipe.connect client in
        f conn
      end
  )

let start_port_forwarding port_control_path vsock_path =
  Log.info (fun f -> f "starting port_forwarding port_control_path:%s vsock_path:%s" port_control_path vsock_path);
  (* Start the 9P port forwarding server *)
  Connect.vsock_path := vsock_path;
  let module Ports = Active_list.Make(Forward) in
  let module Server = Protocol_9p.Server.Make(Log)(Conn_uwt_pipe)(Ports) in
  let fs = Ports.make () in
  Socket_stack.connect ()
  >>= function
  | `Error (`Msg m) ->
    Log.err (fun f -> f "Failed to create a socket stack: %s" m);
    exit 1
  | `Ok _ ->
  Ports.set_context fs vsock_path;
  listen port_control_path
  >>= fun port_s ->
  with_pipe_accept port_s
    (fun conn ->
      Lwt.async
        (fun () ->
          log_exception_continue "Server.connect on 9P"
            (fun () ->
              Server.connect fs conn ()
              >>= function
              | Result.Error (`Msg m) ->
                Log.err (fun f -> f "failed to establish 9P connection: %s" m);
                Lwt.return ()
              | Result.Ok _ ->
                Lwt.return ()
            )
        )
    );
  Lwt.return ()

module Slirp_stack = Slirp.Make(Vmnet.Make(Conn_uwt_pipe))(Resolv_conf)

let set_nofile nofile =
  let open Sys_resource.Resource in
  let soft = Limit.Limit nofile in
  let (_, hard) = Sys_resource_unix.getrlimit NOFILE in
  Log.info (fun f -> f "Setting soft fd limit to %d" nofile);
  try Sys_resource_unix.setrlimit NOFILE ~soft ~hard with
  | Errno.Error ex -> Log.warn (fun f -> f "setrlimit failed: %s" (Errno.string_of_error ex))

let main_t socket_path port_control_path vsock_path _db_path nofile =
  Log.info (fun f -> f "Setting handler to ignore all SIGPIPE signals");
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  set_nofile nofile;

  Lwt.async_exception_hook := (fun exn ->
    Log.err (fun f -> f "Lwt.async failure %s: %s"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ())
    )
  );
  listen socket_path
  >>= fun s ->
  start_port_forwarding port_control_path vsock_path
  >>= fun () ->
  (* FIXME(djs55): database connection *)
  Log.warn (fun f -> f "no database: using hardcoded network configuration values");
  let never, _ = Lwt.task () in
  let stack = { Slirp_stack.peer_ip = Ipaddr.V4.of_string_exn "192.168.65.2";
    local_ip = Ipaddr.V4.of_string_exn "192.168.65.1";
    pcap_settings = Active_config.Value(None, never) } in
  with_pipe_accept s
    (fun conn ->
      ignore(Slirp_stack.connect stack conn)
    );
  Lwt.return ()

let main socket port_control vsock_path db nofile debug =
  Osx_reporter.install ~stdout:debug;
  Printexc.record_backtrace true;
  (* TODO(djs55): we might need to catch deferred exceptions here *)
  match Uwt.Main.run @@ main_t socket port_control vsock_path db nofile with
  | exception Uwt.Main.Deferred all ->
    List.iter
      (fun (e, b) ->
        Log.err (fun f -> f "Caught deferred exception %s; %s" (Printexc.to_string e) (Printexc.raw_backtrace_to_string b))
      ) all;
    exit 1
  | exception Uwt.Main.Fatal(e,b) ->
    Log.err (fun f -> f "Caught fatal exception %s; %s" (Printexc.to_string e) (Printexc.raw_backtrace_to_string b));
    exit 2
  | exception e ->
    Log.err (fun f -> f "Caught exception %s" (Printexc.to_string e));
    exit 3;
  | () ->
    Log.info (fun f -> f "Exiting cleanly");
    exit 0

open Cmdliner

(* NOTE(aduermael): it seems to me that "/var/tmp/com.docker.slirp.socket" is a default value, right?
This socket path is now dynamic, depending on current user's home directory. Could we just
make it fail instead? In case no argument is supplied? *)
let socket =
  Arg.(value & opt string "/var/tmp/com.docker.slirp.socket" & info [ "socket" ] ~docv:"SOCKET")

(* NOTE(aduermael): it seems to me that "/var/tmp/com.docker.port.socket" is a default value, right?
This socket path is now dynamic, depending on current user's home directory. Could we just
make it fail instead? In case no argument is supplied? *)
let port_control_path =
  Arg.(value & opt string "/var/tmp/com.docker.port.socket" & info [ "port-control" ] ~docv:"PORT")

(* NOTE(aduermael): it seems to me that "/var/tmp/com.docker.vsock/connect" is a default value, right?
This socket path is now dynamic, depending on current user's home directory. Could we just
make it fail instead? In case no argument is supplied? *)
let vsock_path =
  Arg.(value & opt string "/var/tmp/com.docker.vsock/connect" & info [ "vsock-path" ] ~docv:"VSOCK")

(* NOTE(aduermael): it seems to me that "/var/tmp/com.docker.db.socket" is a default value, right?
This socket path is now dynamic, depending on current user's home directory. Could we just
make it fail instead? In case no argument is supplied? *)
let db_path =
  Arg.(value & opt string "/var/tmp/com.docker.db.socket" & info [ "db" ] ~docv:"DB")

let nofile = Arg.(value & opt int 10240 & info [ "nofile" ] ~docv:"nofile rlimit")

let debug =
  let doc = "Verbose debug logging to stdout" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let command =
  let doc = "proxy TCP/IP connections from an ethernet link via sockets" in
  let man =
    [`S "DESCRIPTION";
     `P "Terminates TCP/IP and UDP/IP connections from a client and proxy the\
         flows via userspace sockets"]
  in
  Term.(pure main $ socket $ port_control_path $ vsock_path $ db_path $ nofile $ debug),
  Term.info "proxy" ~doc ~man

let () =
  Printexc.record_backtrace true;
  match Term.eval command with
  | `Error _ -> exit 1
  | _ -> exit 0
