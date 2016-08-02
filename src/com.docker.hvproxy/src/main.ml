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

module Host = Host_uwt

module HV = Flow_lwt_hvsock.Make(Host.Time)(Host.Main)

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
        Host.Time.sleep 1.
        >>= fun () ->
        hvsock_connect vmid serviceid
      | _ ->
        HV.Hvsock.close socket
        >>= fun () ->
        Host.Time.sleep 1.
        >>= fun () ->
        hvsock_connect vmid serviceid
    )

let proxy named_pipe =
  Host.Sockets.Stream.Unix.bind named_pipe
  >>= fun server ->
  Host.Sockets.Stream.Unix.listen server
    (fun fd ->
      let module LocalChannel = Channel.Make(Host.Sockets.Stream.Unix) in
      let local = LocalChannel.create fd in
(*
      LocalChannel.write_line local "Enter address to connect using 'VMuuid-SERVICEuuid\\n'";
      LocalChannel.flush local
      >>= fun () ->
*)
      LocalChannel.read_line local
      >>= fun bufs ->
      let message = String.trim (String.concat ~sep:"" (List.map Cstruct.to_string bufs)) in
      Log.info (fun f -> f "Read [%s]" message);
      match String.cut ~sep:"." message with
      | Some (vmid, serviceid) ->
        hvsock_connect (Hvsock.Id vmid) serviceid
        >>= fun hv ->
        Log.info (fun f -> f "Connected to %s %s" vmid serviceid);
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
            Host.Sockets.Stream.Unix.close fd
          )
      | None ->
        Log.err (fun f -> f "Failed to parse request, expected VMID.SERVICEID");
        Lwt.return_unit
    );
  Lwt.return_unit

let main_t pipe_name =
  Logs.set_reporter (Logs_fmt.reporter ());
  Printexc.record_backtrace true;

  Lwt.async_exception_hook := (fun exn ->
    Log.err (fun f -> f "Lwt.async failure %s: %s"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ())
    )
  );

  let rec loop () =
    Log.info (fun f -> f ".");
    Host.Time.sleep 1.
    >>= fun () ->
    loop () in
  let _ = loop () in

  proxy pipe_name
  >>= fun () ->
  let forever, _ = Lwt.task () in
  forever

let main pipe_name =
  Host.Main.run (main_t pipe_name)

open Cmdliner

let pipe =
  let doc =
    Arg.info ~doc:
      "The address on the host for HV socket forwards."
    [ "pipe" ]
  in
  Arg.(value & opt string "\\\\.\\pipe\\dockerhvproxy" doc)

let command =
  let doc = "proxy Named pipe connections over Hyper-V sockets" in
  let man =
    [`S "DESCRIPTION";
     `P "Accepts named pipe connections forever and forwards to the VM"]
  in
  Term.(pure main $ pipe),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"None" ~doc ~man

let () =
  Printexc.record_backtrace true;
  match Term.eval command with
  | `Error _ -> exit 1
  | _ -> exit 0
