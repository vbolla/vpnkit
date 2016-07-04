let src =
  let src = Logs.Src.create "port forward" ~doc:"forward local ports to the VM" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Infix

let nslookup_exe = "C:\\Windows\\system32\\nslookup.exe"

let upstream_dns = ref (Ipaddr.V4.of_string_exn "127.0.0.1")

let set_dns dns =
  Log.info (fun f -> f "using DNS forwarder on %s:53" dns);
  upstream_dns := (Ipaddr.V4.of_string_exn dns)

let parse_nslookup_output stdout =
  let open Astring in
  begin match String.cuts ~sep:"\n" stdout with
  | _ :: addressLine :: _ ->
    let _, nameServer = String.cut ~sep:":" addressLine in
    if String.cut ~sep:"::" addressLine <> None then begin
      (* IPv6 *)
      let host = Ipaddr.V6.of_string_exn nameServer in
      Some (Ipaddr.V6 host, 53)
    end else match String.cut ~sep:"#" addressLine with
      | Some (host, port) ->
        (* IPv4 with non-standard port *)
        let host = Ipaddr.V4.of_string_exn host in
        let port = int_of_string port in
        Some (Ipaddr.V4 host, port)
      | None ->
        (* IPv4 with standard port *)
        let host = Ipaddr.V4.of_string_exn host in
        Some (Ipaddr.V4 host, 53)
  end

module Make(Process: Sig.PROCESSES) = struct

  type `a cache = {
    mutable item: `a option;
    mutable fetch_time: float;
    m: Lwt_mutex.t;
    fetch: unit -> `a option Lwt.t;
    timeout: float;
  }

  let get cache =
    Lwt_mutex.with_lock cache.m
      (fun () ->
        let now = Unix.gettimeofday () in
        if cache.item = None || (now -. cache.fetch_time > cache.timeout) then begin
          fetch ()
          >>= function
          | Some result ->
            cache.item <- result;
            cache.fetch_time <- Unix.gettimeofday ();
            Lwt.return (Some result)
          | None ->
            Lwt.return None
        end
      )

  let make ~fetch ~timeout () =
    let item = None in
    let fetch_time = 0. in
    let m = Lwt_mutex.create () in
    { item; fetch_time; m; fetch; timeout }


  let nslookup_localhost () =
    Host.Process.spawn nslookup_exe
    >>= function
    | `Error (`Msg m) ->
      Log.err (fun f -> f "Failed to run %s: %s" nslookup_exe m);
      Lwt.return None
    | `Ok stdout ->
      begin match parse_nslookup_output stdout with
      | None ->
        Log.err (fun f -> f "Failed to parse nslookup output: [%s]" stdout);
        Lwt.return None
      | Some x -> Lwt.return x
      end

  let nameserver_cache = make ~fetch:nslookup_localhost ~timeout:5.0 ()

  let get () =
    get nameserver_cache
    >>= function
    | None ->
      Log.err (fun f -> f "Falling back to DNS on %s" (Ipaddr.V4.to_string !upstream_dns));
      Lwt.return [ Ipaddr.V4 !upstream_dns, 53 ]
    | Some (ip, port) ->
      Lwt.return [ ip, port ] (* FIXME: needs to be a list *)
end
