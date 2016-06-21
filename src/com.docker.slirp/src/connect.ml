let src =
  let src = Logs.Src.create "port forward" ~doc:"forward local ports to the VM" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let (/) = Filename.concat
let home = try Sys.getenv "HOME" with Not_found -> "/Users/root"
let vsock_path = ref (home / "Library/Containers/com.docker.docker/Data/@connect")
let _vsock_port = 62373l

include Conn_uwt_pipe

type port = Hostnet.Forward.Port.t

let connect _port =
  (* FIXME(djs55): need a Uwt-friendly API to connect to hyperkit *)
  Log.err (fun f -> f "port forwarding isn't going to work");
  failwith "port forwarding isn't going to work"
