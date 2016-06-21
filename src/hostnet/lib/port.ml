
let src =
  let src = Logs.Src.create "port forward" ~doc:"forward local ports to the VM" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let bind _local_ip _local_port _sock_stream =
  Log.err (fun f -> f "binding local ports not supported");
  failwith "binding local ports not supported"
