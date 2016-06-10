let src =
  let src = Logs.Src.create "usernet" ~doc:"Mirage TCP/IP <-> socket proxy" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let hvsockaddr = ref None

let set_port_forward_addr x = hvsockaddr := Some x

type port = Hostnet.Forward.Port.t

include Hostnet.Conn_lwt_unix

open Lwt.Infix

let connect port = match !hvsockaddr with
  | None ->
    Log.err (fun f -> f "Please set a Hyper-V socket address for port forwarding");
    failwith "Hyper-V socket forwarding not initialised"
  | Some _sockaddr ->
    begin match port with
    | `Tcp(_ip, _port) ->
      let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string "10.0.75.2", 8080) in
      let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd sockaddr
      >>= fun () ->
      Lwt.return (connect fd)
    | `Udp(_ip, _port) -> failwith "unimplemented"
    end 
