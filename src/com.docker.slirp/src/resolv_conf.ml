open Lwt

let get () =
  (* FIXME(djs55): need a Uwt-friendly way to read /etc/resolv.conf *)
  return [ Ipaddr.V4 (Ipaddr.V4.of_string_exn "8.8.8.8"), 53 ]
