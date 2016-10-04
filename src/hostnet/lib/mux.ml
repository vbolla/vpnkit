open Lwt.Infix

module DontCareAboutStats = struct
  type stats = {
    mutable rx_bytes: int64;
    mutable rx_pkts: int32;
    mutable tx_bytes: int64;
    mutable tx_pkts: int32;
  }
  let get_stats_counters _ = {
    rx_bytes = 0L; rx_pkts = 0l;
    tx_bytes = 0L; tx_pkts = 0l;
  }

  let reset_stats_counters _ = ()
end

module ObviouslyCommon = struct
  type page_aligned_buffer = Io_page.t

  type buffer = Cstruct.t

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Disconnected
  ]

  type macaddr = Macaddr.t

  type 'a io = 'a Lwt.t

  type id = unit
end

module Make(Netif: V1_LWT.NETWORK) = struct
  include DontCareAboutStats
  include ObviouslyCommon

  type rule = Ipaddr.V4.t

  type t = {
    netif: Netif.t;
  }

  let callback t buf =
    (* Does the packet match any of our rules? *)
    Lwt.return_unit

  let connect netif =
    let t = { netif } in
    Netif.listen netif (callback t)
    >>= fun () ->
    Lwt.return t

  let write _t _buffer =
    Lwt.fail (Failure "Mux.write should not be used directly")
  let writev _t _buffers =
    Lwt.fail (Failure "Mux.writev should not be used directly")
  let listen _t _callback =
    Lwt.fail (Failure "Mux.listen should not be used directly")
  let disconnect t = Lwt.return_unit

  let mac t = Netif.mac t.netif

  let port t rule =
    let module M = struct
      include DontCareAboutStats
      include ObviouslyCommon

      type t = Netif.t
      let write t buffer = Netif.write t buffer
      let writev t buffers = Netif.writev t buffers
      let listen t callback = Netif.listen t callback
      let disconnect t = Lwt.return_unit

      let mac t = failwith "unimplemented port.mac"
    end in
    Lwt.return (module M: V1_LWT.NETWORK)

end
