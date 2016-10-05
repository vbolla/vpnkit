open Lwt.Infix

let src =
  let src = Logs.Src.create "mux" ~doc:"Mirage TCP/IP <-> socket proxy" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

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

  module RuleMap = Map.Make(Ipaddr.V4)

  type callback = Cstruct.t -> unit Lwt.t

  type t = {
    netif: Netif.t;
    mutable rules: callback RuleMap.t;
    mutable default_callback: callback;
  }

  let callback t buf =
    (* Does the packet match any of our rules? *)
    match Wire_structs.parse_ethernet_frame buf with
    | Some (Some Wire_structs.IPv4, _, payload) ->
      let dst = Wire_structs.Ipv4_wire.get_ipv4_dst payload |> Ipaddr.V4.of_int32 in
      if RuleMap.mem dst t.rules
      then RuleMap.find dst t.rules buf
      else begin
        Log.debug (fun f -> f "using default callback for packet for %s" (Ipaddr.V4.to_string dst));
        t.default_callback buf
      end
    | _ ->
      Log.debug (fun f -> f "dropping non-IPv4 frame");
      Lwt.return_unit

  let connect netif =
    let rules = RuleMap.empty in
    let default_callback = fun _ -> Lwt.return_unit in
    let t = { netif; rules; default_callback } in
    Netif.listen netif (callback t)
    >>= fun () ->
    Lwt.return t

  let write t buffer =
    Netif.write t.netif buffer
  let writev t buffers =
    Netif.writev t.netif buffers
  let listen t callback =
    t.default_callback <- callback;
    Lwt.return_unit
  let disconnect t =
    Netif.disconnect t.netif

  let mac t = Netif.mac t.netif

  let port t rule =
    let module M = struct
      include DontCareAboutStats
      include ObviouslyCommon

      type t = unit
      let callback = ref (fun _ -> Lwt.return_unit)

      let write () buffer = Netif.write t.netif buffer
      let writev () buffers = Netif.writev t.netif buffers
      let listen () callback =
        t.rules <- RuleMap.add rule callback t.rules;
        Lwt.return_unit
      let disconnect () =
        t.rules <- RuleMap.remove rule t.rules;
        Lwt.return_unit

      let mac () = Netif.mac t.netif
    end in
    Lwt.return (module M: V1_LWT.NETWORK)

end
