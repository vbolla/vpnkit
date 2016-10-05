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
      Log.debug (fun f -> f "using default callback for non-IPv4 frame");
      t.default_callback buf

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

  module Port = struct
    include DontCareAboutStats
    include ObviouslyCommon

    type port = {
      switch: t;
      netif: Netif.t;
      rule: rule;
    }

    let write t buffer = Netif.write t.netif buffer
    let writev t buffers = Netif.writev t.netif buffers
    let listen t callback =
      t.switch.rules <- RuleMap.add t.rule callback t.switch.rules;
      Lwt.return_unit
    let disconnect t =
      t.switch.rules <- RuleMap.remove t.rule t.switch.rules;
      Lwt.return_unit

    let mac t = Netif.mac t.netif

    type t = port
  end

  let port t rule = { Port.switch = t; netif = t.netif; rule }

end
