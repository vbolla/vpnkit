open Lwt

let src =
  let src = Logs.Src.create "usernet" ~doc:"Mirage TCP/IP <-> socket proxy" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let client_macaddr = Macaddr.of_string_exn "C0:FF:EE:C0:FF:EE"
(* random MAC from https://www.hellion.org.uk/cgi-bin/randmac.pl *)
let server_macaddr = Macaddr.of_string_exn "F6:16:36:BC:F9:C6"

let _mtu = 1452 (* packets above this size with DNF set will get ICMP errors *)

let log_exception_continue description f =
  Lwt.catch
    (fun () -> f ())
    (fun e ->
       Log.err (fun f -> f "%s: caught %s" description (Printexc.to_string e));
       Lwt.return ()
    )

module Infix = struct
  let ( >>= ) m f = m >>= function
    | `Ok x -> f x
    | `Error x -> Lwt.return (`Error x)
end

let or_failwith name m =
  m >>= function
  | `Error _ -> Lwt.fail (Failure (Printf.sprintf "Failed to connect %s device" name))
  | `Ok x -> Lwt.return x

let or_error name m =
  m >>= function
  | `Error _ -> Lwt.return (`Error (`Msg (Printf.sprintf "Failed to connect %s device" name)))
  | `Ok x -> Lwt.return (`Ok x)

let restart_on_change name to_string values =
  Active_config.tl values
  >>= fun values ->
  let v = Active_config.hd values in
  Log.info (fun f -> f "%s changed to %s in the database: restarting" name (to_string v));
  exit 1

type pcap = (string * int64 option) option

let print_pcap = function
  | None -> "disabled"
  | Some (file, None) -> "capturing to " ^ file ^ " with no limit"
  | Some (file, Some limit) -> "capturing to " ^ file ^ " but limited to " ^ (Int64.to_string limit)

type config = {
  peer_ip: Ipaddr.V4.t;
  local_ip: Ipaddr.V4.t;
  extra_dns_ip: Ipaddr.V4.t list;
  get_domain_search: unit -> string list;
  pcap_settings: pcap Active_config.values;
}

module Make(Config: Active_config.S)(Vmnet: Sig.VMNET)(Resolv_conf: Sig.RESOLV_CONF)(Host: Sig.HOST) = struct
  (* module Tcpip_stack = Tcpip_stack.Make(Vmnet)(Host.Time) *)

  module Filteredif = Filter.Make(Vmnet)
  module Netif = Capture.Make(Filteredif)
  module Switch = Mux.Make(Netif)
  module Dhcp = Dhcp.Make(Switch)

  (* This ARP implementation will respond to the VM: *)
  module Global_arp_ethif = Ethif.Make(Switch)
  module Global_arp = Arp.Make(Global_arp_ethif)

  (* This stack will attach to a switch port and represent a single remote IP *)
  module Stack_ethif = Ethif.Make(Switch.Port)
  module Stack_arpv4 = Arp.Make(Stack_ethif)
  module Stack_ipv4 = Ipv4.Make(Stack_ethif)(Stack_arpv4)
  module Stack_udp = Udp.Make(Stack_ipv4)
  module Stack_tcp = struct
    include Tcp.Flow.Make(Stack_ipv4)(Host.Time)(Clock)(Random)
    let shutdown_read _flow =
      (* No change to the TCP PCB: all this means is that I've
         got my finders in my ears and am nolonger listening to
         what you say. *)
      Lwt.return ()
    let shutdown_write = close
  end

  (* module Dns_forwarder = Dns_forward.Make(Tcpip_stack.IPV4)(Tcpip_stack.UDPV4)(Resolv_conf)(Host.Sockets)(Host.Time) *)

  type t = {
    after_disconnect: unit Lwt.t;
    interface: Netif.t;
  }

  let after_disconnect t = t.after_disconnect

  let filesystem t =
    Vfs.Dir.of_list
      (fun () ->
        Vfs.ok [
          Vfs.Inode.dir "connections" Host.Sockets.connections;
          Vfs.Inode.dir "capture" @@ Netif.filesystem t.interface;
        ]
      )

  let is_dhcp =
    let open Match in
    ethernet @@ ipv4 () @@ ((udp ~dst:67 () all) or (udp ~dst:68 () all))

  let is_arp_request =
    let open Match in
    ethernet @@ arp ~opcode:`Request ()

  let is_dns =
    let open Match in
    ethernet @@ ipv4 () @@ ((udp ~src:53 () all) or (udp ~dst:53 () all) or ((tcp ~src:53 () all) or (tcp ~dst:53 () all)))

  let is_tcp_syn =
    let open Match in
    ethernet @@ ipv4 () @@ tcp ~syn:true () all

  module Tcp = struct
    type t = {
      src: Ipaddr.V4.t;
      dst: Ipaddr.V4.t;
      src_port: int;
      dst_port: int;
      mutable socket: Host.Sockets.Stream.Tcp.flow option;
    }

    let to_string t =
      Printf.sprintf "TCP %s:%d > %s:%d"
        (Ipaddr.V4.to_string t.src) t.src_port
        (Ipaddr.V4.to_string t.dst) t.dst_port

    module Map = Map.Make(Ipaddr.V4)
    let all = ref Map.empty

    let callback t tcpv4 = match t.socket with
      | None ->
        Log.err (fun f -> f "%s callback called on closed socket" (to_string t));
        Lwt.return_unit
      | Some socket ->
        Lwt.finalize
          (fun () ->
            Mirage_flow.proxy (module Clock) (module Stack_tcp) tcpv4 (module Host.Sockets.Stream.Tcp) socket ()
            >>= function
            | `Error (`Msg m) ->
              Log.err (fun f -> f "%s proxy failed with %s" (to_string t) m);
              Lwt.return_unit
            | `Ok (_l_stats, _r_stats) ->
              Lwt.return_unit
          ) (fun () ->
            t.socket <- None;
            Host.Sockets.Stream.Tcp.close socket
            >>= fun () ->
            Lwt.return_unit
          )

    let process_syn switch arp_table buf =
      let src = failwith "src" in
      let dst = failwith "dst" in
      let src_port = failwith "src_port" in
      let dst_port = failwith "dst_port" in
      let payload = failwith "payload" in

      if Map.mem dst !all
      then Lwt.return (`Ok (Map.find dst !all))
      else begin
        let netif = Switch.port switch dst in
        let open Infix in
        or_error "Stack_ethif.connect" @@ Stack_ethif.connect netif
        >>= fun ethif ->
        or_error "Stack_arpv4.connect" @@ Stack_arpv4.connect ~table:arp_table ethif
        >>= fun arp ->
        or_error "Stack_ipv4.connect" @@ Stack_ipv4.connect ethif arp
        >>= fun ipv4 ->
        or_error "Stack_udp.connect" @@ Stack_udp.connect ipv4
        >>= fun _udp4 ->
        or_error "Stack_tcp.connect" @@ Stack_tcp.connect ipv4
        >>= fun tcp4 ->

        let open Lwt.Infix in
        Stack_ipv4.set_ip ipv4 dst (* I am the destination *)
        >>= fun () ->
        Stack_ipv4.set_ip_netmask ipv4 Ipaddr.V4.broadcast (* 255.255.255.255.*)
        >>= fun () ->
        Stack_ipv4.set_ip_gateways ipv4 [ Ipaddr.V4.unspecified ] (* 0.0.0.0 *)
        >>= fun () ->

        let open Infix in
        (* If this fails we won't register rule or [tcp] *)
        Host.Sockets.Stream.Tcp.connect (dst, dst_port)
        >>= fun socket ->
        let t = { src; dst; src_port; dst_port; socket = Some socket } in
        (* FIXME: this needs to handle more than one connection to the same IP *)
        let listeners port =
          Log.debug (fun f -> f "finding listener for port %d" port);
          Some (fun flow -> callback t flow) in
        let open Lwt.Infix in
        Stack_tcp.input tcp4 ~listeners ~src ~dst payload
        >>= fun () ->
        Lwt.return (`Ok t)
      end
  end

let connect x peer_ip local_ip extra_dns_ip get_domain_search =

  let valid_subnets = [ Ipaddr.V4.Prefix.global ] in
  let valid_sources = [ Ipaddr.V4.of_string_exn "0.0.0.0" ] in

  or_failwith "filter" @@ Filteredif.connect ~valid_subnets ~valid_sources x
  >>= fun (filteredif: Filteredif.t) ->
  or_failwith "capture" @@ Netif.connect filteredif
  >>= fun interface ->

  let kib = 1024 in
  let mib = 1024 * kib in
  (* Capture 1 MiB of all traffic *)
  Netif.add_match ~t:interface ~name:"all.pcap" ~limit:mib Match.all;
  (* Capture 256 KiB of DNS traffic *)
  Netif.add_match ~t:interface ~name:"dns.pcap" ~limit:(256 * kib) is_dns;

  Switch.connect interface
  >>= fun switch ->

  (* Serve a static ARP table *)
  let arp_table = [
    peer_ip, client_macaddr;
    local_ip, server_macaddr;
  ] @ (List.map (fun ip -> ip, server_macaddr) extra_dns_ip) in
  or_failwith "arp_ethif" @@ Global_arp_ethif.connect switch
  >>= fun global_arp_ethif ->
  or_failwith "arp" @@ Global_arp.connect ~table:arp_table global_arp_ethif
  >>= fun arp ->

  let dhcp = Dhcp.make ~client_macaddr ~server_macaddr ~peer_ip ~local_ip ~extra_dns_ip ~get_domain_search switch in

  (* Add a listener which looks for new flows *)
  Switch.listen switch
    (fun buf ->
      if Match.bufs is_dhcp [ buf ]
      then Dhcp.callback dhcp buf
      else if Match.bufs is_arp_request [ buf ] then begin
        Log.debug (fun f -> f "ARP REQUEST");
        (* Arp.input expects only the ARP packet, with no ethernet header prefix *)
        Global_arp.input arp (Cstruct.shift buf Wire_structs.sizeof_ethernet)
      end else if Match.bufs is_tcp_syn [ buf ] then begin
        Log.debug (fun f -> f "TCP SYN D3T3CT3D");
        Tcp.process_syn switch arp_table buf
        >>= function
        | `Error (`Msg m) ->
          Log.debug (fun f -> f "%s: SENDING RESET" m);
          Lwt.return_unit
        | `Ok _tcp ->
          Log.debug (fun f -> f "continuing handshake");
          Lwt.return_unit
      end else begin
        Cstruct.hexdump buf;
        Lwt.return_unit
      end
    )
  >>= fun () ->
(*
  let config = Tcpip_stack.make ~client_macaddr ~server_macaddr ~peer_ip ~local_ip ~extra_dns_ip ~get_domain_search in
        begin Tcpip_stack.connect ~config x
        >>= function
        | `Error (`Msg m) -> failwith m
        | `Ok (s, udps, capture) ->
            let ips_to_udp = List.combine extra_dns_ip udps in
            Vmnet.add_listener x (
              fun buf ->
                match (Wire_structs.parse_ethernet_frame buf) with
                | Some (Some Wire_structs.IPv4, _, payload) ->
                  let src = Ipaddr.V4.of_int32 @@ Wire_structs.Ipv4_wire.get_ipv4_src payload in
                  let dst = Ipaddr.V4.of_int32 @@ Wire_structs.Ipv4_wire.get_ipv4_dst payload in
                  begin match Wire_structs.Ipv4_wire.(int_to_protocol @@ get_ipv4_proto payload) with
                    | Some `UDP ->
                      let hlen_version = Wire_structs.Ipv4_wire.get_ipv4_hlen_version payload in
                      let ihl = hlen_version land 0xf in
                      let udp = Cstruct.shift payload (ihl * 4) in
                      let src_port = Wire_structs.get_udp_source_port udp in
                      let dst_port = Wire_structs.get_udp_dest_port udp in
                      let length = Wire_structs.get_udp_length udp in
                      let flags_fragment_offset = Wire_structs.Ipv4_wire.get_ipv4_off payload in
                      let dnf = ((flags_fragment_offset lsr 8) land 0x40) <> 0 in
                      if Cstruct.len udp < length then begin
                        Log.err (fun f -> f "Dropping UDP %s:%d -> %s:%d reported len %d actual len %d"
                                     (Ipaddr.V4.to_string src) src_port
                                     (Ipaddr.V4.to_string dst) dst_port
                                     length (Cstruct.len udp));
                        Lwt.return_unit
                      end else if dnf && (Cstruct.len payload > mtu) then begin
                        let would_fragment ~ip_header ~ip_payload =
                          let open Wire_structs.Ipv4_wire in
                          let header = Cstruct.create sizeof_icmpv4 in
                          set_icmpv4_ty header 0x03;
                          set_icmpv4_code header 0x04;
                          set_icmpv4_csum header 0x0000;
                          (* this field is unused for icmp destination unreachable *)
                          set_icmpv4_id header 0x00;
                          set_icmpv4_seq header mtu;
                          let icmp_payload = match ip_payload with
                            | Some ip_payload ->
                              if (Cstruct.len ip_payload > 8) then begin
                                let ip_payload = Cstruct.sub ip_payload 0 8 in
                                Cstruct.append ip_header ip_payload
                              end else Cstruct.append ip_header ip_payload
                            | None -> ip_header
                          in
                          set_icmpv4_csum header
                            (Tcpip_checksum.ones_complement_list [ header;
                                                                   icmp_payload ]);
                          let icmp_packet = Cstruct.append header icmp_payload in
                          icmp_packet
                        in
                        let ethernet_frame, len = Tcpip_stack.IPV4.allocate (Tcpip_stack.ipv4 s)
                          ~src:dst ~dst:src ~proto:`ICMP in
                        let ethernet_ip_hdr = Cstruct.sub ethernet_frame 0 len in

                        let reply = would_fragment
                            ~ip_header:(Cstruct.sub payload 0 (ihl * 4))
                            ~ip_payload:(Some (Cstruct.sub payload (ihl * 4) 8)) in
                        (* Rather than silently unset the do not fragment bit, we
                           respond with an ICMP error message which will
                           hopefully prompt the other side to send messages we
                           can forward *)
                        Log.err (fun f -> f
                                    "Sending icmp-dst-unreachable in response to UDP %s:%d -> %s:%d with DNF set IPv4 len %d"
                                     (Ipaddr.V4.to_string src) src_port
                                     (Ipaddr.V4.to_string dst) dst_port
                                     length);
                        Tcpip_stack.IPV4.writev (Tcpip_stack.ipv4 s) ethernet_ip_hdr [ reply ];
                      end else begin
                        let payload = Cstruct.sub udp Wire_structs.sizeof_udp (length - Wire_structs.sizeof_udp) in
                        let for_primary_ip = Ipaddr.V4.compare dst local_ip = 0 in
                        let for_extra_dns = List.fold_left (||) false (List.map (fun ip -> Ipaddr.V4.compare dst ip = 0) extra_dns_ip) in
                        let for_us = for_primary_ip || for_extra_dns in
                        if for_us && dst_port = 53 then begin
                          let primary_udp = Tcpip_stack.udpv4 s in
                          (* We need to find the corresponding `udp` value so we can send
                             data with the correct source IP, and the `nth` value so we can
                             map to the correct destination server. *)
                          let (nth, udp), _ = List.fold_left (fun ((nth, udp), i) (x, udp') ->
                            (if Ipaddr.V4.compare dst x = 0 then (i, udp') else (nth, udp)), i + 1
                          ) ((0, primary_udp), 0) ((local_ip, primary_udp) :: ips_to_udp) in
                          Dns_forwarder.input ~nth ~udp ~src ~dst ~src_port payload
                        end else if (not for_us) then begin
                          (* For any other IP, NAT as usual *)
                          Log.debug (fun f -> f "UDP %s:%d -> %s:%d len %d"
                                       (Ipaddr.V4.to_string src) src_port
                                       (Ipaddr.V4.to_string dst) dst_port
                                       length
                                   );
                          let reply buf = Tcpip_stack.UDPV4.writev ~source_ip:dst ~source_port:dst_port ~dest_ip:src ~dest_port:src_port (Tcpip_stack.udpv4 s) [ buf ] in
                          Socket.Datagram.input ~oneshot:false ~reply ~src:(Ipaddr.V4 src, src_port) ~dst:(Ipaddr.V4 dst, dst_port) ~payload ()
                        end
                        else if for_us && dst_port = 123 then begin
                          (* port 123 is special -- proxy these requests to
                             our localhost address for the local OSX ntp
                             listener to respond to *)
                          let localhost = Ipaddr.V4.localhost in
                          Log.debug (fun f -> f "UDP/123 request from port %d -- sending it to %a:%d" src_port Ipaddr.V4.pp_hum localhost dst_port);
                          let reply buf = Tcpip_stack.UDPV4.writev ~source_ip:local_ip ~source_port:dst_port ~dest_ip:src ~dest_port:src_port (Tcpip_stack.udpv4 s) [ buf ] in
                          Socket.Datagram.input ~oneshot:false ~reply ~src:(Ipaddr.V4 src, src_port) ~dst:(Ipaddr.V4 localhost, dst_port) ~payload ()
                        end else Lwt.return_unit
                      end
                    | _ -> Lwt.return_unit
                  end
                | _ -> Lwt.return_unit
            );
            Tcpip_stack.listen_tcpv4_flow s ~on_flow_arrival:(
              fun ~src:(src_ip, src_port) ~dst:(dst_ip, dst_port) ->
                let for_us dst_ip = Ipaddr.V4.compare dst_ip local_ip = 0 in
                let for_extra_dns = List.fold_left (||) false (List.map (fun ip -> Ipaddr.V4.compare dst_ip ip = 0) extra_dns_ip) in
                let for_dns dst_ip = for_us dst_ip || for_extra_dns in
                ( if for_dns dst_ip && dst_port = 53 then begin
                    Resolv_conf.get ()
                    >>= fun all ->
                    let nth, _ = List.fold_left (fun (nth, i) x ->
                      (if Ipaddr.V4.compare dst_ip x = 0 then i else nth), i + 1
                    ) (0, 0) (local_ip :: extra_dns_ip) in
                    match Dns_forward.choose_server ~nth all.Resolver.resolvers with
                    | Some (description, (Ipaddr.V4 ip, port)) ->
                      Lwt.return (":" ^ description, ip, port)
                    | _ ->
                      Log.err (fun f -> f "Failed to discover DNS server: assuming 127.0.01");
                      Lwt.return (":no DNS server", Ipaddr.V4.of_string_exn "127.0.0.1", 53)
                  end else Lwt.return ("", dst_ip, dst_port)
                ) >>= fun (description, dst_ip, dst_port) ->
                (* If the traffic is for us, use a local IP address that is really
                   ours, rather than send traffic off to someone else (!) *)
                let dst_ip = if for_us dst_ip then Ipaddr.V4.localhost else dst_ip in
                Socket.Stream.Tcp.connect (dst_ip, dst_port)
                >>= function
                | `Error (`Msg _) ->
                  return `Reject
                | `Ok remote ->
                  Lwt.return (`Accept (fun local ->
                      finally (fun () ->
                          (* proxy between local and remote *)
                          Mirage_flow.proxy (module Clock) (module Tcpip_stack.TCPV4_half_close) local (module Socket.Stream.Tcp) remote ()
                          >>= function
                          | `Error (`Msg m) ->
                            Log.err (fun f ->
                              let description =
                                Printf.sprintf "TCP%s %s:%d > %s:%d" description
                                  (Ipaddr.V4.to_string src_ip) src_port
                                  (Ipaddr.V4.to_string dst_ip) dst_port in
                               f "%s proxy failed with %s" description m);
                            return ()
                          | `Ok (_l_stats, _r_stats) ->
                            return ()
                        ) (fun () ->
                          Socket.Stream.Tcp.close remote
                          >>= fun () ->
                          Lwt.return ()
                        )
                    ))
            );
            Tcpip_stack.listen s
            >>= fun () ->
            *)
            Log.info (fun f -> f "TCP/IP ready");
            Lwt.return {
              after_disconnect = Vmnet.after_disconnect x;
              interface
            }

  let create config =
    let driver = [ "com.docker.driver.amd64-linux" ] in

    let pcap_path = driver @ [ "slirp"; "capture" ] in
    Config.string_option config pcap_path
    >>= fun string_pcap_settings ->
    let parse_pcap = function
      | None -> Lwt.return None
      | Some x ->
        begin match Stringext.split (String.trim x) ~on:':' with
        | [ filename ] ->
          (* Assume 10MiB limit for safety *)
          Lwt.return (Some (filename, Some 16777216L))
        | [ filename; limit ] ->
          let limit =
            try
              Int64.of_string limit
            with
            | _ -> 16777216L in
          let limit = if limit = 0L then None else Some limit in
          Lwt.return (Some (filename, limit))
        | _ ->
          Lwt.return None
        end in
    Active_config.map parse_pcap string_pcap_settings
    >>= fun pcap_settings ->

    let max_connections_path = driver @ [ "slirp"; "max-connections" ] in
    Config.string_option config max_connections_path
    >>= fun string_max_connections ->
    let parse_max = function
      | None -> Lwt.return None
      | Some x -> Lwt.return (
        try Some (int_of_string @@ String.trim x)
        with _ ->
          Log.err (fun f -> f "Failed to parse slirp/max-connections value: '%s'" x);
          None
      ) in
    Active_config.map parse_max string_max_connections
    >>= fun max_connections ->
    let rec monitor_max_connections_settings settings =
      begin match Active_config.hd settings with
      | None ->
        Log.info (fun f -> f "remove connection limit");
        Host.Sockets.set_max_connections None
      | Some limit ->
        Log.info (fun f -> f "updating connection limit to %d" limit);
        Host.Sockets.set_max_connections (Some limit)
      end;
      Active_config.tl settings
      >>= fun settings ->
      monitor_max_connections_settings settings in
    Lwt.async (fun () -> log_exception_continue "monitor max connections settings" (fun () -> monitor_max_connections_settings max_connections));

    (* Watch for DNS server overrides *)
    let domain_search = ref [] in
    let get_domain_search () = !domain_search in
    let dns_path = driver @ [ "slirp"; "dns" ] in
    Config.string_option config dns_path
    >>= fun string_dns_settings ->
    Active_config.map
      (function
        | Some txt ->
          let dns = Resolver.parse_resolvers txt in
          begin match dns with
          | Some { Resolver.search; _ } ->
            domain_search := search;
            Log.info (fun f -> f "updating search domains to %s" (String.concat " " !domain_search))
          | _ -> ()
          end;
          Lwt.return dns
        | None ->
          Lwt.return None
      ) string_dns_settings
    >>= fun dns_settings ->

    let rec monitor_dns_settings settings =
      begin match Active_config.hd settings with
      | None ->
        Log.info (fun f -> f "remove resolver override");
        Resolv_conf.set { Resolver.resolvers = []; search = [] }
      | Some r ->
        Log.info (fun f -> f "updating resolvers to %s" (Resolver.to_string r));
        Resolv_conf.set r
      end;
      Active_config.tl settings
      >>= fun settings ->
      monitor_dns_settings settings in
    Lwt.async (fun () -> log_exception_continue "monitor DNS settings" (fun () -> monitor_dns_settings dns_settings));

    let bind_path = driver @ [ "allowed-bind-address" ] in
    Config.string_option config bind_path
    >>= fun string_allowed_bind_address ->
    let parse_bind_address = function
      | None -> Lwt.return None
      | Some x ->
        let strings = List.map String.trim @@ Stringext.split x ~on:',' in
        let ip_opts = List.map
          (fun x ->
            try
              Some (Ipaddr.of_string_exn x)
            with _ ->
              Log.err (fun f -> f "Failed to parse IP address in allowed-bind-address: %s" x);
              None
          ) strings in
        let ips = List.fold_left (fun acc x -> match x with None -> acc | Some x -> x :: acc) [] ip_opts in
        Lwt.return (Some ips) in
    Active_config.map parse_bind_address string_allowed_bind_address
    >>= fun allowed_bind_address ->

    let rec monitor_allowed_bind_settings allowed_bind_address =
      Forward.set_allowed_addresses (Active_config.hd allowed_bind_address);
      Active_config.tl allowed_bind_address
      >>= fun allowed_bind_address ->
      monitor_allowed_bind_settings allowed_bind_address in
    Lwt.async (fun () -> log_exception_continue "monitor_allowed_bind_settings" (fun () -> monitor_allowed_bind_settings allowed_bind_address));

    let peer_ips_path = driver @ [ "slirp"; "docker" ] in
    let parse_ipv4 default x = match Ipaddr.V4.of_string @@ String.trim x with
      | None ->
        Log.err (fun f -> f "Failed to parse IPv4 address '%s', using default of %s" x (Ipaddr.V4.to_string default));
        Lwt.return default
      | Some x -> Lwt.return x in
    let parse_ipv4_list default x =
      let all = List.map (fun x -> Ipaddr.V4.of_string @@ String.trim x) @@ Astring.String.cuts ~sep:"," x in
      let any_none, some = List.fold_left (fun (any_none, some) x -> match x with
        | None -> true, some
        | Some x -> any_none, x :: some
      ) (false, []) all in
      if any_none then begin
        Log.err (fun f -> f "Failed to parse IPv4 address list '%s', using default of %s" x (String.concat "," (List.map Ipaddr.V4.to_string default)));
        Lwt.return default
      end else Lwt.return some in

    let default_peer = "192.168.65.2" in
    let default_host = "192.168.65.1" in
    let default_dns_extra = [
      "192.168.65.3"; "192.168.65.4"; "192.168.65.5"; "192.168.65.6";
      "192.168.65.7"; "192.168.65.8"; "192.168.65.9"; "192.168.65.10";
    ] in
    Config.string config ~default:default_peer peer_ips_path
    >>= fun string_peer_ips ->
    Active_config.map (parse_ipv4 (Ipaddr.V4.of_string_exn default_peer)) string_peer_ips
    >>= fun peer_ips ->
    Lwt.async (fun () -> restart_on_change "slirp/docker" Ipaddr.V4.to_string peer_ips);

    let host_ips_path = driver @ [ "slirp"; "host" ] in
    Config.string config ~default:default_host host_ips_path
    >>= fun string_host_ips ->
    Active_config.map (parse_ipv4 (Ipaddr.V4.of_string_exn default_host)) string_host_ips
    >>= fun host_ips ->
    Lwt.async (fun () -> restart_on_change "slirp/host" Ipaddr.V4.to_string host_ips);

    let extra_dns_ips_path = driver @ [ "slirp"; "extra_dns" ] in
    Config.string config ~default:(String.concat "," default_dns_extra) extra_dns_ips_path
    >>= fun string_extra_dns_ips ->
    Active_config.map (parse_ipv4_list (List.map Ipaddr.V4.of_string_exn default_dns_extra)) string_extra_dns_ips
    >>= fun extra_dns_ips ->
    Lwt.async (fun () -> restart_on_change "slirp/extra_dns" (fun x -> String.concat "," (List.map Ipaddr.V4.to_string x)) extra_dns_ips);

    let peer_ip = Active_config.hd peer_ips in
    let local_ip = Active_config.hd host_ips in
    let extra_dns_ip = Active_config.hd extra_dns_ips in

    Log.info (fun f -> f "Creating slirp server pcap_settings:%s peer_ip:%s local_ip:%s domain_search:%s"
      (print_pcap @@ Active_config.hd pcap_settings) (Ipaddr.V4.to_string peer_ip) (Ipaddr.V4.to_string local_ip) (String.concat " " !domain_search)
    );
    let t = {
      peer_ip;
      local_ip;
      extra_dns_ip;
      get_domain_search;
      pcap_settings;
    } in
    Lwt.return t

  let connect t client =
      Vmnet.of_fd ~client_macaddr ~server_macaddr client
      >>= function
      | `Error (`Msg m) -> failwith m
      | `Ok x ->
        Log.debug (fun f -> f "accepted vmnet connection");

        let rec monitor_pcap_settings pcap_settings =
          ( match Active_config.hd pcap_settings with
            | None ->
              Log.debug (fun f -> f "Disabling any active packet capture");
              Vmnet.stop_capture x
            | Some (filename, size_limit) ->
              Log.debug (fun f -> f "Capturing packets to %s %s" filename (match size_limit with None -> "with no limit" | Some x -> Printf.sprintf "limited to %Ld bytes" x));
              Vmnet.start_capture x ?size_limit filename )
          >>= fun () ->
          Active_config.tl pcap_settings
          >>= fun pcap_settings ->
          monitor_pcap_settings pcap_settings in
        Lwt.async (fun () ->
          log_exception_continue "monitor_pcap_settings"
            (fun () ->
              monitor_pcap_settings t.pcap_settings
            )
          );
        connect x t.peer_ip t.local_ip t.extra_dns_ip t.get_domain_search
end
