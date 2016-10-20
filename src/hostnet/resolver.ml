
let src =
  let src = Logs.Src.create "vpnkit" ~doc:"/etc/resolv.conf parse" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  upstream: Dns_forward_config.t;
  search: string list;
}

let nameserver_prefix = "nameserver "
let search_prefix = "search "
let zone_prefix = "zone "

let of_string txt =
  let open Astring in
  let open Dns_forward_config in
  try
    (* Chop into lines *)
    String.cuts ~sep:"\n" txt
    |> List.map String.trim
    |> List.filter (fun x -> x <> "") in
    |> List.fold_left
      (fun acc line ->
        if String.is_prefix ~affix:nameserver_prefix line then begin
        let line = String.with_range ~first:(String.length nameserver_prefix) line in
        if String.cut ~sep:"::" line <> None then begin
          (* IPv6 *)
          let host = Ipaddr.V6.of_string_exn line in
          `Nameserver (Ipaddr.V6 host, 53) :: acc
        end else match String.cut ~sep:"#" line with
          | Some (host, port) ->
            (* IPv4 with non-standard port *)
            let host = Ipaddr.V4.of_string_exn host in
            let port = int_of_string port in
            `Nameserver (Ipaddr.V4 host, port) :: acc
          | None ->
            (* IPv4 with standard port *)
            let host = Ipaddr.V4.of_string_exn line in
            `Nameserver (Ipaddr.V4 host, 53)
        else if String.is_prefix ~affix:zones_prefix line then begin
          let line = String.with_range ~first:(String.length zone_prefix) line in
          `Zones (String.cuts ~sep:" " line) :: acc
        end else if String.is_prefix ~affix:search_prefix line then begin
          let line = String.with_range ~first:(String.length search_prefix) line in
          `Search (String.cuts ~sep:" " line) :: acc
        end else acc
      ) []
    (* Merge the zones and nameservers together *)
    |> List.fold_left
      (fun (zones_opt, acc) line -> match zones_opt, line with
        | _, `Zones zones -> (Some zones, acc)
        | Some zones, `Nameserver (ip, port) ->
          let zones = List.map (String.cuts ~sep:".") zones in
          let server = { address = { ip; port }; zones } in
          None, { acc with upstream = server :: acc.upstream }
        | _, `Search search ->
          zones_opt, { acc with search }
      ) (None, { upstream = []; search = [] })
    |> List.map (fun x -> Some x)
  with _ -> None

let to_string t =
  let nameservers = List.map
    (fun server ->
      let open Dns_forward_config in
      [ nameserver_prefix ^ (Ipaddr.to_string server.address.ip) ^ "#" ^ (string_of_int server.address.port) ]
      @ (if zones <> [] then [ zones_prefix ^ (String.concat " " server.zones) ] else [])
    ) t.upstream |> List.concat in
  let search = List.map
    (fun search ->
      search_prefix ^ search
    ) t.search in
  String.concat "\n" (nameservers @ search)

let of_resolv_conf txt =
  let open Dns.Resolvconf in
  let lines = Astring.String.cuts ~sep:"\n" txt in
  let config = List.rev @@ List.fold_left (fun acc x ->
      match map_line x with
      | None -> acc
      | Some x ->
        begin
          try
            KeywordValue.of_string x :: acc
          with
          | _ -> acc
        end
    ) [] lines in
  let ipv4_servers = List.fold_left (fun acc x -> match x with
    (* Remove any IPv6 servers *)
    | KeywordValue.Nameserver(Ipaddr.V4 ipv4, port) -> (ipv4, port) :: acc
    | _ -> acc
  ) [] config |> List.rev in
  let search = List.fold_left (fun acc x -> match x with
    | KeywordValue.Search names -> names @ acc
    | _ -> acc
  ) [] config |> List.rev in
  let upstream = List.map (fun (ip, port) -> { Config.address = { Config.ip; port }; zones = [] }) ipv4_servers in
  Lwt.return ({ upstream; search })
