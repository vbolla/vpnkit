
let src =
  let src = Logs.Src.create "vpnkit" ~doc:"/etc/resolv.conf parse" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let nameserver_prefix = "nameserver "
let search_prefix = "search "

type resolv_conf = {
  resolvers: (Ipaddr.V4.t * int);
  search_domain: string;
}

let parse_resolve_conf txt =
  let open Astring in
  try
    let lines = String.cuts ~sep:"\n" txt in
    let r = List.fold_left
      (fun acc line ->
        let line = String.trim line in
        if line = "" then acc
        else if String.is_prefix ~affix:nameserver_prefix line then begin
          let line = String.with_range ~first:(String.length nameserver_prefix) line in
          if String.cut ~sep:"::" line <> None then begin
            (* IPv6 *)
            let host = Ipaddr.V6.of_string_exn line in
            { acc with resolvers = (Ipaddr.V6 host, 53) :: acc.resolvers }
          end else match String.cut ~sep:"#" line with
            | Some (host, port) ->
              (* IPv4 with non-standard port *)
              let host = Ipaddr.V4.of_string_exn host in
              let port = int_of_string port in
              { acc with resolvers = (Ipaddr.V4 host, port) :: acc.resolvers }
            | None ->
              (* IPv4 with standard port *)
              let host = Ipaddr.V4.of_string_exn line in
              { acc with resolvers = (Ipaddr.V4 host, 53) :: acc.resolvers }
        end else if String.is_prefix ~affix:search_prefix line then begin
          let line = String.with_range ~first:(String.length search_prefix) line in
          let all = String.cuts ~sep:" " line in
          (* They should normally be all on one line, but support one per line
             since it's more uniform *)
          { acc with search_domain = all @ acc.search_domain }
        end else begin
          Log.err (fun f -> f "skipping unexpected DNS resolv.conf line: %s" line);
          failwith ("unexpected line " ^ line)
        end
      ) { resolvers = []; search_domain = [] } lines in
    Some { r with resolvers = List.rev r.resolvers }
  with _ -> None


type t = {
  upstream: Dns_forward_config.t;
  search: string list;
}

let parse_resolvers txt =
  let resolv_conf = parse_resolv_conf txt in
  let upstream = List.map (fun (ip, port) ->
    let address = { Dns_forward_config.ip; port } in
    let zones = [] in
    { Dns_forward_config.zones; address }
  )
let to_string t =
  let lines = List.map (fun (ip, port) -> "nameserver " ^ (Ipaddr.to_string ip)
  ^ "#" ^ (string_of_int port)) t.resolvers in
  String.concat "\n" lines
