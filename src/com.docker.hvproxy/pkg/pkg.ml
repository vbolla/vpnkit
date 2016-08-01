#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg
open Result

let distrib = Pkg.distrib ()

let () =
  Pkg.describe ~distrib ~change_logs:[] ~metas:[] "com.docker.hvproxy" @@ fun c ->
  Ok [ Pkg.bin "src/main" ~dst:"com.docker.hvproxy" ]
