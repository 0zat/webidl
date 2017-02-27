#!/usr/bin/env ocaml

let compile_cmxa = "ocamlbuild -use-ocamlfind webidl.cmx"
let compile_cma = "ocamlbuild -use-ocamlfind webidl.cmo"
let install = "ocamlfind install webidl META _build/src/lib/webidl.cmi _build/src/lib/webidl.cmo _build/src/lib/webidl.cmx" 
let remove = "ocamlfind remove webidl"

let exec str =
  print_string ("setup.ml exec:" ^ str ^ "\n");
  let result = Sys.command str in
  if result <> 0 then
    exit result

let () =
  match Sys.argv.(1) with
  | "build" -> 
    exec compile_cmxa;
    exec compile_cma
  | "install" -> exec install
  | "remove" -> exec remove
  | _ -> (print_string "unkown option"; exit 1)



