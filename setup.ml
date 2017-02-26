#!/usr/bin/env ocaml

let compile_cmxa = "ocamlbuild -use-ocamlfind libwebidl.cmxa"
let compile_cma = "ocamlbuild -use-ocamlfind libwebidl.cma"
let install = "ocamlfind install webidl META _build/src/ast/ast.cmi _build/src/webidl.cmi _build/src/libwebidl.cma _build/src/libwebidl.cmxa" 
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



