#!/usr/bin/env ocaml

let compile_cmxa = "ocamlbuild -use-ocamlfind libwebidl.cmxa"
let compile_cma = "ocamlbuild -use-ocamlfind libwebidl.cma"
let install = "ocamlfind install webidl META _build/src/ast/ast.cmi _build/src/webidl.cmi _build/src/libwebidl.cma _build/src/libwebidl.cmxa" 

let exec str =
  print_string ("build.ml exec:" ^ str);
  let result = Sys.command str in
  if result <> 0 then
    exit result

let () =
  exec compile_cmxa;
  exec compile_cma;
  exec install

