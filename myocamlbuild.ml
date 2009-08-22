open Ocamlbuild_plugin
open Command

module C = Myocamlbuild_config

;;

let () =
  let with_res res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = with_res r d input_line in

  with_res (open_out "version.ml") close_out (fun out ->
   let revision =
    try
     get_line (Unix.open_process_in "git describe --always") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "version.id") close_in with _ -> "<unknown>")
   in
   Printf.fprintf out "let id=\"%s\"\n" (String.escaped revision)
  )

;;

dispatch begin function
| After_rules ->

     C.extern "extlib" ~cma:"extLib";
     C.extern "lablgtk2" ~cma:"lablgtk";
     C.extern "mlxbase";

| _ -> ()
end
