open Ocamlbuild_plugin
open Command

module C = Myocamlbuild_config

;;

let () =
  let with_res res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = with_res r d input_line in

  with_res (open_out "git.ml") close_out (fun out ->
   let revision =
    try
     get_line (Unix.open_process_in "git describe --always") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "git.describe") close_in with _ -> "<unknown>")
   in
   Printf.fprintf out "let revision=\"%s\"\n" (String.escaped revision)
  )

;;

dispatch begin function
| After_rules ->

     let extlib_dir = C.lib "extlib" in

     ocaml_lib ~extern:true ~dir:extlib_dir "extLib";
     ocaml_lib ~extern:true ~dir:(C.lib "deriving") "deriving";
     ocaml_lib ~extern:true ~dir:(C.lib "oUnit") "oUnit";

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A extlib_dir]);

| _ -> ()
end
