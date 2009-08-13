
open ExtLib
open Printf
open Prelude

let () = Printexc.record_backtrace true

let usage_msg =
  let s1 = sprintf "dbfplot ver. %s\n" Version.id in
  let s2 = sprintf "Usage: %s <file.dbf>\n" (Filename.basename Sys.executable_name) in
  let s3 = "" in
  s1 ^ s2 ^ s3

(*
let main () =
  if 1 = Array.length Sys.argv then
    while true do
      match Dbf.user_select_file () with Some f -> process f | None -> ()
    done
  else
    let args = [] in
    Arg.parse (Arg.align args) process usage_msg
*)

let main () = Gtkui.main ()

let () = Printexc.print main ()

