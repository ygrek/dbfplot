
open ExtLib
open Printf
open Prelude

let temp_gnuplot = ".temp.gnuplot"
let temp_csv = ".temp.csv"

let with_open_out s = bracket (open_out s) close_out_noerr
let with_output_ch ch = bracket (IO.output_channel ch) IO.close_out

let gnuplot out name cols =
  IO.printf out "set datafile separator \",\";\n";
  IO.printf out "set data style lines;\n";
  let plot_col col =
    sprintf "'%s' using %u title column(%u)" name col col
  in
  IO.printf out "plot %s;\n" (String.concat "," (List.map plot_col cols));
  IO.printf out "pause -1;\n"

let process dbf =
  with_open_out temp_gnuplot (fun ch ->
    with_output_ch ch (fun out ->
      gnuplot out temp_csv [5;6;7;8;9]
  ));
  Sys.command (sprintf "dbfxtrct -i%s > %s" dbf temp_csv) >> ignore;
  Sys.command (sprintf "gnuplot %s" temp_gnuplot) >> ignore

let usage_msg = 
  let s1 = sprintf "dbfplot ver. %s\n" Git.revision in
  let s2 = sprintf "Usage: %s <file.dbf>\n" (Filename.basename Sys.executable_name) in
  let s3 = "" in
  s1 ^ s2 ^ s3

let main () =
  if 1 = Array.length Sys.argv then print_string usage_msg;
  let args = [] in
  Arg.parse (Arg.align args) process usage_msg

let () = Printexc.print main ()

