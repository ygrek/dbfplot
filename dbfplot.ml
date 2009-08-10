
open ExtLib
open Printf
open Prelude

let with_open_out s = bracket (open_out s) close_out_noerr
let with_output_ch ch = bracket (IO.output_channel ch) IO.close_out

let gnuplot out name cols =
  IO.printf out "set datafile separator \",\";\n";
  IO.printf out "set data style lines;\n";
  let plot_col col =
    sprintf "'%s' using %u title column(%u)" name col col
  in
  IO.printf out "plot %s;\n" (String.concat "," (List.map plot_col cols))

let main () =
  with_open_out "temp.gnuplot" (fun ch ->
    with_output_ch ch (fun out ->
      gnuplot out "temp.csv" [5;6;7;8;9]
  ));
  Sys.command "dbfxtrct -it.dbf > temp.csv" >> ignore;
  Sys.command "gnuplot temp.gnuplot" >> ignore

let () = Printexc.print main ()

