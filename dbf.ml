
open ExtLib
open Printf
open Prelude

let temp_gnuplot = ".temp.gnuplot"
let temp_csv = ".temp.csv"

let gnuplot_cmd = match Sys.os_type with
  | "Win32" -> "wgnuplot"
  | _ -> "gnuplot"

let with_open_out s = bracket (open_out s) close_out_noerr
let with_output_ch ch = bracket (IO.output_channel ch) IO.close_out

let gnuplot out name cols =
  IO.printf out "set datafile separator \",\";\n";
  IO.printf out "set data style lines;\n";
  let plot_col (col,title) =
    sprintf "'%s' using 0:%u title '%s'" name col title
  in
  IO.printf out "plot %s;\n" (String.concat "," (List.map plot_col cols))
(*   IO.printf out "pause -1;\n" *)

let split_columns s =
    String.nsplit (String.replace_chars (function ',' -> " " | x -> String.make 1 x) s) " " >>
    List.map String.strip >>
    List.filter (fun s -> String.length s > 0)

let user_select_file () =
  let l =
  Sys.readdir "." >> Array.enum >>
  Enum.filter (fun s -> String.ends_with (String.lowercase s) ".dbf") >>
  List.of_enum
  in
  List.iteri (fun i s -> printf "%u) %s\n" i s) l;
  print_string "Select file : "; flush stdout;
  let input = input_line stdin in
  let name = try let n = int_of_string input in List.nth l n with _ -> input in
  if Sys.file_exists name then Some name else
    begin printf "No such file : %s\n%!" name; None end

let dbf_to_csv dbf csv =
  match Sys.os_type with
  | "Win32" -> Sys.command (sprintf "exptizer /export /closewhendone \"%s\" \"%s\"" (String.escaped dbf) csv) >> ignore
  | _ -> Sys.command (sprintf "dbfxtrct -i\"%s\" > %s" (String.escaped dbf) csv) >> ignore

let get_columns name =  
  let cols = bracket (open_in name) close_in_noerr (fun ch ->
    let first = try input_line ch with _ -> "" in
    split_columns first >> List.take 19)
  in
  if List.length cols = 19 then
  ["Timer";"Num";"Date";"Time";"U_set";"U";"dU_dT";"I";"R";"S_set";"S";
   "dS_dT";"L";"dL_dT";"m";"dm_dT";"Vakuum";"Mode";"Info"]
  else cols

let user_select_columns name =
  let cols = get_columns name in
  if cols <> [] then
  begin
    List.iteri (fun i s -> printf "%u) %s\n" i s) cols;
    print_string "Select columns : "; flush stdout;
    let input = input_line stdin in
    let sel = split_columns input >> List.filter_map (fun s -> catch int_of_string s) in
    Some sel
  end
  else begin print_endline "No columns found"; None end

let display name cols =
  with_open_out temp_gnuplot (fun ch ->
    with_output_ch ch (fun out -> gnuplot out name cols)
  );
  Sys.command (sprintf "%s -persist %s" gnuplot_cmd temp_gnuplot) >> ignore

