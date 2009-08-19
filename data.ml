
open ExtLib
open Printf
open Prelude

let temp_gnuplot = ".temp.gnuplot"
let temp_csv = ".temp.csv"
let win = Sys.os_type = "Win32"
let gnuplot_cmd = if win then "wgnuplot" else "gnuplot"

let with_open_out s = bracket (open_out s) close_out_noerr
let with_open_in s = bracket (open_in s) close_in_noerr
let with_output_ch ch = bracket (IO.output_channel ch) IO.close_out

let gnuplot out name cols single =
  let pr fmt = IO.printf out (fmt ^^ ";\n")in
  pr "set datafile separator \",\"";
  pr "set data style lines";
  pr "set lmargin %u" (if win then 5 else 10);
  pr "set tmargin %f" (if win then 0.5 else 1.);
  pr "set bmargin 0";
  pr "set rmargin 0";
  pr "set xtics in offset 0, 2";
  if single then
    pr "plot %s" (List.map (fun (n,t) -> sprintf "'%s' using 0:%u title '%s'" name (n+1) t) cols >> String.concat ",")
  else
  begin
  pr "set multiplot layout %u,1" (List.length cols);
  let plot_col (n,title) =
    pr "plot '%s' using 0:%u title '%s'" name (n+1) title
  in
  List.iter plot_col cols;
  pr "unset multiplot"
  end
(*   pr "plot %s" (String.concat "," (List.map plot_col cols)) *)
(*   IO.printf out "pause -1;\n" *)

let split_columns s =
    String.nsplit (String.replace_chars (function ',' -> " " | x -> String.make 1 x) s) " " >>
    List.map String.strip >>
    List.filter (fun s -> String.length s > 0)

let split_columns s =
  String.nsplit s "," >> List.map String.strip

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

(*
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
*)

module Csv = 
struct

type ('a,'b) t = ('a array * 'b) array (** rectangular, by columns *)

let input ch =
  let a = Std.input_lines ch >> Enum.map (Array.of_list & split_columns) >> 
  Enum.fold (fun a acc -> match acc with
      | [] -> [a]
      | l -> if Array.length (List.hd l) = Array.length a then a::l else l) [] >>
  Array.of_list in
  Array.rev_in_place a;
  (* transpose *)
  let n = Array.length a in
  if n = 0 then [||] else
  Array.init (Array.length a.(0)) (fun i -> Array.init n (fun j -> a.(j).(i)), "")

let convert f m =
  m >> Array.to_list >> List.filter_map (fun (a,x) -> try Some (Array.map f a, x) with _ -> None) >> Array.of_list

let get_ranges m =
  let get_range def (a,_) =
    if Array.length a = 0 then def else
    Array.fold_left (fun (vl,vh as r) v -> if v < vl then v,vh else if v > vh then vl,v else r) (a.(0),a.(0)) a
  in
  Array.map (get_range (0.0,0.0)) m

let set_names m l =
  Array.mapi (fun i (a,s) -> a, if i < List.length l then List.nth l i else s) m

end (* module Csv *)

let read file =
  let m = with_open_in file Csv.input in
  let m = Csv.set_names m ["Timer";"Num";"Date";"Time";"U_set";"U";"dU_dT";"I";"R";"S_set";"S";
   "dS_dT";"L";"dL_dT";"m";"dm_dT";"Vakuum";"Mode";"Info"] in
  Csv.convert float_of_string m

let csv_get_ranges ch =
  let x = ref [] in
  let collect y =
    x := match !x with
    | [] -> List.map (fun v -> v,v) y
    | l -> try List.map2 (fun (vl,vh) v -> if v < vl then v,vh else if v > vh then vl,v else vl,vh) !x y with _ -> !x
  in
  Std.input_lines ch >> Enum.iter (fun s -> split_columns s >> List.map (fun x -> try float_of_string x with _ -> 0.0) >> collect);
  !x

type column = { name : string; vl : float; vh : float; }
let is_notempty c = c.vl +. epsilon_float < c.vh

(* FIXME exceptions *)
let get_columns name =
  (*dbf_to_csv name;*)
  let (ranges,cols) = with_open_in name (fun ch ->
    let first = input_line ch in
    csv_get_ranges ch >> List.take 19,
    split_columns first >> List.take 19)
  in
  List.map2 (fun name (vl,vh) -> {name=name; vl=vl; vh=vh;})
  (if List.length cols = 19 then
  ["Timer";"Num";"Date";"Time";"U_set";"U";"dU_dT";"I";"R";"S_set";"S";
   "dS_dT";"L";"dL_dT";"m";"dm_dT";"Vakuum";"Mode";"Info"]
  else cols) ranges

let get_columns name = try get_columns name with _ -> []

let display name cols single =
  with_open_out temp_gnuplot (fun ch ->
    with_output_ch ch (fun out -> gnuplot out name cols single)
  );
  Sys.command (sprintf "%s -persist %s" gnuplot_cmd temp_gnuplot) >> ignore

