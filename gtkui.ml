
open Printf
open Prelude
open ExtLib
open Data

let x = new Lang.ru

let locale = GtkMain.Main.init ()

let error ?parent message =
  let w =
    GWindow.message_dialog ~message
      ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close
      ?parent ~destroy_with_parent:true ~show:true () in
  w#run () >> ignore;
  w#destroy ();
  ()

let dbf_filter () =
  GFile.filter
    ~name:"DBase files"
    ~patterns:[ "*.dbf"; "*.DBF"] ()

let csv_filter () =
  GFile.filter
    ~name:"CSV files"
    ~patterns:[ "*.csv"; "*.CSV"] ()

let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let ask_for_file ?parent () =
  let dialog = GWindow.file_chooser_dialog
      ~action:`OPEN
      ~title:x#open_file
      ?parent ()
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (csv_filter ()) ;
  dialog#add_filter (all_files ()) ;
  let r = begin match dialog#run () with
  | `OPEN -> dialog#filename
  | `DELETE_EVENT | `CANCEL -> None
  end 
  in
  dialog#destroy (); r

let float = float_of_int

let linear a b c d =
  if abs_float (a -. b) < 2. *. epsilon_float then fun _ -> c else
  let k = float (d - c) /. (b -. a) in
  let q = (float  c) -. k *. a in
  fun x -> int_of_float (k *. x +. q)

let colors = [| 0,0,0; 255,0,0; 0,255,0; 0,0,255; 255,0,255; |] >> Array.map (fun (r,g,b) -> `RGB (r*256,g*256,b*256))
let styles = [| `SOLID; `ON_OFF_DASH; |]

let select_style (dw:GDraw.drawable) ctr =
    let color = ctr mod Array.length colors in
    let style = (ctr / Array.length colors) mod Array.length styles in
    dw#set_foreground colors.(color);
    dw#set_line_attributes ~style:styles.(style) ()

let new_set_style () =
  let ctr = ref 0 in
  fun dw -> select_style dw !ctr; incr ctr

let main () =

  let cols = ref [] in
  let csv_file = ref "" in
  let csv_data = ref [||] in

  let window = GWindow.window ~title:(sprintf "dbfplot %s" Version.id) ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let mainbox = GPack.vbox ~packing:window#add () in
  let bbox = GPack.hbox ~packing:mainbox#pack () in
  let lbox = GPack.hbox ~packing:(mainbox#pack ~padding:2) () in
  let box_opt = GPack.hbox ~packing:(mainbox#pack ~padding:2) () in
  let box_main = GPack.hbox ~packing:(mainbox#pack ~expand:true) () in
  let box_sel = GPack.vbox ~packing:box_main#pack () in

  let is_active i =
    if i < List.length !cols then
      let (b,_) = List.nth !cols i in
      b#active
    else
      false
  in

  let da = GMisc.drawing_area ~packing:box_main#add () in
  let dw = da#misc#realize (); new GDraw.drawable (da#misc#window) in
  let expose_event _ =
    dw#set_foreground `WHITE;
    let (w,h) = dw#size in
    dw#rectangle ~filled:true ~x:0 ~y:0 ~width:w ~height:h ();
    dw#set_foreground `BLACK;
    if Csv.is_ok !csv_data then
    begin
    let ranges = Csv.get_ranges !csv_data in
    let n = Array.length (fst !csv_data.(0)) in
    let x = fun v -> linear 0. (float (n-1)) 0 w (float v) in
    let y i = linear (fst ranges.(i)) (snd ranges.(i)) h 0 in
    for i = 0 to Array.length !csv_data - 1 do
      if is_active i then
      (
      select_style dw i;
      let a = fst !csv_data.(i) in
      let y = y i in
      for j = 1 to n - 1 do
        dw#line ~x:(x (j-1)) ~y:(y a.(j-1)) ~x:(x j) ~y:(y a.(j));
      done
      )
    done;
    end;
    false
  in
  da#event#connect#expose ~callback:expose_event >> ignore;

  let bopen = GButton.button ~label:x#open_file ~packing:bbox#pack () in

  let bdraw = GButton.button ~label:x#draw ~packing:bbox#pack () in

  let b = GButton.button ~label:x#quit ~packing:bbox#pack () in
  let _ = b#connect#clicked ~callback:window#destroy in

  let _ = GMisc.label ~packing:lbox#pack ~text:(x#file ^ " : ") () in
  let filename = GMisc.label ~packing:lbox#pack () in

  let button label packing f =
    let b = GButton.button ~label ~packing () in
    let _ = b#connect#clicked ~callback:f in
    ()
  in

  let opt = box_opt#pack ~padding:2 in
  button x#clear_all opt (fun () -> List.iter (fun (b,_) -> b#set_active false) !cols);
  button x#check_valid opt (fun () -> List.iter (fun (b,col) -> if is_notempty col then b#set_active true) !cols);

  let bsingle = GButton.check_button ~label:x#single_plot ~packing:box_opt#pack () in

  let on_new_file file =
    csv_file := file;
    (*dbf_to_csv file;*)
    csv_data := read file;
    filename#set_text file;
    let columns = get_columns file in
    List.iter (fun w -> w#destroy ()) box_sel#children;
    cols := List.map (fun col -> 
      let b = GButton.check_button ~label:(sprintf "%s (%.3f .. %.3f)" col.name col.vl col.vh) ~packing:box_sel#pack () in
      b#connect#clicked ~callback:(ignore & expose_event) >> ignore;
      b, col) 
    columns
  in

  let open_file () = ask_for_file () >> Option.may on_new_file in

  let _ = bopen#connect#clicked ~callback:open_file in
  let _ = bdraw#connect#clicked ~callback:(fun () ->
    let cols = List.mapi (fun i (b,col) -> if b#active then Some (i,col.name) else None) !cols >> List.filter_map id in
    display !csv_file cols bsingle#active)
  in

(*   open_file (); *)
  on_new_file "Omega200 Uzhgorod/Omega 200-1/09-04-13.DBF.CSV";

  window#show ();
  GMain.main ()

let main () = 
  try
    main ()
  with e -> 
    error (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ()); raise e
