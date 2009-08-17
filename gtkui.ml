
open Printf
open Prelude
open ExtLib
open Dbf

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
    ~patterns:[ "*.csv"] ()

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

let main () =

  let cols = ref [] in
  let csv_file = ref "" in

  let window = GWindow.window ~title:(sprintf "dbfplot %s" Version.id) ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let mainbox = GPack.vbox ~packing:window#add () in
  let bbox = GPack.hbox ~packing:mainbox#pack () in
  let lbox = GPack.hbox ~packing:(mainbox#pack ~padding:2) () in
  let box_opt = GPack.hbox ~packing:(mainbox#pack ~padding:2) () in
  let box_sel = GPack.vbox ~packing:mainbox#pack () in

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
    filename#set_text file;
    let columns = get_columns file in
    List.iter (fun w -> w#destroy ()) box_sel#children;
    cols := List.map (fun col -> GButton.check_button ~label:(sprintf "%s (%.3f .. %.3f)" col.name col.vl col.vh) ~packing:box_sel#pack (), col) columns
  in

  let open_file () = ask_for_file () >> Option.may on_new_file in

  let _ = bopen#connect#clicked ~callback:open_file in
  let _ = bdraw#connect#clicked ~callback:(fun () ->
    let cols = List.mapi (fun i (b,col) -> if b#active then Some (i,col.name) else None) !cols >> List.filter_map id in
    display !csv_file cols bsingle#active)
  in


  open_file ();
(*   on_new_file "Omega200 Uzhgorod/Omega 200-1/09-04-13.DBF"; *)

  window#show ();
  GMain.main ()

let main () = 
  try
    main ()
  with e -> 
    error (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ()); raise e
