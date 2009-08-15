
open Printf
open Prelude
open ExtLib
open Dbf

let locale = GtkMain.Main.init ()

let error ?parent message =
  let w =
    GWindow.message_dialog ~message
      ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close
      ?parent ~destroy_with_parent:true ~show:true () in
  let _ = w#connect#response (fun _ -> w#destroy ()) in
  ()

let dbf_filter () =
  GFile.filter
    ~name:"DBase files"
    ~patterns:[ "*.dbf"; "*.DBF"] ()

let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let ask_for_file ?parent () =
  let dialog = GWindow.file_chooser_dialog
      ~action:`OPEN
      ~title:"Open File"
      ?parent ()
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (all_files ()) ;
  dialog#add_filter (dbf_filter ()) ;
  let r = begin match dialog#run () with
  | `OPEN -> dialog#filename
  | `DELETE_EVENT | `CANCEL -> None
  end 
  in
  dialog#destroy (); r

let main () =

  let cols = ref [] in

  let window = GWindow.window ~title:"dbfplot" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let mainbox = GPack.vbox ~packing:window#add () in
  let bbox = GPack.hbox ~packing:mainbox#pack () in
  let lbox = GPack.hbox ~packing:(mainbox#pack ~padding:2) () in
  let box_sel = GPack.vbox ~packing:mainbox#pack () in

  let bopen = GButton.button ~label:"Open File" ~packing:bbox#pack () in

  let bdraw = GButton.button ~label:"Draw" ~packing:bbox#pack () in
  let _ = bdraw#connect#clicked ~callback:(fun () ->
    List.mapi (fun i (b,n) -> if b#active then Some (i,n) else None) !cols >> List.filter_map id >> display)
  in

  let b = GButton.button ~label:"Quit" ~packing:bbox#pack () in
  let _ = b#connect#clicked ~callback:window#destroy in

  let _ = GMisc.label ~packing:lbox#pack ~text:"File : " () in
  let filename = GMisc.label ~packing:lbox#pack () in

  let on_new_file file =
    dbf_to_csv file;
    filename#set_text file;
    let columns = get_columns file in
    List.iter (fun w -> w#destroy ()) box_sel#children;
    cols := List.map (fun (col,(vl,vh)) -> GButton.check_button ~label:(sprintf "%s (%.3f .. %.3f)" col vl vh) ~packing:box_sel#pack (), col) columns
  in

  let open_file () = ask_for_file () >> Option.may on_new_file in
  let _ = bopen#connect#clicked ~callback:open_file in

(*   open_file (); *)
(*   on_new_file "Omega200 Uzhgorod/Omega 200-1/09-04-13.DBF"; *)

  window#show ();
  GMain.main ()

