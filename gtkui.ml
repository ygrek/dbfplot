
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

  match ask_for_file () with
  | None -> ()
  | Some file ->
  dbf_to_csv file;
  let columns = get_columns file in

  let window = GWindow.window ~title:"dbfplot" ~border_width:10 () in
(*   window#event#connect#delete ~callback:(fun _ -> prerr_endline "Delete event occured"; false); *)
  window#connect#destroy ~callback:GMain.quit;

  let mainbox = GPack.vbox ~packing:window#add () in

(*   let b = GButton.button ~label:"Open File" ~packing:mainbox#pack () in *)
(*   b#connect#clicked ~callback:(fun () -> file := ask_for_file window); *)

  (* ----------------------------------------------------------------------- *)

  let box_select = GPack.vbox ~packing:(mainbox#pack ~padding:2) () in

  let cols = List.map (fun col ->
    GButton.check_button ~label:col ~packing:box_select#pack (), col) columns in

(*   let _ = GMisc.separator `HORIZONTAL ~packing:(mainbox#pack ~padding:2) () in *)

  (* ----------------------------------------------------------------------- *)

  let button = GButton.button ~label:"Draw" ~packing:mainbox#pack () in
  let _ = button#connect#clicked ~callback:(fun () ->
    let sel = List.mapi (fun i (b,n) -> if b#active then Some (i,n) else None) cols >> List.filter_map id in
    display sel)
  in

  let button = GButton.button ~label:"Quit" ~packing:mainbox#pack () in
  button#connect#clicked ~callback:window#destroy;

  window#show ();
  GMain.main ()

