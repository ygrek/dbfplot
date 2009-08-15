
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

let new_file (box:GPack.box) =
  match ask_for_file () with
  | None -> None
  | Some file ->
  dbf_to_csv file;
  let columns = get_columns file in
  List.iter (fun w -> w#destroy ()) box#children;
  List.map (fun col -> GButton.check_button ~label:col ~packing:box#pack (), col) columns >> some

let main () =

  let cols = ref [] in

  let window = GWindow.window ~title:"dbfplot" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let mainbox = GPack.vbox ~packing:window#add () in
  let bbox = GPack.hbox ~packing:mainbox#pack () in
  let box_sel = GPack.vbox ~packing:mainbox#pack () in

  let bopen = GButton.button ~label:"Open File" ~packing:bbox#pack () in

  let bdraw = GButton.button ~label:"Draw" ~packing:bbox#pack () in
  let _ = bdraw#connect#clicked ~callback:(fun () ->
    List.mapi (fun i (b,n) -> if b#active then Some (i,n) else None) !cols >> List.filter_map id >> display)
  in

  let b = GButton.button ~label:"Quit" ~packing:bbox#pack () in
  let _ = b#connect#clicked ~callback:window#destroy in

  let open_file () =
    begin match new_file (box_sel :> GPack.box) with
    | None -> ()
    | Some l -> cols := l
    end
  in
  let _ = bopen#connect#clicked ~callback:open_file in

  open_file ();

  window#show ();
  GMain.main ()

