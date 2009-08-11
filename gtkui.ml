
open Printf
open Prelude
open ExtLib

let locale = GtkMain.Main.init ()

let error ?parent message =
  let w =
    GWindow.message_dialog ~message
      ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close
      ?parent ~destroy_with_parent:true ~show:true () in
  let _ = w#connect#response (fun _ -> w#destroy ()) in
  ()

let control f columns =

  let window = GWindow.window ~title:"dbfplot" ~border_width:10 () in
  let _ = window#event#connect#delete ~callback:(fun ev -> GMain.Main.quit (); false) in
  let mainbox = GPack.vbox ~packing:window#add () in

  (* ----------------------------------------------------------------------- *)

  let box_select = GPack.vbox ~packing:(mainbox#pack ~padding:2) () in

  let cols = List.map (fun col ->
    GButton.check_button ~label:col ~packing:box_select#pack ()) columns in

  let _ = GMisc.separator `HORIZONTAL ~packing:(mainbox#pack ~padding:2) () in

  (* ----------------------------------------------------------------------- *)

  let button = GButton.button ~label:"Draw" ~packing:mainbox#pack () in
  let _ = button#connect#clicked ~callback:(fun () ->
    let sel = List.mapi (fun i b -> if b#active then Some i else None) cols >> List.filter_map id in
    f sel)
  in

  let button = GButton.button ~label:"Quit" ~packing:mainbox#pack () in
  let _ = button#connect#clicked ~callback:(GMain.Main.quit) in

  window#show ();
  GMain.Main.main ()

