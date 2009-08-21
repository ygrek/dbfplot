
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

  let bopen = GButton.button ~label:x#open_file ~packing:bbox#pack () in

  let button label packing f =
    let b = GButton.button ~label ~packing () in
    let _ = b#connect#clicked ~callback:f in
    ()
  in

  button x#quit bbox#pack window#destroy;
  GMisc.label ~packing:lbox#pack ~text:(x#file ^ " : ") () >> ignore;
  let filename = GMisc.label ~packing:lbox#pack () in
  let opt = box_opt#pack ~padding:2 in
  button x#clear_all opt (fun () -> List.iter (fun b -> b#set_active false) !cols);
  let bmouse_values = GButton.check_button ~label:x#show_mouse_values ~packing:opt () in
  bmouse_values#set_active true;

  let is_active i =
    if i < List.length !cols then
      let b = List.nth !cols i in
      b#active
    else
      false
  in

  let da = GMisc.drawing_area ~packing:box_main#add () in
(*   let dw = da#misc#realize (); new GDraw.drawable (da#misc#window) in *)

  (* Backing pixmap for drawing area *)
  let backing = ref (GDraw.pixmap ~width:200 ~height:200 ()) in

  let update () =
    let dw = (!backing :> GDraw.drawable) in
    dw#set_foreground `WHITE;
    let (w,h) = dw#size in
    dw#rectangle ~filled:true ~x:0 ~y:0 ~width:w ~height:h ();
    dw#set_foreground `BLACK;
    if Csv.is_ok !csv_data then
    begin
      let (cx,cy) = da#misc#pointer in
      let mouse = cx > 0 && cx < w && cy > 0 && cy < h && bmouse_values#active in
      if mouse then
      begin
        select_style dw 0;
        dw#line ~x:cx ~y:0 ~x:cx ~y:h;
        dw#line ~x:0 ~y:cy ~x:w ~y:cy;
      end;
      let ranges = Csv.get_ranges !csv_data in
      let n = Array.length (fst !csv_data.(0)) in
      let x = fun v -> linear 0. (float (n-1)) 0 w (float v) in
      let y i = linear (fst ranges.(i)) (snd ranges.(i)) h 0 in
      let unx = fun v -> linear 0. (float w) 0 (n-1) (float v) in
      for i = 0 to Array.length !csv_data - 1 do
        if is_active i then
        begin
          select_style dw i;
          let a = fst !csv_data.(i) in
          let y = y i in
          for j = 1 to n - 1 do
            dw#line ~x:(x (j-1)) ~y:(y a.(j-1)) ~x:(x j) ~y:(y a.(j));
          done;
          if mouse then
          begin
            let v = a.(unx cx) in
            let font = da#misc#style#font in
            let str = string_of_float v in
            let str_w = Gdk.Font.string_width font str + 4 in
            let str_h = Gdk.Font.string_height font str + 2 in
            let px = cx and py = y v in
            let x = if px + str_w < w then px + 4 else px - str_w in
            let y = if py - str_h < 0 then py + str_h else py - 2 in
            dw#set_foreground `WHITE;
            dw#rectangle ~x ~y:(y-str_h) ~width:str_w ~height:str_h ~filled:true ();
            select_style dw 0;
            dw#string ~font ~x ~y str;
          end
        end;
      done;
    end;
    da#misc#draw None
  in

  (* Create a new backing pixmap of the appropriate size *)
  let configure ev =
    let width = GdkEvent.Configure.width ev in
    let height = GdkEvent.Configure.height ev in
    let pixmap = GDraw.pixmap ~width ~height ~window () in
    pixmap#set_foreground `WHITE;
    pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    backing := pixmap;
    update ();
    true
  in

  (* Redraw the screen from the backing pixmap *)
  let expose ev =
    let area = GdkEvent.Expose.area ev in
    let x = Gdk.Rectangle.x area in
    let y = Gdk.Rectangle.y area in
    let width = Gdk.Rectangle.width area in
    let height = Gdk.Rectangle.width area in
    let drawing =
      da#misc#realize ();
      new GDraw.drawable (da#misc#window)
    in
    drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
    false
  in

  let motion_notify ev =
    let _ =
      (* needed for MOTION_HINT *)
      if GdkEvent.Motion.is_hint ev 
        then da#misc#pointer 
        else (int_of_float (GdkEvent.Motion.x ev), int_of_float (GdkEvent.Motion.y ev))
    in
    update ();
    true
  in

  da#event#connect#expose ~callback:expose >> ignore;
  da#event#connect#configure ~callback:configure >> ignore;
  da#event#connect#motion_notify ~callback:motion_notify >> ignore;
  da#event#connect#button_press ~callback:(fun ev -> print_endline "event"; false) >> ignore;
  da#event#add [`EXPOSURE;
    `BUTTON_PRESS;
    `POINTER_MOTION;
    `POINTER_MOTION_HINT;
    ];

(*    button x#check_valid opt (fun () -> List.iter (fun (b,col) -> if is_notempty col then b#set_active true) !cols); *)

(*   let bsingle = GButton.check_button ~label:x#single_plot ~packing:box_opt#pack () in *)

  let on_new_file file =
    List.iter (fun w -> w#destroy ()) box_sel#children;
    csv_file := file;
    filename#set_text file;
    csv_data := read file;
    let ranges = Csv.get_ranges !csv_data in
    csv_data := Array.mapi (fun i x -> if is_notempty ranges.(i) then Some x else None) !csv_data >> Array.to_list >> List.filter_map id >> Array.of_list;
    let ranges = Csv.get_ranges !csv_data in
    cols := !csv_data >> Array.mapi (fun i (a,name) ->
      let b = GButton.check_button 
        ~label:(sprintf "%s (%.3f .. %.3f)" name (fst ranges.(i)) (snd ranges.(i)))
        ~packing:box_sel#pack 
        () 
      in
      b#connect#clicked ~callback:update >> ignore;
      b) >> Array.to_list;
    update ();
  in

  let open_file () = ask_for_file () >> Option.may on_new_file in

  let _ = bopen#connect#clicked ~callback:open_file in
(*
  let _ = bdraw#connect#clicked ~callback:(fun () ->
    let cols = List.mapi (fun i (b,col) -> if b#active then Some (i,col.name) else None) !cols >> List.filter_map id in
    display !csv_file cols bsingle#active)
  in
*)

(*   open_file (); *)
  on_new_file "Omega200 Uzhgorod/Omega 200-1/09-04-13.DBF.CSV";

  window#show ();
  GMain.main ()

let main () = 
  try
    main ()
  with e -> 
    error (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ()); raise e
