open Bsp
open Couleur
open Graphics

let rec affiche_coloration ?(v=true) ?(infx=0) ?(infy=0) ?(supx=800)
                ?(supy=800) ?(offset=25) (bsp : bsp) =
  match bsp with
  | L (lab, l, r) ->
     if v then
       begin
         affiche_coloration ~v:(not v) ~infx:infx ~infy:infy ~supx:lab.coord ~supy:supy l;
         affiche_coloration ~v:(not v) ~infx:lab.coord ~infy:infy ~supx:supx ~supy:supy r
       end
     else
       begin
         affiche_coloration ~v:(not v) ~infx:infx ~infy:infy ~supx:supx ~supy:lab.coord l;
         affiche_coloration ~v:(not v) ~infx:infx ~infy:lab.coord ~supx:supx ~supy:supy r
       end
  | R x ->
     match x with
     | None -> ()
     | Some c ->
        set_color (get_rgb c);
        fill_rect (infx+offset+3) (infy+offset+3) (supx-infx-6) (supy-infy-6)

let affiche_linetree ?(offset=25) (lt : linetree) =
  let rec affiche_linetree linetree =
    match linetree with
    | Leef -> ()
    | Line ((a,b), (x,y), color, left, right) ->
       match color with
       | None -> set_color black;
       | Some c ->
          affiche_linetree left;
          affiche_linetree right;
          set_color (get_rgb_l c);
          set_line_width 3;
          draw_segments [|(a + offset, b + offset, x + offset, y + offset)|]
  in
  affiche_linetree lt

let affiche_cadre ?(offset=25) (larg : int) (haut : int) =
  (* Affiche un cadre autour du puzzle *)
  set_line_width 3;
  set_color black;
  draw_segments[|(offset, offset, offset, haut + offset);
                 (offset, haut + offset, larg + offset, haut + offset);
                 (larg + offset, haut + offset, larg + offset, offset);
                 (larg + offset, offset, offset, offset)|]

let rec loop ?(offset=25) (origin_bsp : bsp) (bsp : bsp)
             (linetree : linetree) (larg : int) (haut : int) =
  if check_current origin_bsp bsp then
      print_endline "BRAVO ! VOUS AVEZ COMPLETE LE PUZZLE !";
  clear_graph ();
  affiche_linetree linetree;
  affiche_cadre larg haut;
  affiche_coloration bsp;
  let e = wait_next_event  [ Button_down ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | _ -> loop origin_bsp bsp linetree larg haut
  else
    if e.button
    then
      let bsp = change_color bsp (e.mouse_x - offset, e.mouse_y - offset) in
      loop origin_bsp bsp linetree larg haut
    else loop origin_bsp bsp linetree larg haut

let main () =
  let larg = 800 and haut = 800 in
  Random.self_init ();
  open_graph " 850x850" ;
  let origin_bsp = random_bsp_naive 4 larg haut in
  let linetree = linetree_of_bsp origin_bsp larg haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (* print_endline (string_of_bsp bsp); *)
  loop origin_bsp working_bsp linetree larg haut

let _ = main()
