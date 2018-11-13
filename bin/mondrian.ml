open Bsp
open Couleur
open Graphics

let rec affiche_coloration ?(v=true) ?(infx=0) ?(infy=0) (offset : int) (supx : int)
                (supy : int) (bsp : bsp) =
  match bsp with
  | L (lab, l, r) ->
     if v then
       begin
         affiche_coloration ~v:(not v) ~infx:infx ~infy:infy offset lab.coord supy l;
         affiche_coloration ~v:(not v) ~infx:lab.coord ~infy:infy offset supx supy r
       end
     else
       begin
         affiche_coloration ~v:(not v) ~infx:infx ~infy:infy offset supx lab.coord l;
         affiche_coloration ~v:(not v) ~infx:infx ~infy:lab.coord offset supx supy r
       end
  | R x ->
     match x with
     | None -> ()
     | Some c ->
        set_color (get_rgb c);
        fill_rect (infx+offset+3) (infy+offset+3) (supx-infx-6) (supy-infy-6)

let affiche_linetree (offset : int) (lt : linetree) =
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

let affiche_cadre (offset : int) (larg : int) (haut : int) =
  (* Affiche un cadre autour du puzzle *)
  set_line_width 3;
  set_color black;
  draw_segments[|(offset, offset, offset, haut + offset);
                 (offset, haut + offset, larg + offset, haut + offset);
                 (larg + offset, haut + offset, larg + offset, offset);
                 (larg + offset, offset, offset, offset)|]

(* Génère un bsp et sa copie vide  *)
let init (prof : int) (larg : int) (haut : int) =
  let origin_bsp = random_bsp_naive prof larg haut in
  let linetree = linetree_of_bsp origin_bsp larg haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (origin_bsp,linetree,working_bsp)

let print_victory (offset : int)  (larg : int) (haut : int) loop =
  clear_graph ();
  moveto 0 0;
  draw_string "BRAVO ! VOUS AVEZ COMPLÉTÉ LE PUZZLE !";
  let _ = wait_next_event [Button_down ; Key_pressed] in
  let (o,l,w) = init 5 larg haut in
  loop offset o w l larg haut

let rec loop (offset : int) (origin_bsp : bsp) (bsp : bsp)
             (linetree : linetree) (larg : int) (haut : int) =
  if check_current origin_bsp bsp
  then print_victory offset larg haut loop
  else
    begin
      clear_graph ();
      affiche_linetree offset linetree;
      affiche_cadre offset larg haut;
      affiche_coloration offset larg haut bsp;
      let e = wait_next_event  [ Button_down ; Key_pressed ] in
      if e.keypressed
      then
        match e.key with
        | 'q' -> ()
        | _ -> loop offset origin_bsp bsp linetree larg haut
      else
        if e.button
        then
          let bsp = change_color bsp (e.mouse_x - offset, e.mouse_y - offset) in
          loop offset origin_bsp bsp linetree larg haut
        else loop offset origin_bsp bsp linetree larg haut
    end

let main () =
  let larg = 800
  and haut = 800
  and offset = 25 in
  Random.self_init ();
  open_graph (" " ^ string_of_int (larg + 2 * offset) ^ "x" ^ string_of_int (haut + 2 * offset)) ;
  let prof = 4 in
  let (origin_bsp,linetree,working_bsp) = init prof larg haut in
  print_endline (string_of_bsp origin_bsp);
  print_endline "#########################";
  let bsp_sat = loop_sat 10 (bsp_sat_of_bsp origin_bsp) in
  print_endline (string_of_bsp_sat bsp_sat);
  print_endline "#########################";
  print_endline (string_of_bsp_sat bsp_sat);
  print_endline "#########################";
  print_formule (get_fnc_of_bsp prof origin_bsp);
  print_endline "#########################";
  print_maybe_other_sol prof origin_bsp;
  loop offset origin_bsp working_bsp linetree larg haut

let _ = main()
