open Lib.Bsp
open Lib.Bsp_sat
open Lib.Couleur
open Lib.Solve
open Lib.Formule
open Lib.Generate_formule
open Lib.Generate_formule2
open Graphics

let rec affiche_coloration ?(v=true) ?(infx=0) ?(infy=0) (offset : int) (supx : int)
                (supy : int) bsp =
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

let affiche_linetree (offset : int) (lt : 'a linetree) =
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
let init3coul (prof : int) (larg : int) (haut : int) : (couleur bsp * couleur linetree * couleur bsp) =
  let origin_bsp = random_bsp_naive prof larg haut rand_three_coul in
  let linetree = linetree_of_bsp get_color_line origin_bsp larg haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (origin_bsp,linetree,working_bsp)

let init2coul (prof : int) (larg : int) (haut : int) :
      (([`Red | `Blue] as 'a) bsp * 'a linetree * 'a bsp) =
  let origin_bsp = random_bsp_naive prof larg haut rand_two_coul in
  let linetree = linetree_of_bsp get_color_line2 origin_bsp larg haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (origin_bsp,linetree,working_bsp)

let rec loop (offset : int) (origin_bsp : ([< `Blue | `Green | `Red ] as 'a) bsp) (working_bsp : 'a bsp) (linetree : 'a linetree) (pmothersol : 'a bsp -> 'a linetree -> unit) (chcol : (([< `Blue | `Green | `Red ] as 'a) option -> 'a)) (larg : int) (haut : int) (prof : int)=
  if check_current origin_bsp working_bsp
  then
    print_endline "victory";
  clear_graph ();
  affiche_linetree offset linetree;
  affiche_cadre offset larg haut;
  affiche_coloration offset larg haut working_bsp;
  let e = wait_next_event [ Button_down ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | _ -> loop offset origin_bsp working_bsp linetree pmothersol chcol larg haut prof
  else
    if e.button
    then
        let bsp = change_color chcol  working_bsp (e.mouse_x - offset, e.mouse_y - offset) in
        pmothersol bsp linetree;
        loop offset origin_bsp bsp linetree pmothersol chcol larg haut prof
    else loop offset origin_bsp working_bsp linetree pmothersol chcol larg haut prof

let debug_main (origin_bsp : [< `Red | `Green | `Blue] bsp) fnc_of_bsp pmothersol get_col prof =
  print_endline (string_of_bsp origin_bsp);
  print_endline "#########################";
  let bsp_sat = loop_sat prof (bsp_sat_of_bsp get_col origin_bsp) in
  print_endline (string_of_bsp_sat bsp_sat);
  print_endline "#########################";
  print_formule (fnc_of_bsp prof origin_bsp);
  print_endline "#########################";
  print_endline (machinestring_of_bsp origin_bsp);
  print_endline "#########################";
  pmothersol prof origin_bsp;
  print_endline "#########################"

let main () =
  let larg = 800
  and haut = 800
  and offset = 25 in
  Random.self_init ();
  open_graph (" " ^ string_of_int (larg + 2 * offset) ^ "x" ^ string_of_int (haut + 2 * offset)) ;
  let prof = 6 in
  let col3 = false in
  if col3
   then
     let (origin_bsp,linetree,working_bsp) = init3coul prof larg haut in
     debug_main origin_bsp get_fnc_of_bsp print_maybe_other_sol get_color_line prof ;
     loop offset origin_bsp working_bsp linetree print_maybe_other_sol_soluce next_coul larg haut prof
  else
    let (origin_bsp,linetree,working_bsp) = init2coul prof larg haut in
     debug_main origin_bsp get_fnc_of_bsp2 print_maybe_other_sol2 get_color_line2 prof;
     loop offset origin_bsp working_bsp linetree print_maybe_other_sol_soluce2 next_coul2 larg haut prof

let _ = main()
