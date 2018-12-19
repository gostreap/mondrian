open Lib.Bsp
open Lib.Bsp_sat
open Lib.Couleur
open Lib.Solve
open Lib.Formule
open Lib.Generate_formule
open Lib.Generate_formule2
open Graphics

let line_width = 3
let offset = 25

type info_game =
  { larg : int;
    haut : int;
    prof : int;
  }

let rec affiche_coloration ?(v=true) infx infy supx supy bsp =
  match bsp with
  | L (lab, l, r) ->
     if v then
       begin
         affiche_coloration ~v:(not v) infx infy lab.coord supy l;
         affiche_coloration ~v:(not v) lab.coord infy supx supy r
       end
     else
       begin
         affiche_coloration ~v:(not v) infx infy supx lab.coord l;
         affiche_coloration ~v:(not v) infx lab.coord supx supy r
       end
  | R x ->
     match x with
     | None -> ()
     | Some c ->
        set_color (get_rgb c);
        fill_rect
          (infx+offset+line_width)
          (infy+offset+line_width)
          (supx-infx-2*line_width) (supy-infy-2*line_width)

let affiche_linetree (lt : 'a linetree) =
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
          set_line_width line_width;
          draw_segments [|(a + offset, b + offset, x + offset, y + offset)|]
  in
  affiche_linetree lt

let affiche_cadre (larg : int) (haut : int) =
  (* Affiche un cadre autour du puzzle *)
  set_line_width line_width;
  set_color black;
  draw_segments[|(offset, offset, offset, haut + offset);
                 (offset, haut + offset, larg + offset, haut + offset);
                 (larg + offset, haut + offset, larg + offset, offset);
                 (larg + offset, offset, offset, offset)|]

(* Génère un bsp et sa copie vide  *)
let init3coul infos : (couleur bsp * couleur bsp_sat * couleur linetree * couleur bsp) =
  let origin_bsp = random_bsp_naive infos.prof infos.larg infos.haut rand_three_coul in
  let bsp_sat = loop_sat infos.prof (bsp_sat_of_bsp get_color_line origin_bsp) in
  let linetree = linetree_of_bsp get_color_line origin_bsp infos.larg infos.haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (origin_bsp,bsp_sat,linetree,working_bsp)

let init2coul infos : (([`Red | `Blue] as 'a) bsp * 'a bsp_sat * 'a linetree * 'a bsp) =
  let origin_bsp = random_bsp_naive infos.prof infos.larg infos.haut rand_two_coul in
  let bsp_sat = loop_sat infos.prof (bsp_sat_of_bsp get_color_line2 origin_bsp) in
  let linetree = linetree_of_bsp get_color_line2 origin_bsp infos.larg infos.haut in
  let working_bsp = empty_copy_of_bsp origin_bsp in
  (origin_bsp,bsp_sat,linetree,working_bsp)

let rec loop (origin_bsp : ([< `Blue | `Green | `Red ] as 'a) bsp) (origin_bsp_sat: 'a bsp_sat) (working_bsp : 'a bsp) (linetree : 'a linetree) (pmothersol : 'a bsp_sat -> 'a bsp -> 'a linetree -> unit) (fill : 'a bsp_sat -> 'a bsp -> 'a linetree -> 'a bsp)(chcol : (([< `Blue | `Green | `Red ] as 'a) option -> 'a)) infos =
  
  if check_current origin_bsp working_bsp
  then print_endline "victory";
  affiche_linetree linetree;
  affiche_cadre infos.larg infos.haut;
  affiche_coloration 0 0 infos.larg infos.haut working_bsp;
  let e = wait_next_event [ Button_down ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | 'h' ->
       let new_bsp = fill origin_bsp_sat working_bsp linetree in
       loop origin_bsp origin_bsp_sat new_bsp linetree pmothersol fill chcol infos
    | _ -> loop origin_bsp origin_bsp_sat working_bsp linetree pmothersol fill chcol infos
  else
    if e.button
    then
        let bsp = change_color chcol working_bsp (e.mouse_x - offset, e.mouse_y - offset) in
        pmothersol origin_bsp_sat bsp linetree;
        loop origin_bsp origin_bsp_sat bsp linetree pmothersol fill chcol infos
    else loop origin_bsp origin_bsp_sat working_bsp linetree pmothersol fill chcol infos

let debug_main (origin_bsp : [< `Red | `Green | `Blue] bsp) origin_bsp_sat fnc_of_bsp pmothersol prof =
  print_endline (string_of_bsp_sat origin_bsp_sat);
  print_endline "#########################";
  print_formule (fnc_of_bsp prof origin_bsp);
  print_endline "#########################";
  print_endline (machinestring_of_bsp origin_bsp);
  print_endline "#########################";
  pmothersol prof origin_bsp;
  print_endline "#########################"

let main () =
  let infos =
    {larg=800;
     haut=800;
     prof=if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 3;
    } in
  let col3 = if Array.length Sys.argv >= 3 then true else false in
  Random.self_init ();
  open_graph (" " ^ string_of_int (infos.larg + 2 * offset) ^ "x" ^ string_of_int (infos.haut + 2 * offset)) ;
  if col3
   then
     let (origin_bsp,origin_bsp_sat,linetree,working_bsp) = init3coul infos in
     debug_main origin_bsp origin_bsp_sat get_fnc_of_bsp print_maybe_other_sol infos.prof ;
     loop origin_bsp origin_bsp_sat working_bsp linetree print_maybe_other_sol_soluce fill_one_rectangle next_coul infos
  else
    let (origin_bsp,origin_bsp_sat,linetree,working_bsp) = init2coul infos in
     debug_main origin_bsp origin_bsp_sat get_fnc_of_bsp2 print_maybe_other_sol2 infos.prof;
     loop origin_bsp origin_bsp_sat working_bsp linetree print_maybe_other_sol_soluce2 fill_one_rectangle2 next_coul2 infos

let _ = main()
