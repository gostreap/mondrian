open Lib.Bsp
open Lib.Bsp_sat
open Lib.Couleur
open Lib.Solve
open Lib.Formule
open Lib.Generate_formule
open Lib.Generate_formule2
open Graphics
open Lib.Utils

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
          (infy+2*offset+line_width)
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
          draw_segments [|(a + offset, b + 2*offset, x + offset, y + 2*offset)|]
  in
  affiche_linetree lt

let affiche_cadre (larg : int) (haut : int) =
  (* Affiche un cadre autour du puzzle *)
  set_line_width line_width;
  set_color black;
  draw_segments[|(offset, 2*offset, offset, haut + 2*offset);
                 (offset, haut + 2*offset, larg + offset, haut + 2*offset);
                 (larg + offset, haut + 2*offset, larg + offset, 2*offset);
                 (larg + offset, 2*offset, offset, 2*offset)|];
  draw_segments[|(offset, offset/2, offset, offset + offset/2);
                 (offset, offset + offset/2, larg + offset, offset + offset/2);
                 (larg + offset, offset + offset/2, larg + offset, offset/2);
                 (larg + offset, offset/2, offset, offset/2)|]

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

let rec loop (origin_bsp : ([< `Blue | `Green | `Red ] as 'a) bsp) (origin_bsp_sat: 'a bsp_sat) (working_bsp : 'a bsp) (linetree : 'a linetree) (pmothersol : int -> 'a bsp_sat -> 'a bsp -> 'a linetree -> unit) (get_fnc_soluce : int -> 'a bsp -> 'a linetree -> formule option) (chcol : ('a option -> 'a)) (col_first : 'a bsp -> (bool*int) list -> (bool*int) list option * 'a bsp) last_sol infos =
  if check_current origin_bsp working_bsp
  then print_message "Victoire";
  affiche_linetree linetree;
  affiche_cadre infos.larg infos.haut;
  affiche_coloration 0 0 infos.larg infos.haut working_bsp;
  let e = wait_next_event [ Button_down ; Key_pressed ] in
  if e.keypressed
  then
      begin
      clean_message ();
      match e.key with
      | 'q' -> ()
      | 'h' ->
         begin
             print_message "Calcul en cours...";
             let sol, new_bsp = fill_one_rectangle get_fnc_soluce col_first infos.prof origin_bsp_sat working_bsp linetree last_sol in
             loop origin_bsp origin_bsp_sat new_bsp linetree pmothersol get_fnc_soluce chcol col_first sol infos     
         end
      | _ -> loop origin_bsp origin_bsp_sat working_bsp linetree pmothersol get_fnc_soluce chcol col_first last_sol infos
      end
  else
    if e.button
    then
        begin
            clean_message();
            let bsp = change_color chcol working_bsp (e.mouse_x - offset, e.mouse_y - 2*offset) in
            pmothersol infos.prof origin_bsp_sat bsp linetree;
            loop origin_bsp origin_bsp_sat bsp linetree pmothersol get_fnc_soluce chcol col_first None infos
        end
    else loop origin_bsp origin_bsp_sat working_bsp linetree pmothersol get_fnc_soluce chcol col_first last_sol infos

let debug_main (origin_bsp : [< `Red | `Green | `Blue] bsp) origin_bsp_sat fnc_of_bsp pmothersol prof =
  print_endline (string_of_bsp_sat origin_bsp_sat);
  print_endline "#########################";
  print_formule (fnc_of_bsp prof origin_bsp);
  print_endline "#########################";
  print_endline (machinestring_of_bsp origin_bsp);
  print_endline "#########################";
  pmothersol prof origin_bsp;
  print_endline "#########################"

let init () =
  let rec getint low high =
    try
      let res = read_int () in
      if res < low || res > high
      then
        begin
          print_endline ("Entrez un nombre entre "^(string_of_int low)^" et "^(string_of_int high));
          getint low high
        end
      else res
    with
      Failure _ ->
      print_endline "Entrez un entier !";
      getint low high

  in
  print_endline "Bienvenue dans Mondrian, voulez-vous jouer avec 2 ou 3 couleurs ?";
  let coul = getint 2 3 in
  print_endline "Quelle est la profondeur maximum que vous dérirez (nous recommandons moins de 10 pour les 2 couleurs et moins de 5 pour les 3 couleurs) ?";
  (coul, getint 1 15)

let main () =
  let (col3,prof) = init () in
  let infos =
    {larg=800;
     haut=800;
     prof=prof
    } in
  Random.self_init ();
  open_graph (" " ^ string_of_int (infos.larg + 2 * offset) ^ "x" ^ string_of_int (infos.haut + 3 * offset)) ;
  if col3 = 3
   then
     let (origin_bsp,origin_bsp_sat,linetree,working_bsp) = init3coul infos in
     (* debug_main origin_bsp origin_bsp_sat get_fnc_of_bsp print_maybe_other_sol infos.prof ; *)
     loop origin_bsp origin_bsp_sat working_bsp linetree print_maybe_other_sol_soluce get_fnc_of_bsp_soluce next_coul color_first None infos
  else
    let (origin_bsp,origin_bsp_sat,linetree,working_bsp) = init2coul infos in
     (* debug_main origin_bsp origin_bsp_sat get_fnc_of_bsp2 print_maybe_other_sol2 infos.prof; *)
     loop origin_bsp origin_bsp_sat working_bsp linetree print_maybe_other_sol_soluce2 get_fnc_of_bsp_soluce2 next_coul2 color_first2 None infos

let _ = main()
