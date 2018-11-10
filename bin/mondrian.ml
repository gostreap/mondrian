open Bsp
open Graphics

let rec affiche_coloration ?(v=true) ?(infx=0) ?(infy=0) ?(supx=800)
                ?(supy=800) ?(offset=25) (bsp : bsp) =
  match bsp with
  | R x ->
     begin
         match x with
         | None -> ()
         | Some c ->
            begin
                set_color c;
                fill_rect (infx+offset+3) (infy+offset+3) (supx-infx-6) (supy-infy-6)
            end
     end
  | L (lab, l, r) ->
     begin
         if v then
             begin
                 (* set_color black;
                  * set_line_width 1;
                  * draw_segments [|(lab.coord + offset, infy + offset,
                  *                  lab.coord + offset, supy + offset)|] ; *)
                 affiche_coloration ~v:(not v) ~infx:infx ~infy:infy ~supx:lab.coord ~supy:supy l;
                 affiche_coloration ~v:(not v) ~infx:lab.coord ~infy:infy ~supx:supx ~supy:supy r
             end
         else
             begin
                 (* set_color black;
                  * set_line_width 1;
                  * draw_segments [|(infx + offset, lab.coord + offset,
                  *                  supx + offset, lab.coord + offset)|] ; *)
                 affiche_coloration ~v:(not v) ~infx:infx ~infy:infy ~supx:supx ~supy:lab.coord l;  
                 affiche_coloration ~v:(not v) ~infx:infx ~infy:lab.coord ~supx:supx ~supy:supy r
             end
     end

let affiche ?(offset=25) (bsp : bsp) (larg : int) (haut : int) =
  set_line_width 1;
  set_color black;
  (* Affiche un cadre autour du puzzle *)
  draw_segments[|(offset, offset, offset, haut + offset);
                 (offset, haut + offset, larg + offset, haut + offset);
                 (larg + offset, haut + offset, larg + offset, offset);
                 (larg + offset, offset, offset, offset)|] ;
  let linetree = linetree_of_bsp bsp larg haut in
  let rec affiche_linetree linetree =
    match linetree with
    | Leef -> ()
    | Line ((a,b), (x,y), color, left, right) ->
       begin
           match color with
           | None -> set_color black;
           | Some c -> set_color c;
           set_line_width 3;
           draw_segments [|(a + offset, b + offset, x + offset, y + offset)|] ;
           affiche_linetree left;
           affiche_linetree right
       end
  in
  affiche_linetree linetree;
  affiche_coloration bsp


      
let rec loop ?(offset=25) (bsp : bsp) (larg : int) (haut : int) =
  clear_graph ();
  affiche bsp larg haut;
  let e = wait_next_event  [ Button_down ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | _ -> loop bsp larg haut
  else if e.button then
       begin
           let bsp = change_color bsp (e.mouse_x - offset, e.mouse_y - offset) in
           loop bsp larg haut
       end
  else loop bsp larg haut
    
let main () =
  let larg = 800 and haut = 800 in
  Random.self_init ();
  open_graph " 850x850" ;
  let bsp = random_bsp_naive 3 larg haut
  in
  (* print_endline (string_of_bsp bsp); *)
  loop bsp larg haut


let _ = main()
