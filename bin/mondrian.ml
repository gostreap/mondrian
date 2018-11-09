open Bsp
open Graphics

let rec loop () =
  let e = wait_next_event  [ Mouse_motion ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | _ -> loop ()
  else loop ()

let rec affiche ?(v=true) ?(infx=0) ?(infy=0) ?(supx=500) ?(supy=500) (bsp : bsp) =
  set_line_width 1;
  set_color black;
  draw_segments[|(25,25,25,525);(25,525,525,525);(525,525,525,25);(525,25,25,25)|] ;
    match bsp with
    | R x ->
       begin
           match x with
           | None -> ()
           | Some c ->
              begin
                  set_color c;
                  fill_rect (infx+25+2) (infy+25+2) (supx-infx-4) (supy-infy-4)
              end
       end
    | L (lab, l, r) ->
       begin
           if v then
               begin
                   set_color black;
                   set_line_width 1;
                   draw_segments [|(lab.coord + 25, infy + 25, lab.coord + 25, supy + 25)|] ;
                   affiche ~v:(not v) ~infx:infx ~infy:infy ~supx:lab.coord ~supy:supy l;
                   affiche ~v:(not v) ~infx:lab.coord ~infy:infy ~supx:supx ~supy:supy r
               end
           else
               begin
                   set_color black;
                   set_line_width 1;
                   draw_segments [|(infx + 25, lab.coord + 25, supx + 25, lab.coord + 25)|] ;
                   affiche ~v:(not v) ~infx:infx ~infy:infy ~supx:supx ~supy:lab.coord l;  
                   affiche ~v:(not v) ~infx:infx ~infy:lab.coord ~supx:supx ~supy:supy r
               end
       end
  
let main () =
  Random.self_init ();
  open_graph " 550x550" ;
  let bsp = random_bsp_naive 4 500 500
  in
  affiche bsp;
  print_string (string_of_bsp bsp);
  loop ()


let _ = main()
