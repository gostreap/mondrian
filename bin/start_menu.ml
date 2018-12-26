open Graphics

open Types

type rect =
  {
    x:int;
    y:int;
    h:int;
    w:int;
  }

let draw_str r str =
  let (l,_) = text_size str in
  let l' = (r.w-l)/2 in
  moveto (r.x+l') (r.y+r.h/3);
  draw_string str

let is_in r u v =
  u >= r.x && u <= (r.x+r.w) && v >= r.y && v <= (r.y+r.h)

let draw_r r =
  let l = 3 in
  set_color black;
  fill_rect r.x r.y r.h r.w;
  set_color white;
  fill_rect (r.x+l) (r.y+l) (r.h-2*l) (r.w-2*l)

let start_menu larg haut offset =
  clear_graph ();
  set_color black;
  let lb = (larg-offset)/3 in
  let hb = (haut-offset)/4 in
  let r1 = {x=offset; y=3*hb; h=lb; w=hb} in
  let r2 = {x=offset + 2*lb; y=3*hb; h=lb; w=hb} in
  draw_r r1;
  draw_r r2;
  set_color black;
  set_font "-misc-dejavu sans mono-bold-r-normal--18-0-0-0-m-0-iso8859-1";
  draw_str r1 "2 couleurs";
  draw_str r2 "3 couleurs";
  let rec find_coul () =
    let e = wait_next_event [ Button_down ] in
    if is_in r1 e.mouse_x e.mouse_y
    then 2
    else
      if is_in r2 e.mouse_x e.mouse_y
      then 3
      else find_coul () in
  let nb = find_coul () in

  let prof = 4 in
  clear_graph ();
  {larg=larg; haut=haut; prof=prof; nbcoul=nb;}
