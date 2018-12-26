open Graphics

open Types

type rect =
  {
    x:int;
    y:int;
    h:int;
    w:int;
  }

(* Affiche un string au milieu d'un rectangle *)
let draw_str r str =
  let (l,_) = text_size str in
  let l' = (r.w-l)/2 in
  set_color black;
  moveto (r.x+l') (r.y+r.h/3);
  draw_string str

(* Teste si un point (x,y) est dans le rectangle r *)
let is_in r u v =
  u >= r.x && u <= (r.x+r.w) && v >= r.y && v <= (r.y+r.h)

(* Dessine un rectangle *)
let draw_r r =
  let l = 3 in
  set_color black;
  fill_rect r.x r.y r.h r.w;
  set_color white;
  fill_rect (r.x+l) (r.y+l) (r.h-2*l) (r.w-2*l)

(* Affiche des carrés contenant des indices entre 1 et nx*ny, retourne une liste associative entre les carrés et leurs indices *)
let draw_choices_num larg haut offset nx ny =
  let w = (larg-2*offset)/nx in
  let h = (haut-2*offset)/ny in
  let res = ref [] in
  for j=0 to nx-1 do
    for i=0 to ny-1 do
      let r = {x=i*w+offset; y=j*h+offset; h=h-h/ny; w=w-w/nx;} in
      let ind = j*nx+i+1 in
      draw_r r;
      draw_str r (string_of_int ind);
      res := (r,ind)::!res
    done
  done;
  !res

(* Retourne None si le point (x,y) n'appartient à aucun rectangle de la liste, ou Some indice *)
let is_in_l x y =
  let aux acc (r,i) =
    match acc with
    | None -> if is_in r x y then Some i else None
    | _ -> acc
  in List.fold_left aux None

(* Menu de démarrage *)
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
  clear_graph ();
  let n' = if nb = 2 then 3 else 2 in
  let arr = draw_choices_num larg haut offset n' n' in
  let rec find_prof () =
    let e = wait_next_event [ Button_down ] in
    match is_in_l e.mouse_x e.mouse_y arr with
    | None -> find_prof ()
    | Some x -> x in
  let prof = find_prof () in
  clear_graph ();
  {larg=larg; haut=haut; prof=prof; nbcoul=nb;}
