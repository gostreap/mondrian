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
let draw_r c r =
  let l = 3 in
  set_color black;
  fill_rect r.x r.y r.w r.h;
  set_color c;
  fill_rect (r.x+l) (r.y+l) (r.w-2*l) (r.h-2*l);
  set_color black

(* Affiche des carrés contenant des indices entre 1 et nx*ny, retourne une liste associative entre les carrés et leurs indices *)
let draw_choices_num larg haut offset nx ny stop =
  set_color black;
  moveto offset haut;
  draw_string "Profondeur maximale:";
  set_color white;
  fill_rect 0 0 larg haut;
  set_color black;
  let w = (larg-2*offset)/nx in
  let h = (haut-2*offset)/ny in
  let res = ref [] in
  for j=0 to ny-1 do
    for i=0 to nx-1 do
      let ind = i+j*nx+1 in
      if ind <= stop
      then begin
         let r = {x=i*w+offset; y=j*h+offset; h=h-3; w=w-3;} in
         draw_r white r;
         draw_str r (string_of_int ind);
         res := (r,ind)::!res
       end
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
  set_font "-misc-dejavu sans mono-bold-r-normal--18-0-0-0-m-0-iso8859-1";
  draw_str {x=offset;y=haut-offset;w=larg-2*offset;h=offset} "Mondrian";
  let lb = (larg-offset)/3 in
  let hb = (haut-offset)/4 in
  let r1 = {x=offset; y=3*hb; w=lb; h=hb} in
  let r2 = {x=offset + 2*lb; y=3*hb; w=lb; h=hb} in
  let dr1 c =
    draw_r c r1;
    draw_str r1 "2 couleurs" in
  let dr2 c =
    draw_r c r2;
    draw_str r2 "3 couleurs" in
  dr1 white;
  dr2 white;
  let rec find_prof nb arr : info_game =
    let e = wait_next_event [ Button_down ] in
    match is_in_l e.mouse_x e.mouse_y arr with
    | None ->
       test e (fun () -> find_prof nb arr)
    | Some prof ->
       begin
         clear_graph ();
         {larg=larg; haut=haut; prof=prof; nbcoul=nb;}
       end
  and test e f =
    if is_in r1 e.mouse_x e.mouse_y
    then
      begin
        dr2 white;
        dr1 red;
        find_prof 2 (draw_choices_num larg (2*haut/3) offset 3 3 9)
      end
    else
      if is_in r2 e.mouse_x e.mouse_y
      then
        begin
          dr1 white;
          dr2 red;
          find_prof 3 (draw_choices_num larg (2*haut/3) offset 3 2 5)
        end
      else f ()
  in
  let rec find_coul () =
    let e = wait_next_event [ Button_down ] in
    test e find_coul in
  find_coul ()
