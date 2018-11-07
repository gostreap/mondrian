open List

type label =
  { coord : int;
    colored : bool;
  }

type bsp = R of Graphics.color option
         | L of label * bsp * bsp

(* Affichage rudimentaire de BSP *)
let rec string_of_bsp (bsp : bsp) =
  match bsp with
  | R x ->
     begin
       match x with
         None -> "None"
       | Some x -> if x = Graphics.red then "red" else "blue"
     end
  | L (lab,l,r) ->
    "(" ^ (string_of_bsp l) ^ ") " ^ (string_of_int (lab.coord)) ^ " (" ^ (string_of_bsp r) ^ ")"

(* Génère un BSP aléatoire de profondeur 'profondeur'

NOTE: Pour l'instant, toutes les arrêtes sont visibles

 *)
let random_bsp_naive (profondeur : int) (largeur : int) (hauteur : int) =
  let rec aux profondeur largeur hauteur vertical =
  if profondeur = 0
  then R (Some (if Random.bool () then Graphics.blue else Graphics.red))
  else
    let lab =
      { coord = Random.int (if vertical then largeur else hauteur);
        colored = true
      } in
    let l = aux (profondeur-1) largeur hauteur (not vertical) in
    let r = aux (profondeur-1) largeur hauteur (not vertical) in
    L (lab,l,r)
  in aux profondeur largeur hauteur true
