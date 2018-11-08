open Graphics

type label =
  { coord : int;
    colored : bool;
  }

(* Commence avec une séparation verticale *)
type bsp = R of color option
         | L of label * bsp * bsp

type point = (int * int)

(* Affichage rudimentaire de BSP *)
let rec string_of_bsp (bsp : bsp) =
  match bsp with
  | R x ->
     begin
       match x with
         None -> "None"
       | Some x -> if x = red then "red" else "blue"
     end
  | L (lab,l,r) ->
    "(" ^ (string_of_bsp l) ^ ") " ^ (string_of_int (lab.coord)) ^ " (" ^ (string_of_bsp r) ^ ")"

(* Génère un BSP aléatoire de profondeur 'profondeur'

NOTE: Pour l'instant, toutes les arrêtes sont visibles

 *)
let rec random_bsp_naive ?(v=true) (profondeur : int) (largeur : int) (hauteur : int) =
  if profondeur = 0
  then R (Some (if Random.bool () then blue else red))
  else
    let lab =
      { coord = Random.int (if v then largeur else hauteur);
        colored = true
      } in
    let l = random_bsp_naive ~v:(not v) (profondeur-1) (if v then lab.coord else largeur) (if v then hauteur else lab.coord) in
    let r = random_bsp_naive ~v:(not v) (profondeur-1) (if v then (largeur-lab.coord) else largeur) (if v then hauteur else (hauteur - lab.coord)) in
    L (lab,l,r)


(* Change la couleur d'un rectangle d'un bsp, dans lequel se situe p *)
let rec change_color ?(v=true) (bsp : bsp) ((x,y) as p : point) =
    match bsp with
    | R c ->
       begin
         match c with
         |  None ->   R (Some blue)
         | Some c -> R (Some (if c = blue then red else blue))
       end
    | L (lab, left, right) ->
       if v
       then if x < lab.coord
            then L (lab, change_color ~v:(not v) left p, right)
            else L (lab, left, change_color ~v:(not v) right (x-lab.coord,y))
       else if y < lab.coord
            then L (lab, change_color ~v:(not v) left p, right)
            else L (lab, left, change_color ~v:(not v) right (x,y-lab.coord))
