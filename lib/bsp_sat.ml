open Bsp
open Couleur
open Utils

(* bsp for SAT *)
type bsp_sat =
  | R_sat of int * bool * couleur (* id * secure * coul *)
  | L_sat of couleur_l option * bool * bsp_sat * bsp_sat (* coul * secure * left * right *)

let rec string_of_bsp_sat (bsp : bsp_sat) =
  match bsp with
  | L_sat (lab,b,l,r) ->
     "(" ^ (string_of_bsp_sat l) ^ " " ^string_of_bool b  ^ "*" ^ (maybe "black" string_of_couleur_l lab) ^
         " " ^ (string_of_bsp_sat r) ^ ")"
  | R_sat (n,x,c) -> string_of_int n ^ "*" ^ string_of_bool x ^ "*" ^ (switch_coul "r" "g" "b" c)

let bsp_sat_of_bsp (bsp : bsp) =
  let rec aux v bsp =
    match bsp with
    | R x ->
       let c =
         match x with
         | None -> failwith "translate_bsp"
         | Some x -> x in (v+1,R_sat (v,false,c))
    | L (lab,l,r) ->
       let (n,ll) = aux v l in
       let (m,rr) = aux n r in
       let c =
         if lab.colored
         then get_color_line bsp
         else None in
       (m,L_sat (c,false,ll,rr))
  in snd (aux 0 bsp)

(* Renvoie un bsp ou les feuilles ont un indice différent de 1 si leur couleur est fixé *)
let rec secure_bsp_sat (bsp : bsp_sat) =
    match bsp with
  | L_sat (c,_,l,r) ->
     let lr =
       match l, r with
       | R_sat (n,_,co), R_sat (m,_,_) -> Some (L_sat (c,true,R_sat (n,true,co),R_sat (m,true,co)))
       | L_sat (_,b,_,_) as l, (L_sat (_,b',_,_) as r) ->
          if b && b'
          then Some (L_sat (c,true,l,r))
          else None
       | _ -> None

     in
     if maybe false (switch_coul_l false false false false (fun _ -> true)) c
     then
       match lr with
       | Some x -> x
       | _ -> L_sat (c,false,secure_bsp_sat l,secure_bsp_sat r)
     else L_sat (c,false,secure_bsp_sat l,secure_bsp_sat r)
  | i -> i

let rec loop_sat (n : int) (b : bsp_sat) =
  if n <= 0
  then b
  else loop_sat (n-1) (secure_bsp_sat b)

(* TOOOOODDDDDDOOOOOO Compter en utilisant les lignes sécurisées ? *)

(* Renvoie un couple (r, g, b, list) où:
 * r est le nombre de rectangle rouge adjacents sécurisés
 * g est le nombre de rectangle vert adjacents sécurisés
 * b est le nombre de rectangle bleu adjacents sécurisés
 * list est la liste des rectangles non sécurisés*)
let get_adja_stat (bsp_sat : bsp_sat) =
  let rec get_stat ?(v=true) (is_l : bool) (bsp_sat : bsp_sat)
          : (int * int * int * int list) =
    match bsp_sat with
    | L_sat (_,_,x,y) ->
       let rx,gx,bx,ll as x' = get_stat ~v:(not v) is_l x in
       let ry,gy,by,lr as y' = get_stat ~v:(not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,gx+gy,bx+by,ll@lr)
    | R_sat (n,s,x) ->
       if s then
           let (r,g,b) = switch_coul (1,0,0) (0,1,0) (0,0,1) x in
           (r,g, b, [])
       else (0,0,0,[n])
  in
  match bsp_sat with
  | R_sat _ -> (0,0,0,[])
  | L_sat (_,_,l,r) ->
     let (lr,lg,lb,llist) = get_stat true l in
     let (rr,rg,rb,rlist) = get_stat false r in
     (lr+rr,lg+rg,lb+rb,llist@rlist)
