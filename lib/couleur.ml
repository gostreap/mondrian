open Utils

type couleur = [`Red | `Green | `Blue]

type 'a couleur_l =
  Purple
| Yellow
| Cyan
| White
| C of 'a

let switch_coul_l p y c w f coul =
  match coul with
  | Purple -> p
  | Yellow -> y
  | Cyan   -> c
  | White  -> w
  | C c    -> f c

let switch_coul r g b c =
  match c with
  | `Red -> r
  | `Green -> g
  | `Blue -> b

let switch_coul2 r b c =
  match c with
  | `Red -> r
  | `Blue -> b

let get_rgb = switch_coul Graphics.red (Graphics.rgb 6 139 0) Graphics.blue

let get_rgb_l =
  switch_coul_l
    (Graphics.rgb 255 0 255)
    (Graphics.rgb 255 139 0)
    (Graphics.rgb 0 200 255)
    (Graphics.rgb 170 170 170)
    get_rgb

let string_of_couleur = switch_coul "`Red" "`Green" "`Blue"

let string_of_couleur_l =
  switch_coul_l
    "purple"
    "yellow"
    "cyan"
    "white"
    string_of_couleur

let next_coul = maybe (Some `Blue) (switch_coul (Some `Green) None (Some `Red))
let next_coul2 = maybe (Some `Blue) (switch_coul2 None (Some `Red))
