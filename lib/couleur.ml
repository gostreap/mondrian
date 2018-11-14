type couleur = Red | Blue

type couleur_l = Purple | C of couleur

let switch_coul_l e f c =
  match c with
  | Purple -> e
  | C c -> f c

let switch_coul r b c =
    match c with
  | Red -> r
  | Blue -> b

let get_rgb = switch_coul Graphics.red Graphics.blue

let get_rgb_l = switch_coul_l (Graphics.rgb 255 0 255) get_rgb

let string_of_couleur = switch_coul "red" "blue"

let string_of_couleur_l = switch_coul_l "purple" string_of_couleur

let neg_coul = switch_coul Blue Red
