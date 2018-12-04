type couleur = [ `Blue | `Green | `Red ]
type 'a couleur_l = Purple | Yellow | Cyan | White | C of 'a
val switch_coul_l : 'a -> 'a -> 'a -> 'a -> ('b -> 'a) -> 'b couleur_l -> 'a
val switch_coul : 'a -> 'a -> 'a -> [<`Red | `Green | `Blue] -> 'a
val switch_coul2 : 'a -> 'a -> [< `Blue | `Red ] -> 'a
val get_rgb : [< `Blue | `Green | `Red ] -> Graphics.color
val get_rgb_l : [< `Blue | `Green | `Red ] couleur_l -> Graphics.color
val string_of_couleur : [< `Blue | `Green | `Red ] -> string
val string_of_couleur_l : [< `Blue | `Green | `Red ] couleur_l -> string
val next_coul : couleur option -> couleur
val next_coul2 : [`Blue | `Red ] option -> [`Blue | `Red ]
