type couleur = Red | Green | Blue
type couleur_l = Purple | Yellow | Cyan | White | C of couleur
val switch_coul_l :
  'a -> 'a -> 'a -> 'a -> (couleur -> 'a) -> couleur_l -> 'a
val switch_coul : 'a -> 'a -> 'a -> couleur -> 'a
val get_rgb : couleur -> Graphics.color
val get_rgb_l : couleur_l -> Graphics.color
val string_of_couleur : couleur -> string
val string_of_couleur_l : couleur_l -> string
val next_coul : couleur -> couleur
