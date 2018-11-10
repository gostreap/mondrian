type couleur = Red | Blue
type couleur_l = Purple | C of couleur
val switch_coul_l : 'a -> (couleur -> 'a) -> couleur_l -> 'a
val switch_coul : 'a -> 'a -> couleur -> 'a
val get_rgb : couleur -> Graphics.color
val get_rgb_l : couleur_l -> Graphics.color
val string_of_couleur : couleur -> string
val string_of_couleur_l : couleur_l -> string
val neg_coul : couleur -> couleur
