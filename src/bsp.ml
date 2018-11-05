type label =
  { coord : int;
    colored : bool;
  }
  
type bsp = R of Graphics.color option
         | L of label * bsp * bsp
