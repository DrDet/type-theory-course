type tree = Abstruction of string * tree | Application of tree * tree | Variable of string;;

type de_bruijn_tree = Abstr of de_bruijn_tree | Appl of de_bruijn_tree * de_bruijn_tree | BoundVar of int | FreeVar of string;;
