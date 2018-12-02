type var = Var of string;;

type tree = Abstr of var * tree | Appl of tree * tree | Var of string;;
