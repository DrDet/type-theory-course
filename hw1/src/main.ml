open Tree;;
open Printf;;
open Buffer;;

let string_to_tree s =
	let (>>) x f = f x in
	s >> Lexing.from_string >> Parser.main Lexer.main
;;

let rec tree_to_string t = match t with
	| Appl(a, b) -> let lhs = tree_to_string a in
					let rhs = tree_to_string b in
					"(" ^ lhs ^ " " ^ rhs ^ ")"
	| Abstr(Var(s), body) -> let body_str = tree_to_string body in
							"(\\" ^ s ^ "." ^ body_str ^ ")"
	| Var(s) -> s
;;

let rec read_in_string buf =
	try
		let s = input_line stdin in
		add_string buf s;
		add_string buf " ";
		read_in_string buf
	with eof ->
		contents buf
;;

let s = read_in_string (Buffer.create 2000000) in
let t = string_to_tree s in
let formatted = tree_to_string t in
fprintf stdout "%s\n" formatted
;;
