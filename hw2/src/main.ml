open Tree;;
open Printf;;
open Buffer;;

module Int = struct type t = int let compare = compare end;;
module IntMap = Map.Make(Int);;
module StringMap = Map.Make(String);;

let string_to_tree s =
	let (>>) x f = f x in
	s >> Lexing.from_string >> Parser.main Lexer.main
;;

let rec tree_to_string t = match t with
	| Application(a, b) -> let lhs = tree_to_string a in
					let rhs = tree_to_string b in
					"(" ^ lhs ^ " " ^ rhs ^ ")"
	| Abstruction(s, body) -> let body_str = tree_to_string body in
							"(\\" ^ s ^ "." ^ body_str ^ ")"
	| Variable(s) -> s
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

let to_de_brujin_tree t =
let rec impl t nesting bound_vars = match t with
	| Variable(s) -> if StringMap.mem s bound_vars
				then BoundVar(nesting - (StringMap.find s bound_vars))
				else FreeVar(s)
	| Application(a, b) -> Appl(impl a nesting bound_vars, impl b nesting bound_vars)
	| Abstruction(s, a) -> Abstr(impl a (nesting + 1) (StringMap.add s nesting bound_vars))
in
impl t 0 StringMap.empty
;;

let to_normal_tree t =
let rec impl t nesting = match t with
	| FreeVar(s) -> Variable(s)
	| BoundVar(n) -> Variable("x'o'" ^ string_of_int (nesting - n))
	| Appl(a, b) -> Application(impl a nesting, impl b nesting)
	| Abstr(a) -> Abstruction("x'o'" ^ string_of_int nesting, impl a (nesting + 1))
in
impl t 0
;;

let rec shift t nesting delta = match t with
	| BoundVar(n) when n > nesting -> BoundVar(n + delta)
	| Appl(a, b) -> Appl(shift a nesting delta, shift b nesting delta)
	| Abstr(a) -> Abstr(shift a (nesting + 1) delta)
	| var -> var
;;

let rec substitute p q nesting = match p with
	| Appl(a, b) -> Appl(substitute a q nesting, substitute b q nesting)
	| Abstr(a) -> Abstr(substitute a q (nesting + 1))
	| BoundVar(n) when n = nesting + 1 -> shift q 0 nesting
	| BoundVar(n) when n > nesting + 1 -> BoundVar(n - 1)
	| var -> var
;;
let rec debr_to_str = function
	| Abstr(a) -> "\\(" ^ debr_to_str a ^ ")"
	| Appl(a, b) -> "(" ^ debr_to_str a ^ " " ^ debr_to_str b ^ ")"
	| BoundVar(n) -> string_of_int n
	| FreeVar(s) -> s
;;

let rec beta_reduce t =
(* fprintf stdout "%s\n" (debr_to_str t);  *)
match t with
	| Appl(Abstr(p), q) -> (substitute p q 0, true)
	| Appl(a, b) -> begin
						let (left, l_changed) = beta_reduce a in
						if l_changed then
							(Appl(left, b), true)
						else
							let (right, r_changed) = beta_reduce b in
							(Appl(a, right), r_changed)
					end
	| Abstr(a) -> let (a', a_changed) = beta_reduce a in
				  (Abstr(a'), a_changed)
	| var -> (var, false)
;;

let normalize t' =
	let changed = ref true in
	let t = ref t' in
	while (!changed) do
		let (res_t, ch) = beta_reduce !t in
		t := res_t;
		changed := ch
	done;
	!t
;;


let s = read_in_string (Buffer.create 2000000) in
let t' = string_to_tree s in
let t'' = to_de_brujin_tree t' in
let reduced = normalize t'' in
let t = to_normal_tree reduced in
let formatted = tree_to_string t in
fprintf stdout "%s\n" formatted
;;
