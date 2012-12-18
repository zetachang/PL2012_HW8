# use "calcParser.ml";;
# use "calcLexer.ml";;
# load "str.cma";;

(* type expr = X | Y | Int of int | Add of expr * expr | Mul of expr * expr | Pow of expr * expr *)

type term = Group of int * int * int;;

let parse s = expr1 lex (Lexing.from_string s);;

let multiply la lb =
let mul (Group (a, b, c))  (Group (d, e, f))  = Group (a*d , b+e, c+f) in
	(List.fold_right (fun a b ->
		List.append b (List.map (fun c -> (mul a c)) lb)
	) la []);;

let rec eval exp = match exp with
	X -> [Group(1, 1, 0)]
	| Y -> [Group(1, 0, 1)]
	| Int (n) -> [Group(n, 0, 0)]
	| Add (exp_a, exp_b) -> List.append (eval exp_a) (eval exp_b)
	| Pow (var, Int(num)) -> [Group(1, (if var = X then num else 0), (if var = Y then num else 0))]
	| Mul (exp_a, exp_b) -> multiply (eval exp_a) (eval exp_b)
	| _ -> [Group(0, 0, 0)];;
	
let rec eval_numeric_terms p = 
	if p = [] then 0
	else (
		match List.hd p with
		Group(n, 0, 0) -> n + (eval_numeric_terms (List.tl p))
		| _ -> (eval_numeric_terms (List.tl p))
	)


let find_max_x p =
	let max = ref 0 in (
		List.iter (fun (Group (a, b, c)) -> 
			if b > !max then max := b;
		) p;
		!max;
	);;
	
let encode_ele e =
 	let print_x (Group (_, b, _)) = (
		match b with
		0 -> "" 
		| 1 -> "x"
		| n -> Printf.sprintf "x^%d" n
	) in 
	let print_y (Group (_, _, c)) = (
		match c with
		0 -> ""
		| 1 -> "y"
		| n -> Printf.sprintf "y^%d" n
	) in 
	match e with 
	Group (n, 0, 0) -> (string_of_int n)
	| Group (1 ,b, c) -> (
		(print_x e) ^ (print_y e)
	) | Group (n, b, c) -> (
		(string_of_int n) ^ (print_x e) ^ (print_y e)
	);;
	
let rec remove_zero_term p =
	if p = [] then []
	else (
		match (List.hd p) with
		Group(0, _, _) -> remove_zero_term (List.tl p)
		| _ -> (List.hd p)::remove_zero_term (List.tl p)
	);;
			
let encode_poly_by_y p =
	let sorted = List.sort (fun (Group (a,b,c)) (Group(d,e,f)) -> (f-c)) p in
	let vb_terms = List.filter (fun (Group(a,b,c)) -> b != 0 || c != 0) sorted in
	let num_terms = List.filter (fun (Group(a,b,c)) -> b == 0 && c == 0) sorted in 
	let numeric_v = eval_numeric_terms num_terms in 
	let encoded = List.map (fun a -> (encode_ele a)) vb_terms in
		if numeric_v = 0 then (String.concat " + " encoded)
		else (String.concat "+" (List.append encoded [string_of_int(numeric_v)]));;

let concat_term a b =
	if  Str.string_match (Str.regexp "[^+]+\\+") a 0 then (
		if b = "1" then "(" ^ a ^ ")" 
		else "(" ^ a ^ ")" ^ b
	)
	else (
		if b = "1" then a 
		else (
			if a = "1" then b
			else a ^ b
		)
	);;

let rec simplify n p = 
	if n = -1 then []
	else (
		let seg = List.filter (fun (Group (a,b,c)) -> (b = n)) p in
		let removed = List.map (fun (Group (a,b,c)) -> Group (a,0,c)) seg in
			if (List.length removed) = 0 then (simplify (n-1) p)
			else ( 
				(concat_term (encode_poly_by_y removed) (encode_ele (Group (1, n, 0)))) :: (simplify (n-1) p)
			)	
	);;
	
let rec remove_white_space n s =
    if n = (String.length s) then ""
    else (
        if s.[n] = ' ' then (remove_white_space (n+1) s)
        else (Char.escaped s.[n]) ^ (remove_white_space (n+1) s)
    );;

let rec add_symbol n s =
let is_number c = Str.string_match (Str.regexp "[0-9]") (Char.escaped c) 0 in
    if n = (String.length s) then (String.sub s (n-1) 1)
    else (
    if( (s.[n-1] = ')' || s.[n-1] = 'x' || s.[n-1] = 'y' || (is_number s.[n-1]) ) && 
        (s.[n] = '(' || s.[n] = 'x' || s.[n] = 'y' || (is_number s.[n]) ) && 
        not ( (is_number s.[n-1]) && (is_number s.[n]) )
    ) then ((Char.escaped s.[n-1]) ^ "*" ^ (add_symbol (n+1) s))
    else (Char.escaped s.[n-1]) ^ (add_symbol (n+1) s)
);;

let get_ans s =
	let removed_white_space = remove_white_space 0 s in
	let added_times_symbol = add_symbol 1 removed_white_space in
	let parsed_tree = parse added_times_symbol in
	let evaluated_terms = eval parsed_tree in
	let zero_term_removed = remove_zero_term evaluated_terms in 
	let max_power_of_x = find_max_x zero_term_removed in
	let simplified_string_list = simplify max_power_of_x zero_term_removed  in
		String.concat " + " simplified_string_list;;


print_endline (get_ans "2x + 0 + (1+0)y");;
print_endline (get_ans "x + y + 2*3 + 2x + x^2 + y^2");;
print_endline (get_ans "(2x + 1)y + x + x^2");;
print_endline (get_ans "1 + 2yx + x^2 + x^2*y + x^3*2y + y");;