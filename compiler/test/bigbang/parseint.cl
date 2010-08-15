(* parses an integer from a string *)
let rec
		(* char list -> ((int -> int) * char list) *)
		parseminus = function x::xs -> 
						if x == '-' 
						then ((function z -> -z),		 xs) 
						else ((function z ->  z), x::xs)
and
		(* char list -> int -> int *)
		parsepositive = function list -> function acc ->
				(match list with
						[]			-> acc
				|		x::xs		-> (parsepositive xs (10 * acc + (intofchar x))))
and
		(* char -> int *)
		intofchar = function x ->
				(match x with
						'1'			->	1
				|		'2'			->	2
				|		'3'			->	3
				|		'4'			->	4
				|		'5'			->	5
				|		'6'			->	6
				|		'7'			->	7
				|		'8'			->	8
				|		'9'			->	9
				|		'0'			->	0)
in
		let
				(* ((int -> int) * char list) -> int *)
				parsepos = function (sign,list) -> (sign (parsepositive list 0))
		in let 
				(* ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) *)
				o = function f -> function g -> function x -> (f (g x))
		in
				(((o parsepos parseminus) "-42") + ((o parsepos parseminus) "42"))

(* EXPECTED RESULT 0 *)
