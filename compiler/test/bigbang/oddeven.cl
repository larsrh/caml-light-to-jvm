(* the classical example for mutual recursion *)

let rec
		odd = function x -> 
				(match x with 
						0		-> false
				|		_		-> (even (x - 1)))
and
		even = function y -> 
				(match y with 
						0		-> true
				|		_		-> (odd (y - 1)))
in
		((even 4) & not (odd 0))

(* EXPECTED RESULT true (or 1) *)