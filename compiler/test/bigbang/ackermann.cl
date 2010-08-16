(* The infamous ackermann-function *)

let rec 
		a = function m -> function n ->
				(match m with
						0		->	(n + 1)
				|		_		-> 
						(match n with
								0		->	(a (m - 1) 1)
						|		_		->	(a (m - 1) (a m (n-1)))))
in
		(a 1 2)

(* EXPECTED RESULT 4 *)