(* complex list match test *)

let rec
		list = function n ->
				(match n with 
						0		-> []
				|		_		-> (n :: (list (n-1))))
in
		(match list 5 with 
				[]						 -> 42 
		|		n1::n2::n3::[] -> n2
		|		n1::n2				 -> 
						(match n2 with 
								[]			-> 42
						|		n1::n2	-> n1))

(* EXPECTED RESULT 4 *)