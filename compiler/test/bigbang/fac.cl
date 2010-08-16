(* A simple factorial program *)

let rec
		fac = function n ->
				if n == 0 
				then 1 
				else n * (fac (n - 1))
in
		(fac 5)

(* EXPECTED RESULT 5! = 1*2*3*4*5 = 120 *)