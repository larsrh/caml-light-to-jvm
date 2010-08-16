(* Incredible that this really works with our compiler :-) *)

let rec
		(* foldl : ('a -> 'b -> 'a) -> 'a -> 'b list  *)
		foldl = function myfun -> function acc -> function list ->
				(match list with 
						[]			-> acc 
				|		x::xs		-> (foldl myfun (myfun acc x) xs))
and
		(* prod : int -> int -> int *)
		prod = function x -> function y -> x * y
and
		(* list : int -> int -> int list *)
		list = function l -> function h ->
				if h < l
				then []
				else (h :: (list l (h-1)))
in
		(foldl prod 1 (list 3 5))

(* EXPECTED RESULT 3*4*5 = 60 *)