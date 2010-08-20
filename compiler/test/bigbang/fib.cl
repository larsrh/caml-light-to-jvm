let rec fib = function
	0 -> (0,1)
|	n -> let (l1, l2) = (fib (n-1)) in
		(l2, l1+l2);;

let f = function x ->
	let (y, z) = (fib x) in y;;

f 3
