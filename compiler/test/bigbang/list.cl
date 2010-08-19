let hd = fun x::_ -> x;;

let tl = fun _::xs -> xs;;

let rec append = fun
    [] ys       -> ys
|   x::xs ys    -> x :: append xs ys;;

let rec zip = fun
    [] []       -> []
|   x::xs y::ys -> (x,y) :: zip xs ys;;

let rec map = fun
    f []    -> []
|   f x::xs -> f x :: map f xs;;

let rec filter = fun
    p []    -> []
|   p x::xs -> if p x then x :: filter p xs else filter p xs;;

let rec foldl = fun
    f acc []    -> acc
|	f acc x::xs	-> foldl f (f acc x) xs;;

let rec foldr = fun
    f acc []    -> acc
|   f acc x::xs -> f x (foldr f acc xs);;

let revert = foldl (fun xs x -> x::xs) [];;

hd (revert [3;2;1])
