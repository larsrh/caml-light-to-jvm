let data = { field1 = { a = 3; b = 5 }; field2 = 77; field3 = fun x -> x / 7; field4 = (1,2,3) };;

(data.field3) (data.field2) - (data.field1.a * (data.field4 @ 2) - data.field1.b)
(* EXPECTED RESULT 77/7 - (3 * 3 - 5) = 7 *)