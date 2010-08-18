begin
let rec
		fac = function n ->
				if n == 0 
				then 1 
				else n * (fac (n - 1))
in
        print_int (fac 5);
        print_newline()
end
;;
