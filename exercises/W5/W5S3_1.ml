let is_multiple i x = i mod x = 0;;

let output_multiples x n m =
  for i=n to m do
		if is_multiple i x then
	   (print_int i;print_string ",")
	  else ()
done;;

exception Zero;;

let display_sign_until_zero f m =
	try
		for i=0 to m do
			let fi = f i in
			if fi = 0 then
				raise Zero
		  else if fi < 0 then
		    (print_string "negative";print_newline())
		  else 
		    (print_string "positive";print_newline())
		done
	with
	| Zero -> print_string "zero";;

