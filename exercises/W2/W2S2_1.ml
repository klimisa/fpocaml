let exchange k = ((k mod 10) * 10)  + (k / 10);;

let is_valid_answer (grand_father_age, grand_son_age) =
  ((grand_son_age * 4) = grand_father_age 
   && (exchange ((exchange grand_father_age) * 3)) = grand_son_age);;

let find answer = 
		let not_found = (-1,-1) in
		let rec find_anwser (max_gfa, gsa) =
    let gfa = gsa * 4 in
		  if gfa >= max_gfa 
		  then not_found 
		  else 
				if is_valid_answer (gfa, gsa) 
				then	(gfa, gsa) 
				else find_anwser (max_gfa, gsa + 1)
  in find_anwser answer;;
