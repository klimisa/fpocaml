type index = Index of int ;;

let read a index = 
	match index with
  | Index i -> a.(i);;

let inside a index =
	match index with
  | Index i -> (i >= 0 && i < (Array.length a)) ;;

let next index =
  	match index with
  | Index i -> Index (i + 1) ;;

let min_index a =
		  let array_length = (Array.length a) in
				let rec sort_array index minimal_index =
					if index = array_length then Index minimal_index
					else if a.(index) < a.(minimal_index) 
					then sort_array (index+1) index
					else sort_array (index+1) minimal_index
			in sort_array 0 0;;

