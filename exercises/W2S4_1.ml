let min a =
  let array_length = (Array.length a) in
		let rec sort_array index minimal =
    if index = array_length then minimal
    else if a.(index) < minimal 
    then sort_array (index+1) a.(index)
    else sort_array (index+1) minimal
  in sort_array 0 Array.(0);;
 
let min_index a =
  let array_length = (Array.length a) in
		let rec sort_array index minimal_index =
    if index = array_length then minimal_index
    else if a.(index) < a.(minimal_index) 
    then sort_array (index+1) index
    else sort_array (index+1) minimal_index
  in sort_array 0 0;;

let it_scales =
  "no" ;;
