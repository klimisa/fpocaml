let is_sorted a =
    let rec loop i =
      if i >= Array.length a - 1 then true
			else String.compare a.(i) a.(i+1) < 0 && loop (i+1)
    in loop 0;;

let find dict word =
  let key_not_found = -1 in
  let initial_min = 0 and initial_max = (Array.length dict) - 1 in
  let rec binary_search dict key imin imax =
    if imax < imin then
      key_not_found
    else
      let imid = imin + ((imax - imin) / 2) in
      let imid_word = dict.(imid) in      
      let 
        key_is_in_lower_subset = String.compare imid_word key > 0 and
        key_is_in_upper_subset = String.compare imid_word key < 0 
      in      
      if (key_is_in_lower_subset) then
        binary_search dict key imin (imid - 1)
      else if (key_is_in_upper_subset) then
        binary_search dict key (imid + 1) imax
      else
        imid
  in binary_search dict word initial_min initial_max;;
