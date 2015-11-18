let is_sorted a =
  let array_length = (Array.length a) in
  if array_length = 0 || array_length = 1 then true
  else
    let is_asc s s' = (String.compare s s' < 0) in
    let rec loop current_idx next_idx =
      if next_idx < array_length - 1 then
        if is_asc a.(current_idx) a.(next_idx) then
          loop (current_idx + 1) (next_idx + 1)
        else false
      else
        is_asc a.(current_idx) a.(next_idx)
    in loop 0 1;;

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
