let rotate a =
  let n = Array.length a in 
  if n = 0 then ()
  else
    begin
      let v = a.(0) in
      for i = 0 to n-2 do
        a.(i) <- a.(i+1)
      done;
      a.(n-1)<-v
    end;;

let rotate_by a n =
	let m = 
		if n < 0 
		then (Array.length a) + n
		else n
	in
  for i = 1 to m do
    rotate a
  done;;

