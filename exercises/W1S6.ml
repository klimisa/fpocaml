let rec gcd n m =
  if m = 0 then
    n
  else if n > m then
    gcd (n-m) m
  else
    gcd n (m-n);;

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) 
  in n <> 1 && is_not_divisor 2;;


let multiple_upto n r =
  let n = abs n in		
		let rec is_divisor d = 
    if (n mod d) = 0 
    then true 
    else 
      (d < r && is_divisor (d+1)) 
    in r <> 1 && is_divisor 2;;
