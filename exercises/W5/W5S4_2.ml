type stack = int array;;
exception Full;;
exception Empty;;

let create size =
  Array.init (size + 1) 
    (function i -> 0);;

let push buf elt =
  let n = buf.(0) + 1 in
  if Array.length buf = n then raise Full
  else 
    (buf.(0) <- n;buf.(n) <- elt);;

let append buf arr =
	for i=(Array.length arr) - 1 downto 0 do
  	push buf arr.(i)
  done;;

let pop buf =
  let i = buf.(0) in
  let n = buf.(i) in
  if i = 0 then raise Empty
  else (buf.(i)<-0;buf.(0)<-i-1;n);;
