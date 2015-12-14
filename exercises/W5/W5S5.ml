type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist ;;

let nil () =
  { pointer = Nil } ;;

let cons elt rest =
  { pointer = List (elt, rest) } ;;

exception Empty_xlist ;;

let head l =
   match l.pointer with
   | Nil -> raise Empty_xlist
   | List (x, xl) -> x;;

let tail l =
   match l.pointer with
   | Nil -> raise Empty_xlist
   | List (x, xl) -> xl;;

let add a l =
   match l.pointer with
   | Nil -> l.pointer <- List (a, { pointer = Nil })
   | List (x, xl) -> l.pointer <- List (a, { pointer = l.pointer });;

let chop l =
   match l.pointer with
   | Nil -> raise Empty_xlist
   | List (x, {pointer}) -> l.pointer <- pointer;;

let rec append l l' =
   match l.pointer with
   | Nil -> l.pointer <- l'.pointer
   | List (x, xl) -> append xl l';;

let rec filter p l =
  match l.pointer with
  | Nil -> l.pointer <- Nil
  | List (x, ({pointer} as e)) -> 
      if p x then	
        filter p e
      else 
        (l.pointer <- pointer;filter p l) ;;
