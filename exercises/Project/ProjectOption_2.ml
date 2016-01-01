type ltable = (string * string list) list;;
type distribution =
  { total : int ;
    amounts : (string * int) list };;
type htable = (string, distribution) Hashtbl.t;;
type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t };;

let simple_0 =
  "I am a man and my dog is a good dog and a good dog makes a good man";;
  
(* -- Part A -------------------------------------------------------------- *)

let words str =
  let buf = Buffer.create 4096 in
  let rec loop acc i =
    let s =  (Buffer.contents buf) in
    if i >= String.length str 
  		then 
      List.rev (s::acc)
    else
      let c = String.get str i in
      match c  with
        'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        -> Buffer.add_char buf c; loop acc (i+1);
      | _ -> Buffer.clear buf; loop (s::acc) (i+1)
  in loop [] 0;;

let build_ltable words =
  let rec helper acc i =
    if i >= List.length words then acc
    else
      let w = (List.nth words i) in
      let v = 
        try List.assoc w acc
        with Not_found -> [] in
      let e = 	
        if i >= List.length words - 1 then
          (w, List.rev ("STOP"::v))
        else
          (w, (List.nth words (i+1))::v) in
      let acc = e::(List.filter (function k,_ -> k <> w) acc) in
      let acc = if i = 0 then ("START", w::[])::acc else acc
      in helper acc (i+1)      
  in helper []  0;;

let next_in_ltable table word =
  let l = List.assoc word table in
  List.nth l (Random.int (List.length l));;

let walk_ltable table =
  let rec loop acc w =
    if w = "STOP" then List.rev acc
    else
      loop (w::acc) (next_in_ltable table w)
  in loop [] (next_in_ltable table "START");;


(* -- Part B -------------------------------------------------------------- *)

let compute_distribution l =
  "Replace this string with your implementation." ;;

let build_htable words =
  "Replace this string with your implementation." ;;

let next_in_htable table word =
  "Replace this string with your implementation." ;;

let walk table =
  "Replace this string with your implementation." ;;

let walk_htable table =
  "Replace this string with your implementation." ;;

(* -- Part C -------------------------------------------------------------- *)

let sentences str =
  "Replace this string with your implementation." ;;

let rec start pl =
  "Replace this string with your implementation." ;;

let shift l x =
  "Replace this string with your implementation." ;;

let build_ptable words pl =
  "Replace this string with your implementation." ;;

let walk_ptable { table ; prefix_length = pl } =
  "Replace this string with your implementation." ;;

let merge_ptables tl =
  "Replace this string with your implementation." ;;

