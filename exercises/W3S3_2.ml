type trie = Trie of int option * char_to_children and char_to_children = (char * trie) list;;

let empty =
  Trie (None, []);;

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let children_from_char m c =
  let rec cfc l =
    match l with 
    | [] -> None
    | (key, t)::xs ->
    		match (xs, t)
        if ch = c then 
          Some t
        else cfc xs
    | 
  in cfc m;;


let update_children m c t =
	let e = (c,t) in
	let rec update_helper accu l = 
		match l with
		| [] -> List.rev accu
		| x::xs -> 
			let (ch, t) = x in
				if ch = c then 
					update_helper (e :: accu) xs
				else 
					update_helper xaccu) xs
	in update_helper [] m;;

let lookup trie w =
  "Replace this string with your implementation." ;;

let insert trie w v =
  "Replace this string with your implementation." ;;

