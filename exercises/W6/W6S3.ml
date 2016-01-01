module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end;;


module CharHashedType =
struct
  type t = char
  let equal i j = i=j
  let hash i = int_of_char i
end;;

module CharHashtbl = Hashtbl.Make(CharHashedType);;

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () =
    	Trie (None, [])

	let lookup trie w =
		let rec lookup_helper t i =
			let Trie (x,l) = t in 
				if i >= String.length w then x
				else
				  let rec loop = function		  
				    | [] -> None 
				    | (c, t)::xs  ->
				        if c = String.get w i then 
				          lookup_helper t (i+1)
				        else loop xs 
				  in loop l
		in lookup_helper trie 0

	let insert trie w v =
		let rec insert_helper t i =
			let Trie (x,l) = t in 
				if i >= String.length w then x
				else
				  let rec loop = function		  
				    | [] -> Trie (x, l @ (String.get w i, [])) 
				    | (c, t)::xs  ->
				        if c = String.get w i then 
				          insert_helper t (i+1)
				        else loop xs 
				  in loop l
		in insert_helper trie 0

end;;
