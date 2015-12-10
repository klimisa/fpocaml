type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

exception Unbalanced of int ;;

let rec height = function
  | Empty -> (0, 1)
  | Node (t, _, t') -> 
  		let (h, c) = height t and (h', c') = height t' in
  			(1 + max h h', c + c') ;;

let rec balanced = function
  | Empty -> (true, 1)
  | Node (t, _, t') ->
      let (b, c) = balanced t in
      if b then
        let (b', c') = balanced t' in
        if b' then	
          let (h, ch) = height t and (h', ch') = height t' in
          (b && b' && (h = h'), c+c'+ch+ch')
        else
          (b && b', c+c')
      else
        (b, c);;
        
let bal_height bst =
  let rec height n = function
    | Empty -> (0, 1)
    | Node (t, _, t') -> 
        let (h, c) = height (n+1) t and (h', c') = height (n+1) t' in
        try
         let r = (1 + max h h', c + c') in
          if h = h' then
							r            
          else 
            raise (Unbalanced n)
        with	
        | Unbalanced a -> (0, a+1) 
  in height 0 bst;;

bal_height
  (Node 
  		(Node 
  			(Node 
  				(Empty, -2, Empty), 1, Node (Empty, -1, Empty)), 
  	-1,
    Node 
    	(Node 
    		(Empty, 0, Empty), 
    		0, 
    	Node 
    		(Empty, -3, Empty))))
    
let balanced_fast bst =
  "Replace this string with your implementation." ;;

