open String;;
open Digest;;
open List;; 

let print_hashes (hashes : Digest.t list) : unit =
  let print_hash h = h |> to_hex |> uppercase |> print_endline in
  iter print_hash hashes;;

