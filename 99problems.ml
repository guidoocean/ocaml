(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | hd :: tl -> last tl;;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec penultimate lst =
  match lst with
  | [] -> None
  | [x;y] -> Some (x,y)
  | hd :: tl -> penultimate tl;;

(* 3. Find the k'th element of a list. (easy) *)
let rec find_pos ind lst =
  match lst with
  | [] | [_] -> None
  | hd :: tl -> if ind = 1 then Some hd else find_pos (ind-1) tl;;

(* 4. Find the number of elements of a list. (easy) *)
let length lst =
  let rec lng ind l =
    match l with
    | [] -> ind
    | hd :: tl -> lng (ind+1) tl in lng 0 lst;;

(* 5. Reverse a list. (easy) *)
let reverse lst =
  let rec rvr_helper l tl =
    match tl with
    | [] -> l
    | hd :: tl -> rvr_helper (hd::l) tl in rvr_helper [] lst;;

(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome lst =
  lst = reverse lst;;

(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten lst =
  let rec flat res l =
    match l with
    | [] -> res
    | One hd :: tl -> flat (res@[hd]) tl
    | Many hd :: tl -> flat (flat res hd) tl in flat [] lst;;

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let compress lst =
  let rec cmp l prev res =
    match l with
    | [] -> res
    | hd :: tl -> if hd = prev then cmp tl hd res else cmp tl hd (res@[hd]) in cmp lst "" [];;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack lst = 
  let rec pck curr l = function
    | [] -> []
    | [x] -> (x::curr) :: l
    | hd :: (tl :: _ as t) -> if hd = tl then pck (hd :: curr) l t
                              else pck [] ((hd :: curr) :: l)  t in reverse (pck [] [] lst);;


(* 10. Run-length encoding of a list. (easy) *)
let encode lst =
  let rec enc counter l = function
    | [] -> []
    | [x] -> l@[(counter + 1, x)]
    | hd :: (tl :: _ as t) -> if hd = tl then enc (counter + 1) l t
                              else enc 0 (l@[(counter + 1, hd)]) t in enc 0 [] lst;;

(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode_v2 lst = 
  let enc_tuple counter element =
    if counter = 1 then One element else Many (counter, element) in
  let rec enc_logic counter tup lstt =
    match lstt with
    | [] -> []
    | [item] -> tup@[(enc_tuple (counter+1) item)]
    | hd :: (prev :: _ as tl) -> if hd = prev then enc_logic (counter + 1) tup tl
                                 else enc_logic 0 ( tup@[(enc_tuple (counter + 1) hd)] ) tl in enc_logic 0 [] lst;;

(* 12. Decode a run-length encoded list. (medium) *)
let decode lst = ;; (* In Progress *)

(* 13. Run-length encoding of a list (direct solution). (medium) *)

(* 14. Duplicate the elements of a list. (easy) *)
let rec duplicate lst = 
  match lst with
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl;;

(* 15. Replicate the elements of a list a given number of times. (medium) *)
let replicate lst count =
  match lst with
  | [] -> []
  | 

(* 33. Determine whether two positive integer numbers are coprime. (easy) *)
let is_prime n = 
  if n / 2 = ??? then true else false;; (* Think how detect is result is float *)
