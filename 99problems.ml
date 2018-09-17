(* 1 *)
let rec last lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | hd :: tl -> last tl;;

(* 2 *)
let rec penultimate lst =
  match lst with
  | [] -> None
  | [x;y] -> Some (x,y)
  | hd :: tl -> penultimate tl;;

(* 3 *)
let rec find_pos ind lst =
  match lst with
  | [] | [_] -> None
  | hd :: tl -> if ind = 1 then Some hd else find_pos (ind-1) tl;;

(* 4 *)
let length lst =
  let rec lng ind l =
    match l with
    | [] -> ind
    | hd :: tl -> lng (ind+1) tl in lng;;