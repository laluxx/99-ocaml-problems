(* let () = print_endline "Hello, World!" *)

(* problem 01 *)
let rec last : 'a list -> 'a option = function
| [] -> None
| [x] -> Some x
| _ :: rest -> last rest


(* problem 02 *)
let rec last_two : 'a list -> ('a * 'a) option = function
| [] -> None
| [x] -> None
| [x; y] -> Some (x, y)
| _ :: rest -> last_two rest



(* problem 03 *)
let rec at idx lst =
  match lst with
  | [] -> None
  | head :: tail ->
    if idx = 1 then
      Some head
    else
      at (idx - 1) tail



(* problem 04 tail recursive btw *)
let length lst =
  let rec aux acc = function
    | [] -> acc
    | _ :: rest -> aux (acc + 1) rest
  in
  aux 0 lst



(* problem 05 *)
let reverse lst =
  let rec aux current_list accumulator =
    match current_list with
    | [] -> accumulator
    | head :: tail -> aux tail (head :: accumulator)
  in
  aux lst []



(* problem 06 *)
let is_palindrome lst =
  lst = reverse lst
