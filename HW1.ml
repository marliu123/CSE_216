(*Q1*)
let rec pow x y =
  match y with
  | 0 -> 1
  | _ -> x * pow x (y - 1);;

let rec float_pow x y =
  match y with 
  | 0 -> 1.0
  | _ -> x *. float_pow x ( y - 1 );;

(*Q2*)

let rec compress lst = 
  match lst with 
  | [] -> []
  | [x] -> [x]
  | x ::(y :: xs) -> 
    if x = y then compress (y :: xs) 
    else x :: compress (y :: xs);;
   
(*Q3*)

