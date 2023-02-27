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
  | x :: y :: xs -> 
    if x = y then compress (y :: xs) 
    else x :: compress (y :: xs);;
   
(*Q3*)
let rec remove_if lst predicate = 
  match lst with 
  | [] -> []
  | x :: xs -> if predicate x then remove_if xs predicate
  else x :: remove_if xs predicate;;

(*Q4*)
let rec slice lst i j = 
  let rec sublist lst i j k list = 
    match lst with 
    | [] -> list
    | x :: xs -> if i > k then sublist xs i j (k + 1) list 
    else if j <= k then list
    else sublist xs i j (k + 1) (list@[x])
  in sublist lst i j 0 [];;

(*Q5*)


(*Q6*)
