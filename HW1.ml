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
let rec equivs e lst =
  let rec equiv_group x ac =
    match ac with
    | [] -> [[x]]
    | hd::tl ->
        if e x (List.hd hd) then (x::hd)::tl
        else hd :: equiv_group x tl in
  let rec group lst ac =
    match lst with
    | [] -> ac
    | hd::tl -> group tl (equiv_group hd ac) 
    in group lst [];;

(*Q6*)

let prime n =
  match n with
  | 1 -> false
  | 2 -> true
  | _ -> let rec not_divisible x = 
    x * x > n || (n mod x <> 0 && not_divisible (x + 1))
    in not_divisible 2;;

let rec goldbachpair x = 
  let rec comp y = 
    if prime y && prime (x - y) then ( y, x - y)
    else comp (y + 1)
  in comp 2;;

(*Q7*)

let rec identical_on lst f g = 
  let rec same lst = 
    match lst with 
    | [] -> true
    | x :: xs -> if f x = g x then same xs
    else false
  in same lst;;
  
(*Q8*)

let rec pairwisefilter cmp lst = 
  match lst with 
  | [] -> []
  | [x] -> [x]
  | x :: y :: xs -> cmp (x, y) :: pairwisefilter cmp xs;;

(*Q9*)
(*uses the pow function from Q1*)
let rec polynomial lst n =  
  match lst with 
  | [] -> 0
  | (x, y) :: xs -> x * (pow n y) + polynomial xs n;;

(*Q10*)

let rec fold_left y ac lst =
  match lst with
  | [] -> ac
  | x :: xs -> fold_left y (y ac x) xs;;

let rec powerset lst =
  match lst with
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      fold_left (fun ac set -> (x :: set) :: set :: ac) [] ps;;
