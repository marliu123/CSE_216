let double x = 2*x;;
let square x = x*x;;
let twice f x = f (f x);;
let quad = twice double;;
let fourth = twice square;;
let tripleFloat x = 3.0*.x;;
let thrice f x = f(f(f(x)));;
let composition f g x = f(g(x));;
let div x y = x/y;;
let triple3 = thrice tripleFloat;;

let rec repeat f n x = match n with 
| 0 -> x
| _ -> repeat f (n - 1) (f x);;


let f list =
let rec aux acc = function
  | [] -> acc
  | h::t -> aux (h::acc) t in aux [] list;;



