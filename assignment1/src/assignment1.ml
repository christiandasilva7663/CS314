let rec rev l = match l with
    [ ] -> [ ]
   | (h::t) -> (rev t) @ [h]

(********************)
(* Problem 1: range *)
(********************)

let rec range num1 num2 =
  if num1 > num2 then []
  else num1 :: range (num1 + 1) num2;;
  (*if num2 < num1 then let numlist = [] else
  if num2 > num1 then numlist :: num2 
  range num1 (num2 - 1) 
  else numlist :: num2*)
  (*[2;3;4;5]*)

(**********************)
(* Problem 2: flatten *)
(**********************)

let rec flatten l =
 match l with 
  | [] -> [] 
  | h::t -> h @ flatten t;;
  
  (*rev [4;3;2;1]*)
  
(*****************************)
(* Problem 3: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  (*let Clist = 
  match l with [[]] -> Clist | (a :: b :: _) -> Clist :: a @ b 
  (remove_stutter l)*)
  (*[]*)
(*let f =
  match l with
  | [] -> []
  | h::t::m -> if h = t then remove_stutter m 
  else
  f::remove_stutter m
  in rev f
  *)
  match l with 
  | [] -> []
  | h :: [] -> h :: []
  | h :: m :: t -> if h = m then  remove_stutter(m :: t) else h :: remove_stutter(m :: t)
  

  
(*******************)
(* Problem 4: sets *)
(*******************)
(*let rec setSetup l = 
  match l with ->
    [] -> []
    | h ::
*)
let rec sort l =
  match l with
    [] -> []
  | h :: t -> sortHelper h (sort t)
and sortHelper x l =
  match l with
    [] -> [x]
  | h :: t -> if x <= h 
  then x :: l 
  else h :: sortHelper x t;;

  let rec elem x a =
  match a with
  | [] -> false
  | h::t -> if h = x then true else (elem x t)

let rec subset a b =
  match a with
  | [] -> true
  | h::t -> if (elem h b) then (subset t b) else false

let rec eq a b =
  (subset a b) && (subset b a)

let rec remove x a =
  match a with 
  | [] -> []
  | h :: t -> if h = x then  remove x  t else h :: remove x t 

let rec union a b =
  let l = [a ; b] in remove_stutter(sort((flatten l)))

let rec diff a b =
  let a = remove_stutter(sort a) in
  match b with
  | [] -> a
  | h :: t -> if (elem h a) then let a = remove h a in diff a t else diff a t ;; 

(*****************************************************)
(* Problem 5: Digital Roots and Additive Persistence *)
(*****************************************************)

(* digitsOfInt : int -> int list
 * we assume n >= 0
 * (digitsOfInt n) is the list of digits of n in the order in which they appear in n
 * e.g. (digitsOfInt 31243) is [3;1;2;4;3]
 *)

let rec digitsOfInt n =
  let rec digitsOfIntHelper l n = 
    if n < 10 then n :: l 
    else digitsOfIntHelper((n mod 10) :: l) (n / 10) 
  in digitsOfIntHelper [] n;;
    
  (*let l = [] in
  let rec helper n l = match l with 
  [] -> []
  | h::t -> if n < 10 then (n mod 10) :: t else helper (n/10) (n mod 10 :: t) 
  in rev (helper 0 l)*)
  (*let l = [] in
  let rec digitsOfIntHelper n l = *)
  (*if n < 10 then let l = (n mod 10 :: l) else n mod 10 :: l
  *) 
  (*[]*)
  (*use let in at the very end and make a rev function because list will be reversed*)

(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

let rec sumList l = 
  match l with
  [] -> 0
  | h::t -> h + sumList(t)

let rec sumInt n = 
  if n = 0 then 0
  else 
  (n mod 10) + sumInt(n/10)

let additivePersistence n = 
  (*let l = digitsOfInt(n) in*)
  let rec additivePersistenceHelper n = 
    if n < 10 then 0 
    else let counter = sumInt(n) in
    if counter > 9 then 1 + additivePersistenceHelper counter
    else 1
  in additivePersistenceHelper n;; 
 
 
 
  (* let rec additivePersistenceHelper l n = 
    if n < 10 then 0 
    else additivePersistenceHelper(digitsOfInt(sumList(l)) sumList(n) ) 
  in additivePersistenceHelper digitsOfInt(n) n;; 
  *)
  (*(-1)*)

let digitalRoot n =
  let rec digitalRootHelper n = 
    if n < 10 then sumInt n
    else let n = sumInt(n) in digitalRootHelper n
  in digitalRootHelper n

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of elem *)
  let _ =
    try
      assert (elem 3 [] = false);
      assert (elem 5 [2;3;5;7;9] = true);
      assert (elem 4 [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of subset *)
  let _ =
    try
      assert (subset [5] [2;3;5;7;9] = true);
      assert (subset [5;3] [2;3;5;7;9] = true);
      assert (subset [5;4] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of eq *)
  let _ =
    try
      assert (eq [5;3;2;9;7] [2;3;5;7;9] = true);
      assert (eq [2;3;7;9] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove *)
  let _ =
    try
      assert (eq (remove 5 []) []);
      assert (eq (remove 5 [2;3;5;7;9]) [2;3;9;7]);
      assert (eq (remove 4 [2;3;5;7;9]) [2;3;5;9;7]);
      assert (eq (remove 9 [2;3;5;7;9]) [2;5;3;7]);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for union *)
  let _ =
    try
      assert (eq (union [2;3;5] []) [2;3;5]);
      assert (eq (union [5;2] [3;7;9]) [2;3;5;9;7]);
      assert (eq (union [2;3;9] [2;7;9]) [2;3;9;7]);
      assert (eq (union [] [2;7;9]) [2;9;7])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for diff *)
  let _ =
    try
      assert (eq (diff [1;3;2] [2;3]) [1]);
      assert (eq (diff ['a';'b';'c';'d'] ['a';'e';'i';'o';'u']) ['b';'c';'d']);
      assert (eq (diff ["hello";"ocaml"] ["hi";"python"]) ["hello";"ocaml"]);
      assert (eq (diff ["hi";"ocaml"] ["hello";"ocaml"]) ["hi"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9876 = 2)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()
