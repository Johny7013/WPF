(*Sortowanie Topologiczne testy*)
open Topol;;

exception Wrong_Ans;; (* Wyjątek wyrzucany w przypadku złej odpowiedzi *)

(* Sprawdza, czy porządek zwrócony przez funkcję topol jest poprawny*)
let test graph lista =
  let hash = Hashtbl.create (List.length lista)
  in
  List.iteri (fun i x -> Hashtbl.add hash x i) lista;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hash v) > (Hashtbl.find hash u)
      then raise Wrong_Ans;) l
  in
  try (List.iter check_one graph; true)
  with Wrong_Ans -> false
;;

(* Sprawdza, czy został zwrócony wyjątek Cykliczne *)
let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true
;;


(* Pusta lista *)
let o = [];;
assert(test o (topol o));;

(* Testy niecykliczne, spójne i niespójne grafy *)
let o = [(1,[2;3])]@o;;
assert(test o (topol o));;

let o = [(2,[3;4])]@o;;
let o = [(3,[4])]@o;;
let o = [(4,[])]@o;;
assert(test o (topol o));;

let o = [("FRANK", [])];;
assert(test o (topol o));;

  
let o = [("FUNT", []); ("FRANK", []); ("FUNT", []); ("FRANK", [])];;
assert(test o (topol o));;

let o = [
  ("do", ["programowania"]);
  ("Wstęp", ["do"]);
  ("programowania", ["funkcyjnego";"imperatywnego"])
];;
assert(test o (topol o));;

let o = [
  (2, [4]);
  (5, []);
  (3, []);
  (4, [5; 6]);
  (1, [2; 3]);
  (6, [7]);
];;

assert(test o (topol o));;

let o = [
  ("Hargreaves", ["Tevez"; "Rooney"]);
  ("Brown", ["Ronaldo"; "Carrick"; "Scholes"; "Hargreaves"]);
  ("Ronaldo", ["Tevez"; "Rooney"]);
  ("Carrick", ["Tevez"; "Rooney"]);
  ("Rooney", ["2008 UCL Winners"]);
  ("Scholes", ["Tevez"; "Rooney"]);
  ("Ferdinand", ["Ronaldo"; "Carrick"; "Scholes"; "Hargreaves"]);
  ("Vidic", ["Ronaldo"; "Carrick"; "Scholes"; "Hargreaves"]);
  ("van der Sar", ["Evra"; "Vidic"; "Ferdinand"; "Brown"]);
  ("Evra", ["Ronaldo"; "Carrick"; "Scholes"; "Hargreaves"])
];;
assert(test o (topol o));;

let o = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (7, [2]);
  (4, [2; 7; 1]);
  (6, [1; 3; 2; 5; 4; 7]);
  (5, [7; 4]);
  (5, [1;2])
];;

assert(test o (topol o));;

let o = [
  (5, [4]);
  (4, [3]);
  (3, [2]);
  (2, [1]);
  (1, [0]);
  (0, [-1]);
  (0, [-1]);
];;
assert(test o (topol o));;

let o = [
  ('A', ['r']);
  ('r', ['e']);
  ('e', ['k']);
  ('m', ['a']);
  ('d', ['o']);
  ('o', ['s']);
  ('s', ['c']);
];;
assert(test o (topol o));;

(* Testy cykliczne, spójne i niespójne grafy *)
let o = [
  (1, [1])
];;
assert(test_cyclic o);;

let o = [
  ("Glory", ["Man"; "Glory"]);
  ("Man", ["United"])
];;
assert(test_cyclic o);;
  
let o = [
  (1, [2]);
  (3, [4; 5; 3]);
  (2, [3])
];;

assert(test_cyclic o);;

let o = [
  ('a', ['b']);
  ('b', ['a'])
];;
assert(test_cyclic o);;

let o = [
  (1, [7; 2; 3]);
  (6, [1; 3; 2; 5; 4; 7]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (7, [2]);
  (5, [7; 4; 1; 2])
];;

assert(test_cyclic o);;

let o = [
  ("studia", ["brak czasu"]);
  ("brak czasu", ["praca"]);
  ("sen", ["jedzenie"; "praca"]);
  ("praca", ["sen"]);
  ("jedzenie", ["praca"; "sen"]);
  ("praca", ["jedzenie"])
 ];;

assert(test_cyclic o);;

let o = [
  (1, [2]);
  (6, [4]);
  (3, [2; 1]);
  (4, [5]);
  (2, [4]);
  (5, [6])
];;
assert(test_cyclic o);;

let o = [
  (2, [3]);
  (3, [4]);
  (2, [4]);
  (4, [7]);
  (7, [3])
];;
assert(test_cyclic o);;

(* Testy wydajnościowe *)

Random.init 461777389;;

let rnd a b =
  (Random.int (b - a + 1)) + a;;

let shuffle_list l =
  let temp = List.map (fun x -> (Random.bits (), x)) l
  in
  let sorted = List.sort compare temp
  in
  List.map snd sorted
;;

let rand_const = rnd 0 255;;

(* Generuje losowe stringi*)
let hash_label n =
  let rec aux acc n =
    if n <= 0 then acc
    else
      let l = (n lxor rand_const) mod 256
      in
      aux (acc ^ (String.make 1 (Char.chr l))) (n - l - 127)
  in
  aux "" (n + 1)
;;

(* Identyczność *)
let int_label n = n;;

let gen_path f n =
  let labels = Array.init n f
  in
  let rec aux acc v =
    if v = n - 1 then acc
    else aux ((labels.(v), [labels.(v + 1)])::acc) (v + 1)
  in
  aux [] 0
;;

let gen_loop f n =
  let g = gen_path f n
  in
  (f (n - 1), [f 0])::g
;;

let gen_random f n m =
  let labels = Array.init n f
  in
  let edges = Array.make n []
  in
  for i = 0 to m - 1 do
    let v = rnd 0 (n - 2)
    in
    let u = rnd (v + 1) (n - 1)
    in
    edges.(v) <- (labels.(u))::(edges.(v))
  done;
  List.mapi (fun i x -> (labels.(i), x)) (Array.to_list edges)
;;

let gen_random_cyclic f n m =
  let labels = Array.init n f
  in
  let edges = Array.make n []
  in
  for i = 0 to m - 1 do
    let v = rnd 0 (n - 1)
    in
    let u = rnd 0 (n - 1)
    in
    edges.(v) <- (labels.(u))::(edges.(v))
  done;
  List.mapi (fun i x -> (labels.(i), x)) (Array.to_list edges)
;;

let o = shuffle_list (gen_path int_label 20000);;
assert(test o (topol o));;

let o = shuffle_list (gen_random int_label 5000 20000);;
assert(test o (topol o));;

let o = shuffle_list (gen_loop int_label 20000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_random_cyclic int_label 5000 20000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_path hash_label 20000);;
assert(test o (topol o));;

let o = shuffle_list (gen_random hash_label 5000 20000);;
assert(test o (topol o));;

let o = shuffle_list (gen_loop hash_label 20000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_random_cyclic hash_label 5000 20000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_path int_label 90000);;
assert(test o (topol o));;

let o = shuffle_list (gen_random int_label 100000 1000000);;
assert(test o (topol o));;

let o = shuffle_list (gen_loop int_label 90000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_random_cyclic int_label 100000 1000000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_path hash_label 90000);;
assert(test o (topol o));;

let o = shuffle_list (gen_random hash_label 100000 1000000);;
assert(test o (topol o));;

let o = shuffle_list (gen_loop hash_label 90000);;
assert(test_cyclic o);;

let o = shuffle_list (gen_random_cyclic hash_label 100000 1000000);;
assert(test_cyclic o);;
