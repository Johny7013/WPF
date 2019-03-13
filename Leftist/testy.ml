(* Jan Klinkosz nr indeksu: 394 342 *)
  
open Leftist;;
open List;;

let x = empty |> add 1;;

(*test funkcji is_empty i stałej emtpy*)
assert(is_empty empty);;
assert( not (is_empty x));;
assert(delete_min x = (1, empty));;


(* sprawdzenie, czy delete_min od pustej kolejki zwraca wyjątek *)
assert(try ( (delete_min empty) |> (fun _ -> false)) with Empty -> true);;
 

(* 
   funkcja sprawdza, czy z kolejki q priorytety były ściągane w
   odpowiedniej kolejności zadanej w l, jeżeli tak zwraca true
   w przeciwnym wypadku false
 *)

exception WA;;

let test q l =
    try
      let (x, final_q) =
	fold_left (fun a head -> 
        let (e, que) = delete_min (snd a)
        in 
        if (compare head e) != 0 then raise WA 
        else (true, que)) 
	                      (true, q) l
      in
      is_empty final_q && x
    with WA -> false
;;


(* tworzy listę przez dodanie i razy listy l na początek listy a *)
let rec make_list l i a =
  if i=0 then a else make_list l (i-1) (l @ a)
;;

(* dodaje do kolejki elementy listy l *)
let add_list l q =
  fold_left (fun a x -> add x a) q l
;;


let q1 = empty |> add 5 |> add 4 |> add 3 |> add 2 |> add 1 |> add (-1);;
let q2 = empty |> add 4 |> add 7 |> add 3 |> add 1 |> add 5;;
let q3 = join q2 q1;;
let q4 = empty |> add (-4) |> add (-2) |> add 3 |> add 17 |> add 12 |> add (-1);;
let q5 = join q3 q4;;

let lista1 = sort compare [5;4;3;2;1;(-1)];;
let lista2 = sort compare [4;7;3;1;5];;
let lista3 = sort compare (lista2 @ lista1);;
let lista4 = sort compare [(-4);(-2);3;17;12;(-1)];;
let lista5 = sort compare (lista4 @ lista3);;


(* Proste testy funkcji add, delete_min i join *)
(* Integer *)
assert(test q1 lista1);;
assert(test q2 lista2);;
assert(test q3 lista3);;
assert(test q4 lista4);;
assert(test q5 lista5);;

(* Float *)

let q1 = empty |> add 5.4 |> add 3.4 |> add 2.3 |> add 2.2 |> add 1.909 |> add (-1.56);;
let q2 = empty |> add 4.67 |> add 7.56 |> add 3.34 |> add 1.12 |> add 5.78;;
let q3 = join q2 q1;;
let q4 = empty |> add (-4.54) |> add (-1.2) |> add 3.67 |> add 12.17 |> add 12.001 |> add (-1.89);;
let q5 = join q3 q4;;

let lista1 = sort compare [5.4; 3.4; 2.3; 2.2; 1.909; (-1.56)];;
let lista2 = sort compare [4.67; 7.56; 3.34; 1.12; 5.78];;
let lista3 = sort compare (lista2 @ lista1);;
let lista4 = sort compare [(-4.54); (-1.2); 3.67; 12.17; 12.001; (-1.89)];;
let lista5 = sort compare (lista4 @ lista3);;

assert(test q1 lista1);;
assert(test q2 lista2);;
assert(test q3 lista3);;
assert(test q4 lista4);;
assert(test q5 lista5);;

(* String *)


let q1 = empty |> add "Ala"  |> add "ma" |> add "kota" |> add "kot" |> add "ma" |> add "Ale";;
let q2 = empty |> add "345" |> add "8476" |> add "3.34" |> add "112" |> add "jak";;
let q3 = join q2 q1;;
let q4 = empty |> add "Ab" |> add "ovo" |> add "usque" |> add "ad" |> add "mala";;
let q5 = join q3 q4;;

let lista1 = sort compare ["Ala"; "ma"; "kota"; "kot"; "ma"; "Ale"];;
let lista2 = sort compare ["345"; "8476"; "3.34"; "112"; "jak"];;
let lista3 = sort compare (lista2 @ lista1);;
let lista4 = sort compare ["Ab"; "ovo"; "usque"; "ad"; "mala"];;
let lista5 = sort compare (lista4 @ lista3);;


assert(test q1 lista1);;
assert(test q2 lista2);;
assert(test q3 lista3);;
assert(test q4 lista4);;
assert(test q5 lista5);;

(* Bool *)

let q1 = empty |> add true  |> add false |> add false |> add true |> add false |> add true;;
let q2 = empty |> add false |> add true |> add false |> add true |> add true;;
let q3 = join q2 q1;;
let q4 = empty |> add false |> add true |> add false |> add false |> add false;;
let q5 = join q3 q4;;

let lista1 = sort compare [true; false; false; true; false; true];;
let lista2 = sort compare [false; true; false; true; true];;
let lista3 = sort compare (lista2 @ lista1);;
let lista4 = sort compare [false; true; false; false; false];;
let lista5 = sort compare (lista4 @ lista3);;


assert(test q1 lista1);;
assert(test q2 lista2);;
assert(test q3 lista3);;
assert(test q4 lista4);;
assert(test q5 lista5);;


(* Testy wydajnościowe *)

let q1 = add_list (make_list [3;1;4;2] 25000 []) empty;;
let q2 = add_list (make_list [(-2);7;(-1);8] 30000 []) empty;;
let q3 = join q1 q2;;
let q4 = add_list (make_list ["42"; "424242"; "4242"] 50000 []) empty;;

(* test z kolejką jako elementem *)
let q5 = add_list (make_list [x] 150000 []) empty;;

let lista1 = sort compare  (make_list [3;1;4;2] 25000 []);;
let lista2 = sort compare  (make_list [(-2);7;(-1);8] 30000 []);;
let lista3 = sort compare (lista1 @ lista2);;
let lista4 = sort compare (make_list ["42"; "424242"; "4242"] 50000 []);;

(* test z kolejką jako elementem *)
let lista5 = sort compare (make_list [x] 150000 []);;

assert(test q1 lista1);;
assert(test q2 lista2);;
assert(test q3 lista3);;
assert(test q4 lista4);;
assert(test q5 lista5);;

  

