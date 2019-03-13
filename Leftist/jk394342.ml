(* 
   Implementacja zadania leftist.ml
   Autor: Jan Klinkosz nr indeksu: 394342
   Review: Piotr Wojtczak nr indeksu: 394980
*)

(* 
   typ wariantowy 'a queue, Nil (kolejka pusta)  albo 
   Node (lewe_poddrzewo * prawe_poddrzewo * priorytet * długość_prawej_ścieżki)
*)
   
type 'a queue = Nil | Node of 'a queue * 'a queue * 'a  * int;;


(* kolejka pusta *)
let empty = Nil;;

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty;;

(* funkcja wyłuskująca lewe_poddrzewo z 'a queue *)
let left_tree x =
  match x with
    Nil -> Nil |
    Node (a, _, _, _) -> a;;

(* funkcja wyłuskująca prawe_poddrzewo z 'a queue *)
let right_tree x =
  match x with
  Nil -> Nil |
  Node (_, a, _, _) -> a;;

(* funkcja wyłuskująca priorytet z Node *)
let priority x =
  match x with
    Node (_, _, a, _) -> a |
    Nil -> raise Empty;;

(* funkcja wyłuskująca długość_prawego_poddrzewa z 'a queue *)
let right_path_len x =
  match x with
  Nil -> -1 |
  Node (_, _, _, a) -> a;;

(* zwraca 'a queue z dłuższą skrajnie prawą ścieżką *)
let max_rpl a b =
  if right_path_len a > right_path_len b then a
  else b ;;

(* zwraca 'a queue z krótszą skrajnie prawą ścieżką *)
let min_rpl a b =
  if right_path_len a < right_path_len b then a
  else b ;;

(* 
   Funkcja join łączy 2 kolejki.
   Jeżeli którakolwiek z kolejek jest pusta, to automatycznie zwraca
   tą drugą.
   W przeciwnym wypadku porównuje priorytety korzeni i jeżeli piorytet
   pierwszej kolejki jest wartością mniejszą to ustawia tę wartość jako korzeń
   kolejki wynikowej i rekurencyjnie wywołuje się na prawym poddrzewie pierwszej 
   kolejki i drugiej kolejce. 
   W konkretnym punkcie zwraca kolejkę, której lewe poddrzewo to takie, 
   które ma max_rpl lewe poddrzewo kolejki z której pochodzi korzeń 
   i drzewo zwrócone przez rekurencyjne wywołanie, 
   natomaist prawe poddrzewo to takie, które ma min_rpl 
   z lewe poddrzewo kolejki z której pochodzi korzeń 
   i drzewo zwrócone przez rekurencyjne wywołanie.
   Priorytet to korzeń o mniejszej wartości, a długość prawego poddrzewa
   to rpl dla prawego podrzewa + 1.

   Jeżeli rpl dla prawego i lewego poddrzewa są równe to zwraca jako lewe
   poddrzewo lewe poddrzewo kolejki, z której pochodzi korzeń, a jako prawe
   to, które zwróci rekurencja (connected).

*)

let rec join a b =
    match a, b with
      Nil, x -> x |
      x, Nil -> x |
      Node (lt1, rt1, p1, _), Node (lt2, rt2, p2, _) ->
    if p2 > p1
    then let connected = join rt1 b
    in
      Node (max_rpl connected lt1,
	  min_rpl lt1 connected, p1,
	  right_path_len (min_rpl lt1 connected) + 1)
    else let connected = join rt2 a
    in
      Node (max_rpl connected lt2,
	  min_rpl lt2 connected, p2,
          right_path_len (min_rpl lt2 connected) + 1);;

(* 
   funkcja podnosi wyjątek Empty, kiedy chcemy wyrzucić coś z kolejki pustej,
   w przeciwnym wypadku zwraca priorytet korzenia i kolejkę, która powstaje ze
   złączenia poddrzew
*)
let delete_min a =
  match a with
    Nil -> raise Empty |
    Node (_, _, _, _) -> (priority a, join (left_tree a) (right_tree a)) ;;

(* 
   funkcja dodaje element do kolejki tworząc kolejkę jednoelementową
   i łącząc ją z kolejką q za pomocą funkcji join, zwraca kolejkę 
   z dodanym elementem
*)
let add e q = join (Node (Nil, Nil, e, 0)) q;;

(*
  funkcja sprawdza, czy kolejka jest pusta, zwraca true, jeżeli tak, 
  w przeciwnym wypadku false
*)
let is_empty a = (a = empty);;
