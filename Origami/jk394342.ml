(*
  Implementacja zadania Origami 
  Autor: Jan Klinkosz id 394 342
  Review: Wojciech Bączkowski id 394 071
*)

open List;;

type point = float * float;;

type kartka = point -> int;;

(* wartość epsilon, stosowana do liczenia wszystkiego z pewną dokładnością *)
let eps = 1. /. 1e11;;

(* sprawdzanie, czy obydwie współrzędne punktu x są >= współrzędnym punktu y *)
let wiekszy (x:point) (y:point) =  fst x +. eps >= fst y && snd x +. eps >= snd y;;

(* sprawdzanie, czy obydwie współrzędne punktu x są <= współrzędnym punktu y *)
let mniejszy (x:point) (y:point) = fst x -. eps <= fst y && snd x -. eps <= snd y;;

(* Iloczyn skalarny dwóch wektorów x i y *)
let skalarny (x:point) (y:point) = fst x *. fst y +. snd x *. snd y;;

(* 
   Iloczyn wektorowy dwóch wektorów x i y 
   1. Jeżeli jest dodatni, to koniec wektora x jest po prawej stonie końca wektora y 
   2. Jeżeli jest ujemny, to koniec wektora x jest po lewej stonie końca wektora y 
   3. Jeżeli jest równy 0 to wektory te są współliniowe
*)
let wektorowy (x:point) (y:point) = fst x *. snd y -. fst y *. snd x;;


(* 
   Zwraca punkt po odbicu punktu x przez prostą przechodzącą przez 
   punkty l1 i l2, l1 != l2 
*)
let odbicie (x:point) (l1:point) (l2:point) = 
  let (wektor:point) = (fst l2 -. fst l1, snd l2 -. snd l1)
  and (x:point) = (fst x -. fst l1, snd x -. snd l1)
  in let (q:point) = ( (skalarny x wektor) /. (skalarny wektor wektor) *. fst wektor,
		       (skalarny x wektor) /. (skalarny wektor wektor) *. snd wektor)
  in ((fst x +. 2. *. (fst q -. fst x) +. fst l1,
       snd x +. 2. *. (snd q -. snd x) +. snd l1):point)
;;

(* zwraca kartę reprezentującą prostokąt, lewy dolny róg to p1, prawy górny to p2 *)
let prostokat (p1:point) (p2:point)  =
  let k (x:point) =
    if wiekszy x p1 && mniejszy x p2
    then 1
    else 0	
  in k
;;

(* zwraca kartkę reprezentującą koło domknięte o środku w p i promieniu r *)
let kolko (p:point) r =
  let k (x:point) =
    let first = (fst x -. fst p) *. (fst x -. fst p)
    and second = (snd x -. snd p) *. (snd x -. snd p)
    in
    if first +. second -. eps <= r *. r
    then 1
    else 0
  in k
;;

(* 
   Składa kartkę wzdłuż skierowanej prostej przechodzącej przez 
   punkty l1 i l2 (l1!=l2) i zwraca już złożoną kartkę.
   De facto funkcja typu kartka odbija punkty względem prostych i sprawdza, 
   czy mieszczą się one w pierwotnej kartce 
*)
let zloz (l1:point) (l2:point) k =
  let new_k (x:point) =
     let (wektor:point) = (fst l2 -. fst l1, snd l2 -. snd l1)
     and (y:point) = (fst x -. fst l1, snd x -. snd l1)
     in let kierunek = wektorowy y wektor
     in if kierunek +. eps >= 0. && kierunek -. eps <= 0. then k x
     else if kierunek < 0. then k x + k (odbicie x l1 l2)
     else 0
  in new_k
;;

(* 
   Składa kartę wzdłuż wszystkich prostych z listy l reprezentowanych przez pary różnych punktów, 
   przez które te proste przechodzą, z listy l 
*)
 
let skladaj l f =
  let pom a (l1, l2) = zloz l1 l2 a
  in fold_left pom f l

;;

