(* Arytmetyka.ml
Autor: Jan Klinkosz nr indeksu 394342
Review: Mateusz Gienieczko nr indeksu 394302 *)


type wartosc =(float * float * bool) (* (początek przedziału, koniec przedziału, czy jest niespójny) *)

let uniwersum = (neg_infinity, infinity, false) (*cała dziedzina*)

let max4 a b c d = if a=nan || b=nan || c=nan || d=nan then nan else max (max a b) (max c d)

let min4 a b c d = if a=nan || b=nan || c=nan || d=nan then nan else min (min a b) (min c d)

let max a b = if a=nan || b=nan then nan else if a<b then b else a

let min a b = if a=nan || b=nan then nan else if a<b then a else b
  
(* przeciążenie operatora *. *)
let ( *. ) a b = if a=0. || b=0. then 0. else a *. b;;

  
(* sprawdza, czy przedział niespójny stał się całą dzidziną wartosci *)
let czy_uniwersum (a , b ) = if a > b then (neg_infinity, infinity, false) else (a,b,true)

(*przerabia x i p na przedział*)
let wartosc_dokladnosc x p =
  (min (x -. x *. p /. 100.) (x +. x *. p /. 100.),
   max (x -. x *. p /. 100.) (x +. x *. p /. 100.), false)

(*tworzy przedział*)
let wartosc_od_do x y =  (min x y, max x y, false)

let wartosc_dokladna x = (x, x, false)

(* sprawdzanie, czy wartosc jest w przedziale, podzielona na przypadki spójnego i niespójnego przedziału*)
let in_wartosc (a,b,c) y =
  if c=false then if a <= y && b >= y
      then true else false
  else if a < y && b > y
      then false  else true

let min_wartosc (a,_,c) =
  if c=false then a else neg_infinity

let max_wartosc (_,b,c) =
  if c=false then b else infinity

let sr_wartosc x =
  if min_wartosc x = neg_infinity
      && max_wartosc x = infinity
  then nan
  else (min_wartosc x +. max_wartosc x) /. 2.

(* sprawdza minimalną i maksymalna wartosc i ustala przedział *)
let plus (a,b,c) (d,e,f) =
  let max_w = max_wartosc (a,b,c) +. max_wartosc (d,e,f)
  and min_w = min_wartosc (a,b,c) +. min_wartosc (d,e,f)
  in match c,f with
    true, true -> uniwersum |
    false, true -> if b +. d >= a +. e then uniwersum else (b +. d, a +. e, true) |
    true, false -> if a +. e >= b +. d then uniwersum else (a +. e, b +. d, true) |
    false, false -> (min_w, max_w, false)
	    
(* sprawdza minimalną i maksymalna wartosc i ustala przedział *)
let minus (a,b,c) (d,e,f) =
  let max_w = max_wartosc (a,b,c) -. min_wartosc (d,e,f)
  and min_w = min_wartosc (a,b,c) -. max_wartosc (d,e,f)
  in match c,f with
    true, true -> uniwersum |
    false, true -> if a -. d <= b -. e then uniwersum else (b -. e, a -. d, true) |
    true, false -> if a -. d >= b -. e then uniwersum else (a -. d, b -. e, true) |
    false, false ->  (min_w, max_w, false)
    
let razy (a,b,c) (d,e,f) =

  (* zwraca maximum i minimum funkcji w zależności od znaków a,b,d,e *)
  let mnozenie (a,b,c) (d,e,f) =
  if a < 0. && b < 0.
  then czy_uniwersum (max(a *. e) (b *. e), min(a *. d) (b *. d))
  else if a < 0. then czy_uniwersum (max(b *. d) (a *. e), min(a *. d) (b *. e))
  else if b < 0. then czy_uniwersum (max(a *. d) (b *. e), min(a *. e) (b *. d))
       else czy_uniwersum  (max(a *. d) (b *. d), min(a *. e) (b *. e))


  in if (d=0. || d= (-0.)) && e=0. then (0., 0., false)
  else if (a=neg_infinity && b = infinity) || (d=neg_infinity && e = infinity)  then uniwersum
  else match c,f with
    true, true -> mnozenie (a,b,c) (d,e,f) |
    false, true -> mnozenie (a,b,c) (d,e,f) |		       
    true, false -> mnozenie (a,b,c) (d,e,f) |

    (* minimum i maximum z wszystkich wymnożeń, co dla spójnych przedziałów jest naszym wynikiem *)
    false, false ->  (min4 (a *. d) (a *. e) (b *. d) (b *. e),
		      max4 (a *. d) (a *. e) (b *. d) (b *. e),false)

let podzielic (a,b,c) (d,e,f) =
  if (d=0. || d= (-0.)) && e=0. then (nan, nan, false)
  else if (a=neg_infinity && b = infinity) || (d=neg_infinity && e = infinity)  then uniwersum
  else match c,f with
    
    true, true -> uniwersum | (* niespójny/niespójny = cała dziedzina *)

    (* mnożnie przez odwrotność *)
    false, true ->
      if in_wartosc (d,e,f) 0. = false
      then razy (a,b,c) ( min (1. /. d) (1./. e), max (1. /. d) (1./. e), false)
      else razy (a,b,c) ( min (1. /. d) (1./. e), max (1. /. d) (1./. e), true)	|

      (* mnożnie przez odwrotność *)
    _, false ->
      if in_wartosc (d,e,f) 0. = false
      then razy (a,b,c) (min (1. /. d) (1. /. e), max (1. /. d) (1. /. e), false)
      else if min_wartosc (d,e,f) = 0.
          (* przedział spójny i min_w=0 to odwrotnością jest 1/max_w, inf *)
      then razy (a,b,c) (1. /. max_wartosc (d,e,f), infinity, false) 
      else if max_wartosc (d,e,f) = 0.
          (* przedział spójny i max_w=0 to odwrotnością jest -inf, 1/min_w *)
      then razy (a,b,c) (neg_infinity, 1. /. min_wartosc (d,e,f), false)
          (* przedział przechodzący przez 0 i żaden kraniec nie jest 0 więc odwrotnosc to przedział niespójny*)
      else razy (a,b,c) ( min (1. /. max_wartosc (d,e,f)) (1./. min_wartosc (d,e,f) ),
			  max (1. /. max_wartosc (d,e,f)) ( 1./. min_wartosc (d,e,f) ), true) 
	    





