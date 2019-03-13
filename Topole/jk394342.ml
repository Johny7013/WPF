(* 
   Implementacja zadania Sortowanie Topologiczne
   Autor: Jan Klinkosz id 394 342
   Review: Piotr Borowski id 394 091
*)

open PMap;;

exception Cykliczne;;

(* 
   Dla danego grafu postaci [|[a1_1;a1_2..], [a2_1;a2_2..], [an_1;an_2..]|]
   (gdzie w komórce tablicy o indeksie i jest lista etykiet sądiadów wierzchołka
   o etykiecie i) zwraca referencję do listy etykiet posortowanej topologicznie.
   Etykiety to wartości typu int.
*)
let top tab =
  let l = ref []
  (*tablica odwiedzin: -1 - wierzchołek nieodwiedzony, 
    0 - wierzchołek w trakcie  przetwarzania,
    1 - wierzchołek przetworzony *)
  and odw = Array.make (Array.length tab) (-1)
  in
  let rec dfs graf x =
    let s = ref (graf.(x))
    in
    begin
      odw.(x) <- 0;
      while !s <> [] do
        match !s with
         h::t -> begin
	   if odw.(h) = (-1)
	   then dfs tab h
 	   else if odw.(h) = 0
	   then raise Cykliczne;
           s := t; end
        |[] -> ()
      done;
      l := [x] @ !l;
      odw.(x) <- 1;
    end
  in
  for i = 0 to Array.length tab - 1 do
    if odw.(i) = -1
    then dfs tab i
  done;
  l
;;

(* 
   Tworzy graf postaci [|[a1_1;a1_2..], [a2_1;a2_2..], [an_1;an_2..]|]
   (gdzie w komórce tablicy o indeksie i jest lista etykiet sądiadów wierzchołka
   o etykiecie i) z listy relacji i tablicę etykiety. Zwraca graf i tablicę etykiety.
   Etykiety to wartości typu int. 
*)
let rec stworz_graf lista =
  let kolejnosc = ref [] (*odwrócona kolejność nadawania etykiet (int)*)
  and mapa = ref empty (*zawiera pary (element, etykieta) *) 
  and licznik = ref 0 (* służy do zliczenia liczby elemntów w grafie*)
  and etykiety = ref [||] (*tablica, gdzie na i-tym miejscu jest el. o etykiecie i*)
  and graf = ref [||]
  in
  (*Sprawdzenie, czy dany element x ma już nadaną etykietę, 
    jeżeli nie to jest ona nadawana, budowanie listy kolejnosc*) 
  let spr x =
    if not (exists x !mapa)
    then
      begin
	mapa := add x !licznik !mapa;
	kolejnosc := [x] @ !kolejnosc;
	incr licznik;
      end
  (* Zwraca tablicę etykiety (o długości ile) z listy kolejnosc*)
  and etyk ile l =
    let tab = Array.make !ile (fst (List.hd lista)) (*zakłada, że lista <> []*)
    in
    List.iter (fun h -> decr ile; tab.(!ile) <- h) !l;
    tab
  in
  (*Nadawanie etykiet elementom z lista*) 
  List.iter (fun (a, l) -> spr a; List.iter (fun h -> spr h) l) lista;

  etykiety := etyk (ref !licznik) kolejnosc; (* tworzenie tablicy etykiet *)
  graf := Array.make !licznik ([]); (* tworzenie pustego grafu *)

  (*Tworzenie grafu etykiet elementów na podstawie relacji z listy lista*)
  List.iter (fun (a, l) ->
      List.iter (fun h ->
      !graf.(find a !mapa) <- [(find h !mapa)] @ !graf.(find a !mapa)) l) lista;

  (!graf, !etykiety)
;;


let topol lista =
  let wynik = ref []
  and lis = ref []
  in
   let para =
   (*Zagwarantowanie, że do funkcji stworz_graf nie zostanie przekazana
     pusta lista (wymaganie funkcji etyk) *)
   if lista = [] then ([||], [||]) 
   else stworz_graf lista
   in
   let etykiety = snd para in
   lis := !(top (fst para)); (* sortowanie topologiczne grafu etykiet *)

   (* Zamiana etykiet na elementy, które one oznaczają *)
   List.iter (fun h -> wynik := [etykiety.(h)] @ !wynik) !lis;

   wynik := List.rev !wynik;
   !wynik
;;
