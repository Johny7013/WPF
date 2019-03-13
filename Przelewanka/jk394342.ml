(* Implementacja zadania Przelewanka
   Autor: Jan Klinkosz id 394 342
   Review: Maciej Nadolski id 394 491 *)

(* Funkcja liczy NWD dwóch liczb *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b);;

(* 
   Funkcja dodaje stan (kombincaję ilości wody w poszególnych szklankach)
   do rozpatrzenia, o ile nie był on już wcześniej rozpatrzony.
   Zwraca true jeżeli nowy stan jest kombinacją końcową,
   False w przeciwnym wypadku 
*)
let dodaj nowy odw q koncowa ruchy=
  let odp = ref false
  in
  if  not (Hashtbl.mem odw nowy)
  then begin
    Hashtbl.add odw nowy true;
    Queue.push (nowy, ruchy + 1) q;
    odp := nowy = koncowa
  end;
  !odp
;;

(* 
   Zwraca nowy stan, który otrzymuje się ze stanu top przez przelanie
   zawartości szklanki i do szklanki j
*)
let przelej top i j pojemnosci=
  let nowy = Array.copy top in
  nowy.(j) <- nowy.(j) + nowy.(i);
  nowy.(i) <- 0;

  if nowy.(j) > pojemnosci.(j)
  then begin
    nowy.(i) <- nowy.(j) - pojemnosci.(j);
    nowy.(j) <- pojemnosci.(j);
  end;

  nowy
;;
	
(* 
   Implementacja algorytmu bfs, który dla danej tablicy pojemności szklanek
   i konfiguracji końcowej (oczekiwanej) zwraca -1 jeżeli nie da się uzyskać
   takiej konfuguracji albo minimalną liczbę ruchów potrzebnych do uzyskania
   tej konfiguracji, jeżeli jest ona możliwa do uzyskani 
*)
let bfs pojemnosci koncowa =
   let wynik = ref (-1) (* zmienna wynikowa *)
   and n = Array.length koncowa (* liczba szklanek *)
   and odw = Hashtbl.create 100000
   (* odw - tablica z haszowaniem za pomocą której sprawdzam, 
      czy dany stan już kiedyś był rozpatrywany *)
   and q = Queue.create ()
   (* kolejka par (stan, min. liczba ruchów porzebnych do uzystania stanu) *)
   and jest_wynik = ref false in
   (*zmienna logiczna, której wartość oznacza, czy znaleziono już odpowiedź *)
   let start = Array.make n 0 (* stan początkowy *)
   in
   Queue.push (start, 0) q;
   Hashtbl.add odw start true;

   while not (Queue.is_empty q) && not (!jest_wynik) do
     let (top, ruchy) = Queue.take q (* zdjęcie i usunięcie el. z kolejki *)
     in
     for i = 0 to n - 1 do begin
        let nowy = Array.copy top in
        (* wartość reprezentująca stan do którego można dojść z danej sytuacji *)
        nowy.(i) <- 0; (* wylanie zawartości szklanki i do zlewu *)
        (* sprawdzenie, czy otrzymano wynik i dodanie stanu*)
        jest_wynik := !jest_wynik || dodaj nowy odw q koncowa ruchy;
        
	      
	let nowy = Array.copy top in
	nowy.(i) <- pojemnosci.(i); (* nalanie wody do pełna w szklance i *)
      	jest_wynik := !jest_wynik || dodaj nowy odw q koncowa ruchy;
		
	for j = i + 1 to n - 1 do
           begin
              (* Przelewanie ze szklanki i do j i j do i *)
       	      let a = przelej top i j pojemnosci
       	      and b = przelej top j i pojemnosci in
	      jest_wynik := !jest_wynik || dodaj a odw q koncowa ruchy;
      	      jest_wynik := !jest_wynik || dodaj b odw q koncowa ruchy;
           end	  
	done;
     end
     done;
  if !jest_wynik then wynik := ruchy + 1; 
  done;

  !wynik
;;


(* Główna funkcja z zadania *)
let przelewanka tab =
  let niepodzielnosc = ref false
  (* czy wartości końcowe dzielą się przez NWD pojemności szklanek *) 
  and czy_puste_pelne = ref false
  (* czy jest jakaś szkalnka, która ma pozostać pusta/pełna *)
  and n = Array.length tab (* rozmiar tablicy *)
  in 
  if tab = [||] then 0
  else begin
    (* zastosowanie 2 heurystyk, które pozwalają względnie szybko 
       stwierdzić, czy dana konfiguracja końcowa jest w ogóle osiągalna
       1. Jeżeli końcowa ilość wody w jakiejś szklance nie dzieli się
       przez NWD pojemności szklanek to takiej konfiguracji nie da się uzyskać
       2. Jeżeli żadna ze szklanek na końcu nie pozostanie pusta albo pełna
       to takiej konfiguracji również nie da się osiągnąć, bo każdy z ruchów, 
       które możemy wykonać pozostawia jakąś szklanek pustą albo pełną *)
    let d = ref (fst tab.(0)) in
    Array.iter (fun (x, _) -> d := gcd !d x) tab;
    Array.iter (fun (x, y) ->
      if y mod !d <> 0 then niepodzielnosc := true;
      if y = 0 || y = x then czy_puste_pelne := true ) tab;

    (* sprawdzenie, czy zachodzi jakaś sytuacja z heurystyk *)
    if !niepodzielnosc || not !czy_puste_pelne then -1
    else begin
      (* rozbicie wejściowej tablicy na 2, pojemności szklanek
	 i końcowa konfiguracja *)
      let koncowa = Array.make n 0
      and pojemnosci = Array.make n 0
      in
      Array.iteri (fun i (x, y) ->
	pojemnosci.(i) <- x;
	koncowa.(i) <- y ) tab;
      (* wywołanie funkcji bfs, która zwróci wynik *)
      bfs pojemnosci koncowa;
    end
  end
;;

      
      
      
      
