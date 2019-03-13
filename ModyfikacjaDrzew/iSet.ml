(*Implementacja zadania  Modyfikacje Drzew
  Autor: Jan Klinkosz nr indeksu: 394 342
  Review: Marek Żochowski nr indeksu: 395 071
 *)


(* warotść typu t to 
   Empty - puste drzewo,
   Node (1, 2, 3, 4, 5), gdzie: 
   1 - lewe poddrzewo, 
   2 - wartość w wierzchołku (przedział)
   3 - prawe poddrzewo
   4 - wysokość drzewa
   5 - para (a,b), gdzie a - liczba pojedynczych elementów
   w lewym poddrzewie, b - liczba elementów w prawym poddrzewie
*) 
   
type t = Empty | Node of t * (int * int) * t  * int * (int * int) ;;

(* puste drzewo *)
let empty = Empty;;

(* sprawdzenie, czy drzewo jest puste *)
let is_empty x = x = Empty;;

(* komparator przedziałów, przy założeniu, że są rozłączne *)
let cmp (a, b) (c, d) = compare b c;;

(* sprawadzanie, czy element x jest w przedziale <a, b> *)
let check (a,b) x =
  if x <= b && x >= a then 0 else
  if x < a then (-1) else 1;;

(* sprawdzanie czy x jest w przedziale <a, b+1> *)
let check2_left (a,b) x =
  if a = min_int then
    if b = max_int then 0
    else if x <= b + 1 then 0 else 1
  else
    if b = max_int then if x >= a  then 0 else -1
    else
      if x <= b + 1 && x >= a  then 0
      else if x < a  then (-1) else 1;;

(* sprawdzanie czy x jest w przedziale <a-1, b> *)
let check2_right (a,b) x =
  if a = min_int then
    if b = max_int then 0
    else if x <= b then 0 else 1
  else
    if b = max_int then if x >= a - 1  then 0 else -1
    else
      if x <= b && x >= a - 1  then 0
      else if x < a - 1 then (-1) else 1;;
  

(* zwracana jest wysokośc drzewa *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* zwraca liczbę pojedycznych elementów  w drzewie, 
jeżeli jest większa niż max_int -> max_int *)
let number_ver x =
  match x with
  | Node(_, (a,b), _, _, low) ->
      if (b - a + fst low + snd low + 1) <= 0 then max_int
      else b - a + fst low + snd low + 1
	
  | Empty -> 0
;;

(* tworzy wierzchołek w drzewie otrzymując lewe poddrzewo, wartość i przwe poddrzewo
pod warunkiem, że lewe i prawe poddrzewo są poprawnymi drzewami AVL *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1, (number_ver l, number_ver r))

(* tworzy i balansuje drzewo otrzymując:
   l jako lewe poddrzewo (poprawne drzewo AVL),
   p jako prawe poddrzewo (poprawne drzewo AVL)
   k jako wierzchołek w drzewie  
   (k to taki wierzchołek, że k jest większy od wszystkich z l i mniejszy od wszystkich z p)

   Balansowanie odbywa się na zasadzie sprawdzenia, czy wysokości w drzewach l i p różnią się
   o więcej niż 2, jeżeli nie to tworzone jest drzewo (l,k,p), gdzie k jest korzeniem.
   W przeciwnym wypadku (rozpatrzmy przypadek, kiedy l ma większą wysokośc od p, drugi
   przypadek jest analogiczny) sprawdzamy:
   Czy lewe poddrzewo l jest wyższe od prawego poddrzewa l: 
   Jeżeli tak to ustawiamy korzeń l jako korzeń całego drzewa i odpowiednio
   dobieramy prawe i lewe poddrzewo. 
   Jeżeli nie to ustalamy jako korzeń korzeń prawego poddrzewa l i odpowiednio dobieramy
   prawe i lewe poddrzewo.

   Dzięki tym operacją stworzymy zbalansowane drzewo (Jeżeli tylko nie różniły się one
   o za dużo, ale o to dba funkcja join).
   
*)


let bal l k r =          
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, (number_ver l, number_ver r))

(* dodaje jeden przedział do drzew pod warunkiem, 
   że jest on rozłączny ze wszystkimi innymi przedziałami,
   sprawdza, czy jest większy/mniejszy/równy od korzenia aktualnego drzewa
   i na podstawie tego ustala gdzie będzie trzeba wstawić ten przedział*)

let rec add_one x = function
  | Node (l, k, r, h, low) ->
      let c = cmp x k in
      if c = 0 then Node (l, x, r, h, low)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, (0, 0))
;;

   
(* przyjmuje 2 drzewa AVL i balansuje je dla coraz to mniejszych
   poddrzew tak długo jak złączenie nie jest AVL 
*)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r
;;

(* zwraca minimalny element drzewa *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* usuwa minimalny element z drzewa *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"


(* Usuwa wszystkie elementy z przedziału <a, b> z drzewa 
   (o ile były takie elementy)
   Realizuje to następująco:
   Jeżeli usuwany przedział jest większy (sprawdzanie za pomocą funkcji check)
   to wywołuje się na prawym poddrzewie, jeżeli mniejszy to na lewym.
   W przypadku kiedy znajdziemy się w momencie kiedy nie będzie możliwe przechodzenie
   jednocześne prawym i lewym końcem przedziału jednoczesnie wywoływane są funkcje 
   walk_left i wal_right, które usuwają odpowiednio wszystko co wieksze równe 
   lewemu końcowi w poddrzewi i mniejsze równe prawemu końcowi w podrzewie.

   To co zwracają kolejne wywołania funkcji jest balansowane i łączone za pomocą 
   funkcji join.
*)
let rec remove (a,b) set =
  let rec walk_left s x =
    match s with
   |  Node (l, v, r, _, _) ->
	let c = check v x in
	if c = 0 then
	  if x - fst v <> 0 then join l (fst v, x-1) Empty
	  else l
	else if c < 0 then walk_left l x
	else join l v (walk_left r x)
   | Empty -> Empty
	 
  and walk_right s x =
    match s with
   |  Node (l, v, r, _, _) ->
	let c = check v x in
	if c = 0 then
	  if snd v - x <> 0 then join Empty (x+1, snd v) r
	  else r
	else if c > 0 then walk_right r x
	else join (walk_right l x) v r
   | Empty -> Empty

  in 	    
  match set with
  |  Node (l, (x,y), r, _, _) ->
      if a > y then join l (x,y) (remove (a,b) r)
      else if b < x then join (remove (a,b) l) (x,y) r
      else
      let (left, right) = ((walk_left set a), (walk_right set b))
      in
      if right = empty then left
      else join left (min_elt right) (remove_min_elt right)
	  
  | Empty -> Empty
;;

(* Funkcja sprawdza jaki największy spójny przedział może utworzyć z ver = (a,b) i elementów,
   które już są w drzewie. Usuwa wszystkie elementy z przedziału <a,b> i też te, które się
   z nim stykają i potem wrzuca funkcją add_one nowy przedział (teraz już na pewno rozłączny
   z innymi) *)

let add ver set = 
  let rec pom (a,b) set =

    (* Funkcja walk_left przyjmuje drzewo s i wartość x i zwraca parę (a,b) gdzie,
       a - drzewo s bez elementów większych równych x (jest AVL)
       b - jest to najmniejsza taka wartość, że w a są wszystkie elementy
       przedziału <b,x>

       walk_right analogicznie tylko, że z drugiej strony
     *)
    
    let rec walk_left s x =
      match s with
      |  Node (l, v, r, _, _) ->
	  let c = check2_left v x in
	  if c = 0 then (l, min (fst v) x)
	  else if c < 0 then walk_left l x 
	  else let (tree, left_end) = (walk_left r x)
	  in (join l v tree, left_end)
      | Empty -> (Empty, x)
	    
    and walk_right s x =
      match s with
      |  Node (l, v, r, _, _) ->
	  let c = check2_right v x in
	  if c = 0 then (r, max (snd v) x)
	  else if c > 0 then walk_right r x
          else let (tree, right_end) = (walk_right l x)
	  in (join tree v r, right_end)
      | Empty -> (Empty, x)

    in 	    
    match set with
    |  Node (l, (x,y), r, _, _) ->
	let (first_root, second_root) = (* Funkcja pomocnicza, aby końce przedziału się nie *)
	  if x = min_int then           (* przękręcały przy odejmowaniu lub dodawaniu *)
	    if y = max_int then (min_int, max_int)
	    else (min_int, y+1)
	  else if y = max_int then (x-1, max_int)
	  else (x-1, y+1)
        in (* a - początek dodawanego przedziału, b - koniec dodawanego przedziału *)
	if a > second_root 
	then let (tree, inter) = (pom (a,b) r) in  (join l (x,y) tree, inter)
        else if b < first_root
        then let (tree, inter) = (pom (a,b) l) in (join tree (x,y) r, inter)
        else let ((left_tree, left_end), (right_tree, right_end)) =
                 ((walk_left set a), (walk_right set b))
             in if right_tree = empty
             then (left_tree, (left_end, right_end))
             else (join left_tree (min_elt right_tree) (remove_min_elt right_tree),
                  (left_end, right_end))
    
| Empty -> (Empty, (a,b))

  in let (new_set, inter) = pom ver set
  in add_one inter new_set
;;

(* sprawdza ile jest elementów <= x w drzewie set
   dzięki wartości w wierzchołu wiemy będąc w korzeniu
   ile jestpojedynczych elementów < od korzenia *)
let rec below x set =
  let rec pom x set result =
    match set with
    | Node (l, k, r, _, (lower_left, _)) ->
	let c = check k x
	in
	if c = 0 then
	  if (lower_left + x - fst k + 1 + result) <= 0 then max_int
	  else lower_left + x - fst k + 1 + result
	else if c > 0 then if (result + lower_left + snd k - fst k + 1) < 0
	    then pom x r max_int else pom x r (result + lower_left + snd k - fst k + 1)
	else pom x l result
    | Empty -> result
  in pom x set 0
;;


(* Zwraca taką trójkę (l,present,r), gdzie l jest liczbą mniejszych elemntów od x,
   które należą do set, r to liczba większych elementów od x, które należą do set,
   a present to zmienna logiczna, która mówi, czy x jest w set 
   
   Funkcja rozbita na przypadki, kiedy x znajduje się w przedziale, który jest
   w korzeniu to odpowiednio łączymy dzielimy na poddrzewa, jeżeli x jest większy
   od elementów z tego przedziału to całe lewe poddrzewo aktualnego drzewa jest 
   mniejsze od x (łącznie z korzeniem), jeżeli x jest mniejszy to analogicznie)
*)
   
let rec split x set =
  match set with
  | Node (l, k, r, _, _) ->
      let c = check k x
      in
	if c = 0 then if x = fst k && x = snd k then (l , true, r)
	else if x = fst k then (l, true, join Empty (x+1, snd k) r)
	else if x = snd k then (join l (fst k, x-1) Empty, true, r)
	else (join l (fst k, x-1) Empty, true, join Empty (x+1, snd k) r)
	else
	  if c > 0 then
	    let (left_tree, bool, right_tree) = split x r
	    in (join l k left_tree, bool, right_tree)
	  else
	    let (left_tree, bool, right_tree) = split x l
	    in (left_tree, bool, join right_tree k r)
  | Empty -> (Empty, false, Empty)
;;

(* sprawdza, czy wartośc x znajduje się w set przy pomocy 
   funkcji check *)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = check k x in
        c = 0 || loop (if c < 0 then l else r)
      | Empty -> false in
  loop set
;;


(* aplikuje funkcje f do wszystkich przedziałów w kolejności rosnącej *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set
;;

(* [fold f s a] wylicza [(f xN ... (f x2 (f x1 a))...)], gdzie x1
    ... xN są elementami s w kolejności rosnącej. *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set
;;

(* zwraca listę elementów seta w kolejności rosnącej *)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set
;;

