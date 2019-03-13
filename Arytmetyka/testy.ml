open Arytmetyka

let is_nan x = compare x nan = 0;;

let a = wartosc_od_do neg_infinity infinity
let b = wartosc_od_do neg_infinity infinity
let c = plus a b
let d = razy a b
let e = podzielic a b
let f = minus a b;;

assert((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity,infinity,true));
assert((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity,infinity,true));
assert((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity,infinity,true));
assert((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity,infinity,true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic  b b;;
assert((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true,true,true));
assert((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true,true,true));;

let a = wartosc_od_do 3. 7.
let b = wartosc_od_do (-2.) 5.
let c = podzielic a b
let d = podzielic c b
let e = plus d (wartosc_dokladna 2.);;
let f = razy d b;;

assert (compare (sr_wartosc d) nan = 0);;
assert (not (in_wartosc d (-3. /. 10.0000000001)));;
assert (sr_wartosc b = 1.5);;
assert (min_wartosc a =3.0);;
assert (max_wartosc d = infinity);;
assert (min_wartosc d = neg_infinity);;
assert (in_wartosc f 1000000.213232333);;
assert (in_wartosc f (-3.14159));;
