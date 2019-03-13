open Origami

let center = (0.,0.)
  
let a = (1., 1.)
let b = (7., 4.)
let c = (4., 0.)
let d = (4., 4.)
let e = (2., 3.)
let f = (5., 2.)
let g = (4., 2.5)
let h = (3., 3.)
let i = (0., 3.)
let j = (5., 3.)
    

let kar = prostokat a b;;
assert(kar c = 0);;
assert(kar d = 1);;
assert(kar b = 1);;
assert(kar center = 0);;

let kar = zloz a b kar;;
assert(kar e = 1);;
assert(kar c = 0);;
assert(kar f = 0);;
assert(kar d = 2);;
assert(kar a = 1);;
assert(kar g = 1);;
assert(kar h = 2);;

let kar =  zloz c d kar;;
assert(kar g = 1);;
assert(kar h = 3);;
assert(kar (1., 4.) = 2);;

let kar = zloz i j kar;;
assert(kar (1., 4.) = 3);;
assert(kar (3., 4.) = 4);;
assert(kar (4., 5.) = 1);;
assert(kar (3.5, 4.) = 4);;
assert(kar (3.5, 4.1) = 2);;

let l = [(a, b); (c, d); (i, j)];;

let kar = prostokat a b;;
let kar = skladaj l kar;;
assert(kar (1., 4.) = 3);;
assert(kar (3., 4.) = 4);;
assert(kar (4., 5.) = 1);;
assert(kar (3.5, 4.) = 4);;
assert(kar (3.5, 4.1) = 2);;

let center = (0.,0.)
  
let a = (1., 1.)
let b = (3., 1.)
let c = (3., 2.)
let d = (4., 4.)
let e = (2., 3.)
let f = (5., 2.)
let g = (4., 2.5)
let h = (3., 3.)
let i = (-1., 1.)
let j = (-1., 2.)
let k = (0., -2.)
let m = (2., -1.)
    

let kar = kolko a 5.;;
assert(kar (6., 6.) = 0);;
assert(kar (1., -4.) = 1);;
assert(kar (6., 1.) = 1);;

let kar = zloz b c kar;;
assert(kar d = 0);;
assert(kar h = 1);;
assert(kar center = 1);;
assert(kar e = 2);;

let kar = zloz j i kar;;
assert(kar center = 2);;
assert(kar a = 3);;
assert(kar (0., 1.) = 3);;
assert(kar (0., 5.) = 2);;

let kar = zloz k m kar;;
assert(kar (0., 5.) = 2);;
assert(kar m = 2);;
assert(kar center = 3);;
assert(kar (1., -2.) = 0);;
assert(kar (-1., -1.) = 2);;

let l = [(b,c); (j, i); (k,m)];;

let kar = kolko a 5.;;
let kar = skladaj l kar;;
assert(kar (0., 5.) = 2);;
assert(kar m = 2);;
assert(kar center = 3);;
assert(kar (1., -2.) = 0);;
assert(kar (-1., -1.) = 2);;

let kolo = kolko (0.,0.) 10. in
assert (kolo (1000., 0.) = 0);
let poziomo = zloz (0.,0.) (1.,0.) kolo in
assert (poziomo (0.,0.) = 1);
assert (poziomo (0.,1.) = 2);
assert (poziomo (0.,-1.) = 0);
let pionowo = zloz (0.,0.) (0.,1.) kolo in
assert (pionowo (0.,0.) = 1);
assert (pionowo (-1.,0.) = 2);
assert (pionowo (1.,0.) = 0);
let cwiartka = zloz (0.,0.) (0.,1.) poziomo in
assert (cwiartka (0.,0.) = 1);
assert (cwiartka (-1.,1.) = 4);
assert (cwiartka (-1.,0.) = 2);;



let a = prostokat center (10., 10.);;

assert(a center = 1);;
assert(a (5., 5.) = 1);;
assert(a (10., 10.) = 1);;
assert(a (0., 10.1) = 0);;
assert(a (10.1, 10.1) = 0);;

let a = zloz (5., 0.) (5., 377.) a;;

assert(a center = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let a = zloz (5., 0.) (5., 1.) a;;

assert(a center = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;

let b = zloz (-7., -7.) (300., 300.) a;;

assert(b center = 2);;
assert(b (0., 5.) = 3);;
assert(b (2.5, 2.5) = 2);;
assert(b (1., 2.) = 4);;
assert(b (2.5, 5.) = 3);;
assert(b (2.5, 6.) = 2);;
assert(b (2.5, 2.) = 0);;
assert(b (5., 5.) = 1);;
assert(b (5., 0.) = 0);;
assert(b (4., 2.) = 0);;
assert(b (10., 0.) = 0);;
assert(b (10., 10.) = 0);;
assert(b (10., 2.5) = 0);;

let a = kolko (3., 3.) 7.;;
let a = zloz (5., -10.) (5., 100.) a;;
let a = zloz (5., 0.) (5., 0.01) a;;

assert(a center = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;

let l = [((5., 0.), (5., 377.)); ((5., 0.), (5., 1.));
	 ((-6., -6.), (-6.1, -6.1)); ((9., 5.), (4., 2.))];;

let a = prostokat center (10., 10.);;

let a = skladaj l a;;

assert(a center = 0);;
assert(a (2.9, 1.9) = 0);;
assert(a (5., 5.) = 0);;
assert(a (7., 1.) = 2);;
assert(a (7.1, 1.45) = 2);;
assert(a (7.1, 1.5) = 4);;
assert(a (7., 3.) = 4);;
assert(a (7., 3.8) = 2);;
assert(a (7., 3.81) = 0);;
assert(a (5., 0.) = 3);;
assert(a (5., 0.5) = 3);;
assert(a (5., 1.) = 7);;
assert(a (5., 2.) = 7);;
assert(a (5., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (9., 5.) = 1);;
assert(a (4., 0.) = 4);;
assert(a (3., 0.) = 4);;
assert(a (2., 0.) = 8);;
assert(a (1., 0.) = 8);;

let l = [((5., -10.), (5., 100.)); ((5., 0.), (5., 0.01));
	 ((1., 0.), (1., -1.)); ((5., 10.), (1., 0.));
	 ((1., 0.), (5., 10.))];;

let a = kolko (3., 3.) 7.;;

let a = skladaj l a;;

assert(a center = 3);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;

