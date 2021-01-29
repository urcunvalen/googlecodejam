 (*  fireflies*)


 (* découpe une string en un couple total de string *)
  
let getn (a,b,c,d,e,f) n= match n with
 1 -> a
|2 -> b
|3 -> c
|4 -> d
|5 -> e
|6 -> f
;;


let addlistcouple (l,c,i) = match (List.rev(l),i) with
    ([],_) -> [(c,"","","","","")]
   |(a::r,1) -> List.rev (((getn a 1)^c, getn a 2, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,2) -> List.rev ((getn a 1, (getn a 2)^c, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,3) -> List.rev ((getn a 1, getn a 2, (getn a 3)^c, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,4) -> List.rev ((getn a 1, getn a 2, getn a 3, (getn a 4)^c, getn a 5, getn a 6) :: r)
   |(a::r,5) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, (getn a 5)^c, getn a 6) :: r)
   |(a::r,6) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, getn a 5, (getn a 6)^c) :: r)
;;

   
   
let rec spacelistcouple (s,i,l,n,k) = if (i >= n) then l else
                                        match (s.[i]) with
                                          (' ') -> spacelistcouple (s,(i+1),l,n,k+1)
                                         |(_)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),k),n,k)
;;	

let callcouple s = spacelistcouple (s,0,[],String.length(s),1)
;;

let center_of_mass (lb,n) = let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  lb) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  lb) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  lb)  /. n in 
sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) 
;;

let to_int lis = (List.map (fun (a,b,c,d,e,f) -> (int_of_string a, int_of_string b, int_of_string c, int_of_string d, int_of_string e, int_of_string f)) lis)
;;


let myr (t,p,a) = ((t *. (1. /. p) +. (a *. (1. /. p)))  /. (1. /.  p))
;;
let center_of_mass (lb,n) = 
let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  lb) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  lb) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  lb)  /. n in 
sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) 
;;


let returnB (alpha,beta) = match (alpha,beta) with
((d1,t1),(d2,t2)) when (d1 > d2) && (t2 > 0.) -> beta
|((d1,t1),(d2,t2)) when (d1 = d2) && (t2 > 0.) &&  (t1 > t2)-> beta
|_-> alpha
;;


let rec recfireCA (l,n,d,t,p,i) = 
(* let p = 10. ** (-. i) in  *)
let l_temp = List.map (fun (a,b,c,d,e,f) -> (myr(a,p,d *. t),myr(b,p,e *. t),myr(c,p,f *. t),d,e,f)) l in
let center = center_of_mass (l_temp,n) in 
match (d,i) with
(_,10) -> (d, myr(t,p,-. p))
|(a,_) when (a >	 center ) -> recfireCA (l,n,center,myr(t,p,p),p,i+1)
|_-> (d, myr(t,p,-. p))
;;

let rec recfireC (l,n,d,t,i) =
if (i = 9.) then 
(d,t)
else
let p = 10. ** (-. i) in
let alpha = recfireCA(l,n,10000.,t,p,0) in
recfireC (l,n,fst(alpha),snd(alpha),i +. 1.)
;;


let rec recfireA (l,n,d,t) = let center = center_of_mass (l,n) in
match (d) with
(a) when (a > center ) -> recfireA (List.map (fun (a,b,c,d,e,f) -> ((a +. d),(b +. e),(c +. f),d,e,f )) l,n,center, t +. 1.)
|_-> (d,t -. 1.) 
;;


let rec recfireB (l,n,d,t,i) = 
if (i = 9.) then
(d,t)
else
let p = 10. ** (-. i) in
let p2 = 10. ** (-. (i -. 1.)) in
let alpha = recfireCA(l,n,10000.,t,p,0) in
let beta = recfireCA(l,n,10000.,myr(t,p2, -. p2),p,0) in
match (alpha,beta) with
((d1,t1),(d2,t2)) when (d1 < d2) -> recfireB (l,n,d1,t1,i +. 1.)
|((d1,t1),(d2,t2)) -> recfireB (l,n,d2,t2,i +. 1.)
;;

let roundmy a = float_of_int(int_of_float(a /. (1. /. 10000000.))) /.  10000000.
;;


let fireflies (l,n) = let b = recfireA(to_int(l),float_of_int n,10000.,0.) in
let a = match (b) with
(_,0.) -> recfireC(to_int(l),float_of_int n,fst b,0.,1.)
|_-> recfireB(to_int(l),float_of_int n, fst b,snd b,1.)
in 

string_of_float (roundmy(fst a )) ^ " " ^ string_of_float (snd a)
;;






let rec algolec = fun flec n ->match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (algolec flec (n-1))
	;;


let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n ->let i = int_of_string (input_line flec) in let l = (algolec flec (i)) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (l,i))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) 
	;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;       
		close_out fecr;
	end
	;;
	
	
	
	
	
	
	
	
	(*   
let getn (a,b,c,d,e,f) n= match n with
 1 -> a
|2 -> b
|3 -> c
|4 -> d
|5 -> e
|6 -> f
;;

let getA (a,b,c) = a
;;

let getB (a,b,c) = b
;;
let getC (a,b,c) = c
;;
(* découpe une string en un couple total de string *)
let addlistcouple (l,c,i) = match (List.rev(l),i) with
    ([],_) -> [(c,"","","","","")]
   |(a::r,1) -> List.rev (((getn a 1)^c, getn a 2, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,2) -> List.rev ((getn a 1, (getn a 2)^c, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,3) -> List.rev ((getn a 1, getn a 2, (getn a 3)^c, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,4) -> List.rev ((getn a 1, getn a 2, getn a 3, (getn a 4)^c, getn a 5, getn a 6) :: r)
   |(a::r,5) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, (getn a 5)^c, getn a 6) :: r)
   |(a::r,6) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, getn a 5, (getn a 6)^c) :: r)
;;

   
   
let rec spacelistcouple (s,i,l,n,k) = if (i >= n) then l else
                                        match (s.[i]) with
                                          (' ') -> spacelistcouple (s,(i+1),l,n,k+1)
                                         |(_)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),k),n,k)
;;	

let callcouple s = spacelistcouple (s,0,[],String.length(s),1)
;;


(* l = [(x1,y1,z1,vx1,v1y,vz1);(x2,y2,z2,vx2,vy2,vz2)] *)


(* List.fold_left (fun acc (a,b,c,d,e,f)  -> a + acc) 0  l*)
(* List.fold_left (fun acc (a,b,c,d,e,f)  -> b + acc) 0  l*)
(* List.fold_left (fun acc (a,b,c,d,e,f)  -> c + acc) 0  l*)

(* le tout divisé par le nombre n de luciole *)

(*  >> centre de l'essaim*)

(* sqrt((x)^2 + (y)^2 + (z)^2 *)
let whatT (t,somme)= 
match (t,somme) with
(0.,_) -> 0.
|(a,b) when ((b < 1.) && (a = float_of_int(int_of_float a))) -> t
|_ -> t -. somme

let rec recfire (l,la,n,d,t,somme,tsave) =  
let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  l) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  l) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  l)  /. n in 
let center = sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) in 
match (d,t) with
(_,res) when ((res -. tsave) > somme *. 50.) -> (la,d,whatT (t,somme))
|(a,_) when (a = center) -> (la,d,whatT (t,somme)) (* si la distance ne change pas entre tn et tn+1 alors elle changera pas ensuite*)
|(a,_) when (a > center) -> recfire (List.map (fun (a,b,c,d,e,f) -> (a +. (d *. somme), b +. (e *. somme),c +. (f *. somme),d,e,f)) l,l,n,center,((float_of_int(int_of_float(t *. (1. /. somme)))) /. (1. /. somme)) +. somme,somme,tsave)
|_ -> (la,d,whatT (t,somme))
;;

let to_int lis = (List.map (fun (a,b,c,d,e,f) -> (float_of_string a, float_of_string b, float_of_string c, float_of_string d, float_of_string e, float_of_string f)) lis)
;;

let rec recfireA (l,n,d,t,i) = match i with
1. -> (d,t)
|_ -> let res = recfire (l,l,n,d,t,10. ** (-. i),t) in recfireA (getA res, n, getB res, getC res, i +. 1.) 
;;

let returnB (alpha,beta) = match (alpha,beta) with
((d1,t1),(d2,t2)) when (d1 > d2) && (t2 > 0.) -> beta
|((d1,t1),(d2,t2)) when (d1 = d2) && (t2 > 0.) &&  (t1 > t2)-> beta
|_-> alpha
;;

let rec recfireB (l,n,i,(da,ta)) =
let ic = (10. ** (-. i)) in
let ir = (10. ** (-. (i -. 1.))) in
let l1 = List.map (fun (a,b,c,d,e,f) -> (a +. (d *. ta), b +. (e *. ta),c +. (f *. ta),d,e,f)) l in
let la = List.map (fun (a,b,c,d,e,f) -> (a +. (d *. (ta-.ir)), b +. (e *. (ta-.ir)),c +. (f *. (ta-.ir)),d,e,f)) l in
let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  la) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  la) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  la)  /. n in 
let center = sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) in 
let alpha = recfire(l1,l1,n,da,ta,ic,ta) in
let beta = recfire(la,la,n,center,ta-.ir,ic,ta-.ir) in
match (i) with
7. -> returnB ((getB alpha, getC alpha),(getB beta, getC beta))
|_ -> let alphaA = recfireB(l,n,i +. 1.,(getB alpha, getC alpha)) in
let betaA = recfireB(l,n,i +. 1.,(getB beta, getC beta)) in returnB(alphaA,betaA)	
;;

let fireflies (l,n) = let a = recfireB(to_int(l),(float_of_int n),1.,recfireA(to_int(l),(float_of_int n),10000.,0.,0.)) in 
string_of_float (fst a ) ^ " " ^ string_of_float (snd a)
;;
  
  
  
  











let rec algolec = fun flec n ->match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (algolec flec (n-1));;


let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n ->let i = int_of_string (input_line flec) in let l = (algolec flec (i)) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (l,i))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;       
		close_out fecr;
	end;;

  

  ================================================
  ===============================================
  (* découpe une string en un couple total de string *)
  
let getn (a,b,c,d,e,f) n= match n with
 1 -> a
|2 -> b
|3 -> c
|4 -> d
|5 -> e
|6 -> f
;;


let addlistcouple (l,c,i) = match (List.rev(l),i) with
    ([],_) -> [(c,"","","","","")]
   |(a::r,1) -> List.rev (((getn a 1)^c, getn a 2, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,2) -> List.rev ((getn a 1, (getn a 2)^c, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,3) -> List.rev ((getn a 1, getn a 2, (getn a 3)^c, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,4) -> List.rev ((getn a 1, getn a 2, getn a 3, (getn a 4)^c, getn a 5, getn a 6) :: r)
   |(a::r,5) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, (getn a 5)^c, getn a 6) :: r)
   |(a::r,6) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, getn a 5, (getn a 6)^c) :: r)
;;

   
   
let rec spacelistcouple (s,i,l,n,k) = if (i >= n) then l else
                                        match (s.[i]) with
                                          (' ') -> spacelistcouple (s,(i+1),l,n,k+1)
                                         |(_)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),k),n,k)
;;	

let callcouple s = spacelistcouple (s,0,[],String.length(s),1)
;;


let myround (t,p) = 
(float_of_int(int_of_float(t *. (1. /. p)))) /. (1. /. p)
;;	

let rec recfire (l,n,d,t,puissance,acc) = 
let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  l) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  l) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  l)  /. n in 
let center = sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) in 
match ((acc,puissance),d,center) with
((2,indice),_,_) when (indice != 1.) -> (d,t)
|((1000,1.),_,_) -> (d,t -. puissance)
|(_,distance,centre) when (centre > distance) -> (d,t)
|_-> recfire (List.map (fun (a,b,c,d,e,f) -> (a +. (d *. puissance), b +. (e *. puissance),c +. (f *. puissance),d,e,f)) l,n,center,myround((myround (t +. puissance,puissance)),puissance),puissance,acc + 1)
;;

let returnB (alpha,beta) = match (alpha,beta) with
((d1,t1),(d2,t2)) when (d1 > d2)  -> beta
|((d1,t1),(d2,t2)) when (d1 = d2) && (t1 > t2)-> beta
|_-> alpha
;;

let rec recfireA (l,n,i,d,t) =let puissance = (10. ** (-. i)) in
let lc = List.map (fun (a,b,c,d,e,f) -> (a +. (d *. t), b +. (e *. t),c +. (f *. t),d,e,f)) l in
let res = recfire (lc,n,d,t,puissance,0) in
let la = List.map (fun (a,b,c,d,e,f) -> (a +. (d *. (snd(res))), b +. (e *. (snd(res))),c +. (f *. (snd(res))),d,e,f)) l in
let lb = List.map (fun (a,b,c,d,e,f) -> (a +. (d *. ((snd(res)) -. puissance)), b +. (e *. ((snd(res)) -. puissance)),c +. (f *. ((snd(res)) -. puissance)),d,e,f)) l in
let xc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  lb) /. n  in 
let yc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  lb) /. n in
let zc = (List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  lb)  /. n in 
let center = sqrt((xc ** 2.) +. (yc ** 2.) +. (zc ** 2.)) in
let resa = recfire (la,n,fst(res),snd(res),puissance *. 0.1,0) in
let resb = recfire (lb,n,center,snd(res) -. puissance,puissance *. 0.1,0) in
match (res,resa,resb,i) with 
((a,b),_,_,7.)  when (a < 1.)-> res
|((a,b),_,_,_)  when (a < 1.)-> recfireA (l,n,i +. 1.,fst(res),snd(res))
|(_,d1,d2,7.) -> returnB(d1,d2)
|_->let alpha = returnB (resa,resb) in 
recfireA(l,n,i +. 1.,fst(alpha),snd(alpha))
;;


let to_int lis = (List.map (fun (a,b,c,d,e,f) -> (float_of_string a, float_of_string b, float_of_string c, float_of_string d, float_of_string e, float_of_string f)) lis)
;;

let fireflies (l,n) = let a = recfireA(to_int(l),float_of_int n,0.,10000.,0.) in
string_of_float (fst a ) ^ " " ^ string_of_float (snd a)
;;





let rec algolxecec = fun flec n ->match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (algolec flec (n-1))
	;;


let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n ->let i = int_of_string (input_line flec) in let l = (algolec flec (i)) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (l,i))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) 
	;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;       
		close_out fecr;
	end
	;;
	
	
	
	
	(* jam "B-small-practice.in" "B-small.out" algolec fireflies *)
	
	
	=================================
	==================================
	
	  
let getn (a,b,c,d,e,f) n= match n with
 1 -> a
|2 -> b
|3 -> c
|4 -> d
|5 -> e
|6 -> f
;;

let getA (a,b,c) = a
;;

let getB (a,b,c) = b
;;
let getC (a,b,c) = c
;;
(* découpe une string en un couple total de string *)
let addlistcouple (l,c,i) = match (List.rev(l),i) with
    ([],_) -> [(c,"","","","","")]
   |(a::r,1) -> List.rev (((getn a 1)^c, getn a 2, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,2) -> List.rev ((getn a 1, (getn a 2)^c, getn a 3, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,3) -> List.rev ((getn a 1, getn a 2, (getn a 3)^c, getn a 4, getn a 5, getn a 6) :: r)
   |(a::r,4) -> List.rev ((getn a 1, getn a 2, getn a 3, (getn a 4)^c, getn a 5, getn a 6) :: r)
   |(a::r,5) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, (getn a 5)^c, getn a 6) :: r)
   |(a::r,6) -> List.rev ((getn a 1, getn a 2, getn a 3, getn a 4, getn a 5, (getn a 6)^c) :: r)
;;

   
   
let rec spacelistcouple (s,i,l,n,k) = if (i >= n) then l else
                                        match (s.[i]) with
                                          (' ') -> spacelistcouple (s,(i+1),l,n,k+1)
                                         |(_)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),k),n,k)
;;	

let callcouple s = spacelistcouple (s,0,[],String.length(s),1)
;;

let tr s = float_of_int(int_of_float s);;

let recfireA (l,n,i) = 
let xc = tr((List.fold_left (fun acc (a,b,c,d,e,f)  -> a +. acc) 0.  l) /. n)  in 
let yc = tr ((List.fold_left (fun acc (a,b,c,d,e,f)  -> b +. acc) 0.  l) /. n) in
let zc = tr ((List.fold_left (fun acc (a,b,c,d,e,f)  -> c +. acc) 0.  l)  /. n) in 
let vxc = tr ((List.fold_left (fun acc (a,b,c,d,e,f)  -> d +. acc) 0.  l) /. n)  in 
let vyc = tr ((List.fold_left (fun acc (a,b,c,d,e,f)  -> e +. acc) 0.  l) /. n) in
let vzc = tr ((List.fold_left (fun acc (a,b,c,d,e,f)  -> f +. acc) 0.  l)  /. n) in 
let xxc = tr(xc +. vxc *. i) in let yyc = tr(yc +. vyc *. i) in let zzc = tr(zc +. vzc *. i) in 
let d = tr(-.((xxc -. xc) *. xc +. (yyc -. yc) *. yc +. (zzc -. zc) *. zc)) in
if (d <= 0.) then
(sqrt(sqrt(xc) +. sqrt(yc) +. sqrt(zc)),0.)
else
let d = tr(tr(sqrt(xc -. xxc)) +. tr(sqrt(yc -. yyc)) +. tr (sqrt(zc -. zzc))) in
let temp = tr(tr(sqrt(yc *. zzc -. zc *. yyc)) +. tr (sqrt(zc *. xxc -. xc *. zzc)) +. tr(sqrt(xc *. yyc -. yc *. xxc))) in
let temp2 = tr(sqrt(abs_float	 (temp /. d))) in
let d = tr(tr(sqrt(xc)) +. tr(sqrt(yc)) +. tr(sqrt(zc)) -. tr(sqrt(temp2))) in
(temp2,sqrt(d /. tr(tr(sqrt(vxc)) +. tr(sqrt(vyc)) +. tr(sqrt(vzc)))))
;;	


let fireflies (l,n) = let a = recfireA(to_int(l),float_of_int n,10000.) in
string_of_float (fst a ) ^ " " ^ string_of_float (snd a)
;;



let rec algolec = fun flec n ->match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (algolec flec (n-1));;


let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n ->let i = int_of_string (input_line flec) in let l = (algolec flec (i)) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (l,i))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;       
		close_out fecr;
	end;;

  

 *)
	(* jam "B-small-practice.in" "B-small.out" algolec fireflies *)