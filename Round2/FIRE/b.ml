(*
 * nom : callcouple
 * pre-conditions : s : string, string contenant des éléments séparés par des espaces
 * post-conditions : renvoi une liste de string, dont chaque élément correspond à un couple de caractère ou de série de caractère de s délimités par des espaces
 *
 * nom : spacelistcouple
 * pre-conditions : s : string, string envoyé par call ; i : int; itérateur de la position dans la string ; l : string list, liste récursivement accumulée avec les éléments de s ; n : int, taille de s, k : indice du nuplé
 * post-conditions : accumule de façon récursive les charactères de s dans la liste l, selon 3 critère : le charactère est un espace ou pas/le capteur b 
 *
 * nom : addlistcouple
 * pre-conditions : l : string list, la liste à laquelle on doit ajouter le charactère (c : string), dans le couple i
 * post-conditions : renvoi la liste avec le dernier élement concaté de c, dans la partie i
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
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



(*
 * nom : fireflies
 * pre-conditions : l : string list, chaque élément est une luciole (coord + vitesse) ; n : nombre de luciole
 * post-conditions : on renvoi le temps auquel le centre de l'essaim sera le plus près de nous. 
 * 			On exprimme la distance en fonction de taille
 *				On dérive l'expression
 *				On cherche la valeur de t pour laquelle la dérivé vaut 06/11/15
 *				On renvoie ce t
 *
 * nom : to_float
 * pre-conditions : lis : string list, la liste des lucioles
 * post-conditions : on renvoie une liste de luciole, ou les lucioles sont des nuplé de 6 éléments : (x,y,z,vx,vy,vz)
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let to_float lis = (List.map (fun (a,b,c,d,e,f) -> (float_of_string a, float_of_string b, float_of_string c, float_of_string d, float_of_string e, float_of_string f)) lis)
;;
let fireflies (l,n) = let l = List.fold_left (fun (x,y,z,vx,vy,vz) (a,b,c,d,e,f) -> (x+.a,y+.b,z+.c,vx+.d,vy+.e,vz+.f) ) (0.,0.,0.,0.,0.,0.) (to_float l) in
let x = getn l 1 in let y = getn l 2 in let z = getn l 3 in let vx = getn l 4 in let vy = getn l 5 in let vz = getn l 6 in 
let v = ( (vx ** 2.) +. (vy ** 2.) +. (vz ** 2.) ) in
let t = if (v = 0.) then 
0.
else
(     ((-.(vx *. x)) -. (vy *. y) -. (vz *. z))   /. v) 
in
let t = 
if (t < 0.) then
0.
else 
t
in
let d = (1. /. (float_of_int n)) *. sqrt ( ((x +. (t *. vx))**2.) +. ((y +. (t *. vy))**2.) +. ((z +. (t *. vz))**2.) ) in 
string_of_float(d) ^ " " ^ string_of_float(t)
;;



 (*
 * nom : jam
 * pre-conditions : fin,fout  2 noms de fichiers
 * pre-conditions : fonction algolec (unit_channel -> `a),
 *                  qui lit le contenu d'un cas de fin,
 * pre-conditions : fonction algopp (`a -> string),
 *                  qui renvoie la solution du cas passé en paramétre
 * post-conditions : retourne le type unit (rien) aprés avoir
 *                   écrit le résultat de l'exécution de l'algopb sur tous
 *                   les cas de fin dans fout
 * auteur : EISTI
 * date création : 07/10/15
 *)
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
	
	(* jam "B-small-practice.in" "B-small.out" algolec fireflies *)
	(* jam "B-large-practice.in" "B-large.out" algolec fireflies *)