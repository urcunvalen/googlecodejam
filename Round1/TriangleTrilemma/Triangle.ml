(*
 * Nom : Triangle
 * Description : Round 1 Projet Google Code Jam 
 * Language : Ocaml
 * Auteur : Valentin Urcun
 * Date : 18/10/16
 *)

(*
 * nom : callcouple
 * pre-conditions : s : string, string contenant des éléments séparés par des espaces
 * post-conditions : renvoi une liste de couple de string, dont chaque élément correspond à un couple de caractère ou de série de caractère de s délimités par des espaces
 *
 * nom : spacelistcouple
 * pre-conditions : s : string, string envoyé par call ; i : int; itérateur de la position dans la string ; l : string list, liste récursivement accumulée avec les éléments de s ; n : int, taille de s, b : bool, capteur de changement de couple
 * post-conditions : accumule de façon récursive les charactères de s dans la liste l, selon 3 critère : le charactère est un espace ou pas/le capteur b 
 *
 * nom : addlist
 * pre-conditions : l : string list, la liste à laquelle on doit ajouter le charactère (c : string)
 * post-conditions : retourne une liste rempli de l'élément (c,"") si l est vide, sinon concat l'élément c au dernier élément de l en position (concat^c,autre) ou (autre,concat^c) selon le capteur b
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let addlistcouple (l,c,b) = match (List.rev(l),b) with
    ([],_) -> [(c,"")]
   |(a::r,true) -> List.rev(((fst(a),snd(a)^c)) :: r)
   |(a::r,false) -> List.rev (((fst(a)^c,snd(a))) :: r)
;;
let rec spacelistcouple (s,i,l,n,b) = if (i >= n) then l else
                                        match (i,s.[i],b) with
                                          (_,' ',false) -> spacelistcouple (s,(i+1),l,n,true)
                                         |(_,' ',true) -> spacelistcouple (s,(i+2),l@[(Char.escaped (s.[i+1]),"")],n,false)
                                         |(_,_,true)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),b),n,b)
                                         |(_,_,false) -> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),b),n,b)
;;
let callcouple s = spacelistcouple (s,0,[],String.length(s),false)
;;

type point = {x : int; y : int};; (* point; x,y : coordonnées *)


(*
 * Nom : samePoint
 * Rôle : renvoie true si 2 points ont strictement les mêmes coordonnées
 * Paramètres : a,b -> 2 points
 *)
let samePoint = fun a b -> (a.x = b.x) && (a.y = b.y);;


(*
 * Nom : together
 * Rôle : true si au moins 2 points sur 3 sont confondus
 * Paramètres : a,b,c -> 3 points
 *)
let together = fun a b c -> (samePoint a b) || (samePoint b c) || (samePoint a c);;


(*
 * Nom : colinear
 * Rôle : renvoie true si 2 points vecteurs sont colinéaires
 * Paramètres : a,b -> 2 vecteurs
 *)
let colinear = fun a b -> (a.x * b.y) = (a.y * b.x);;


(*
 * Nom : getVector
 * Rôle : calcul les coordonnées d'un vecteur en fonction des coordonnées de ses points
 * Paramètres : a,b -> 2 points
 *)
let getVector = fun a b -> {x = a.x - b.x; y = a.y - b.y};;


(*
 * Nom : aligned
 * Rôle : détermine si 3 points sont alignés
 * Paramètres : a,b,c -> 3 points
 *)
let aligned = fun a b c -> (colinear (getVector a b) (getVector a c));;


(*
 * Nom : isTriangle
 * Rôle : déterminer si 3 points forment bien un triangle
 * Paramètres : a,b,c -> 3 points
 *)
let isTriangle = fun a b c -> not((aligned a b c)) && not((together a b c));;


(*
 * Nom : lengthVector
 * Rôle : calcul la distance entre 2 points
 * Paramètres : a,b -> 2 points
 *)
let lengthVector = fun a b ->  sqrt (  ((float_of_int b.x) -. (float_of_int a.x))**2. +. ((float_of_int b.y) -. (float_of_int a.y))**2.);;


(*
 * Nom : isoceles
 * Rôle : déterminer si 3 points déterminent un triangle isocèle
 * Paramètres : a,b,c -> 3 points
 *)
let isosceles = fun a b c -> let ab = (lengthVector a b) in let bc = (lengthVector b c) in let ac = (lengthVector a c) in 
								((ab = bc) || (ab = ac) || (ac=bc));;

(*
 * Nom : isorSca
 * Rôle : renvoie le type du triangle dans une string -> "isosceles" ou "scalene"
 * Paramètres : a,b,c -> 3 points formant un triangle
 *)
let isorSca = fun a b c -> match (isosceles a b c) with 
	true -> "isosceles "
	|false -> "scalene "


(*
 * Nom : greater
 * Rôle : détermine si un angle est obtus, si oui, renvoie 1, renvoie 0 sinon
 * Paramètres : a -> angle
 *)
let greater = fun a -> match a with 
	a when a > 1.57079632679489656 -> 1 (* 1.57079632679489656 = pi en radian*)
	|_ -> 0;;


(*
 * Nom : countGreaterThanRight
 * Rôle : compte le nombre d'angle obtus d'un triangle
 * Paramètres : a,b,c -> 3 points formant un triangle
 *)
let countGreaterThanRight = fun a b c -> greater a + greater b + greater c;;


(*
 * Nom : hasARightAngle
 * Rôle : détermine si un triangle a un angle droit
 * Paramètres : x,y,z -> 3 angles
 *)
let hasARightAngle = fun x y z -> let right = 1.57079632679489656 in (x=right || y=right || z=right);;


(*
 * Nom : scalar
 * Rôle : calcule le produit scalaire de 2 points
 * Paramètres : a,b -> 2 points
 *)
let scalar = fun a b -> a.x * b.x + a.y * b.y;; 


(*
 * Nom : getAngle
 * Rôle : calcule la valeur de l'angle formés par les vecteurs ab et ac
 * Paramètres : a,b,c -> 3 vecteurs
 *)
let getAngle = fun a b c -> acos (( float_of_int (scalar (getVector a b) (getVector a c))) /. ((lengthVector a b) *. (lengthVector a c)));;


(*
 * Nom : getAngles
 * Rôle : retourne la valeur des 3 angles d'un triangle
 * Paramètres : a,b,c -> 3 points formant un triangle
 *)
let getAngles = fun a b c -> (getAngle a b c, getAngle c a b, getAngle b c a);;


(*
 * Nom : acObtuRi
 * Rôle : renvoie "right" si un triangle est droit, "obtuse" s'il est obtus, "acute" sinon
 * Paramètres : a,b,c -> 3 points formant un triangle
 *)
let acObtuRi = fun a b c -> let (x,y,z) = (getAngles a b c) in 
								if (hasARightAngle x y z) then "right " 
								else 
									match (countGreaterThanRight x y z) with 
										0 -> "acute "
										|_ -> "obtuse ";;


(*
 * Nom : getKnow
 * Rôle : renvoie les caractéristiques d'un triangle formés par 3 points, "not a triangle" s'il n'en forment pas un
 * Paramètres : a,b,c -> 3 points
 *)
let getKnow = fun a b c -> match (isTriangle a b c) with 
	false -> "not a triangle"
	|true -> (isorSca a b c)^(acObtuRi a b c)^"triangle";;


(*
 * Nom : sti
 * Rôle : converti une string en entier
 * Paramètres : x -> string à convertir
 *)
let sti = fun x -> int_of_string x;;


(*
 * Nom : getCoord
 * Rôle : récupère les coordonnées de 3 points d'une liste 
 * Paramètres : l -> liste de coordonnées
 *)
let getCoord = fun l -> match (l) with 
	((a,b)::(c,d)::(e,f)::[]) -> ({x=sti a; y=sti b},{x=sti c; y=sti d}, {x=sti e; y=sti f})
	|_ -> ({x=0; y=0},{x=0; y=0}, {x=0; y=0});;


(*
 * Nom : getPoints
 * Rôle : récupère les coordonnées de 3 points d'une string 
 * Paramètres : st -> string
 *)
let getPoints = fun st ->  (getCoord (callcouple st));;


(*
 * Nom : resolve
 * Rôle : renvoie toutes les caractéristiques du triangle d'nu case
 * Paramètres : st -> string
 *)
let resolve = fun st -> let (a,b,c) = (getPoints st) in 
							(getKnow a b c);;


(*
 * nom : jam
 * pre-conditions : fin,fout  2 noms de fichiers
 * pre-conditions : fonction algolec (unit_channel -> `a),
 *                  qui lit le contenu d'un cas de fin,
 * pre-conditions : fonction algopp (`a -> string),
 *                  qui renvoie la solution du cas passé en paramêtre
 * post-conditions : retourne le type unit (rien) après avoir
 *                   écrit le résultat de l'exécution de l'algopb sur tous
 *                   les cas de fin dans fout
 * auteur : EISTI
 * date création : 07/10/15
 *)
let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n -> let line = (algolec flec) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb line)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;

(*jam "A-small-practice.in" "output.out" input_line resolve;;*)

 (*jam "A-large-practice.in" "output.out" input_line resolve;;*)













