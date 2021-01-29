(*
 * Nom : Cable
 * Description : Round 1 Projet Google Code Jam 
 * Language : Ocaml
 * Auteur : Valentin Urcun
 * Date : 20/10/16
 *)

(*
 * nom : call
 * pre-conditions : s : string, string contenant des éléments séparés par des espaces
 * post-conditions : renvoi une liste de string, dont chaque élément correspond à un caractère ou une série de caractère de s délimités par des espaces
 *
 * nom : spacelist
 * pre-conditions : s : string, string envoyé par call ; i : int; itérateur de la position dans la string ; l : string list, liste récursivement accumulée avec les éléments de s ; n : int, taille de s
 * post-conditions : accumule de façon récursive les charactères de s dans la liste l, selon 2 critère : le charactère est un espace ou pas
 *
 * nom : addlist
 * pre-conditions : l : string list, la liste à laquelle on doit ajouter le charactère (c : string)
 * post-conditions : retourne une liste rempli de l'élément c si l est vide, sinon concat l'élément c au dernier élément de l
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let addlistcouple (l,c,b) = match (List.rev(l),b) with
    ([],_) -> [(c,"")]
   |(a::r,true) -> List.rev(((fst(a),snd(a)^c)) :: r)
   |(a::r,false) -> List.rev (((fst(a)^c,snd(a))) :: r);;

let rec spacelistcouple (s,i,l,n,b) = if (i >= n) then l else
                                        match (i,s.[i],b) with
                                          (_,' ',false) -> spacelistcouple (s,(i+1),l,n,true)
                                         |(_,' ',true) -> spacelistcouple (s,(i+2),l@[(Char.escaped (s.[i+1]),"")],n,false)
                                         |(_,_,true)-> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),b),n,b)
                                         |(_,_,false) -> spacelistcouple (s,(i+1),addlistcouple(l,Char.escaped (s.[i]),b),n,b);;

let callcouple s = spacelistcouple (s,0,[],String.length(s),false);;


(*
 * Nom : cross
 * Rôle : détermine si 2 cêbles se croisent
 * Paramètres : a,b -> disposition des câbles
 *)
let cross = fun a b -> match (a,b) with 
	((x,y),(i,j)) when (x < i) && (y > j) -> true 
	|_ -> false;;


(*
 * Nom : getCrossed
 * Rôle : détermine combien de croisements sont engendrés par un cable
 * Paramètres : l -> liste de cables
 				m -> cable dont on veut les croisements
 *)
let getCrossesOfCable = fun l m -> List.fold_left (fun a b -> if (cross m b) then a + 1 else a) 0 l;;


(*
 * Nom : resolve
 * Rôle : compte tous les croisements "visuels" de tous les cables du case et retourne le résultat sous forme de string
 * Paramètres : l -> liste des cables de case
 *)
let resolve = fun l -> string_of_int (List.fold_left (fun a b -> a + (getCrossesOfCable l b)) 0 l);;


(*
 * Nom : to_int
 * Rôle : converti un couple de string en couple d'entier
 * Paramètres : couple de string
 *)
let to_int = function 
	|(a,b) -> (int_of_string a, int_of_string b);;


(*
 * Nom : algolec
 * Rôle : Algorithme de lecture, renvoie la liste de tous les cables du case
 * Paramètres : flec -> fichier de lecture
 				n -> nombre de câbles
 *)
let rec algolec = fun flec n -> match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (algolec flec (n-1));;


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
|n -> let l = (List.map (to_int) (algolec flec (int_of_string (input_line flec)))) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb l)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;       
		close_out fecr;
	end;;


(*jam "A-large-practice.in" "output.out" algolec resolve;;*)