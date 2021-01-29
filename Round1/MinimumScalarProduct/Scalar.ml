(*
 * Nom : Scalar
 * Description : Round 1 Projet Google Code Jam 
 * Language : Ocaml
 * Auteur : Valentin Urcun
 * Date : 15/10/16
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
let addlist (l,c) = match (List.rev(l)) with
    [] -> [c]
  |(a::r) -> List.rev((a^c) :: r);;

let rec spacelist (s,i,l,n) = if (i >= n ) then l else
  match (i,s.[i]) with
   |(_,' ') -> spacelist (s,(i+2),l@[Char.escaped (s.[i+1])],n)
   |_-> spacelist (s,(i+1),addlist(l,Char.escaped (s.[i])),n);;
                         
let call s = spacelist (s,0,[],String.length(s));;


(*
 * Nom : getSortedList
 * Rôle : renvoie la liste 1 triée et l'inverse de la liste 2 triée
 * Paramètres : v1 -> liste 1 
 				v2 -> liste 2
 *)
let getSortedList = fun v1 v2 -> (List.sort compare v1, (List.rev (List.sort compare v2)));;


(*
 * Nom : getScalaire
 * Rôle : effecture le produit scalaire entre 2 listes d'entiers
 * Paramètres : v1, v2 -> 2 listes
 *)
let rec getScalaire = fun v1 v2 -> let (a,b) = (getSortedList v1 v2) in match (a,b) with 
	(t::l,d::r) -> t*d + (getScalaire l r)
	|_ -> 0;;


(*
 * Nom : resolve
 * Rôle : renvoie le produit scalaire minimum entre 2 listes d'entiers
 * Paramètres : flec -> fichier d'entrée
 *)
let resolve = fun v1 v2 -> string_of_int(getScalaire (List.map (int_of_string) (call v1)) (List.map (int_of_string) (call v2)));;


(*
 * Nom : algolec
 * Rôle : algorithme de lecture
 * Paramètres : flec -> fichier d'entrée
 *)
let algolec = fun flec -> let x =  input_line flec in 
							(input_line flec,input_line flec);;


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
|n -> let (v1,v2) = (algolec flec) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb v1 v2)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;


(*jam "A-small-practice.in" "output.out" algolec resolve;;*)

(*jam "A-large-practice.in" "output.out" algolec resolve;;*)






