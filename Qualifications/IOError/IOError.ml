(*
 * Nom : IOError
 * Description : Qualifications Projet Google Code Jam
 * Language : Ocaml
 * Auteur : Valentin Urcun
 * Date : 10/10/16
 *)

open Char
open String
open Str
#load "str.cma";;

(*
 * Rôle : convertis une string de 'I' et 'O' en entier décimal
 * Paramètres : st -> chaine d'une binaire, à convertir
 * Pré-conditions : faux si st n'est pas un nombre binaire
 *)
let rec binToInt = fun st -> let l = length st in if l = 0 then 0 else match (get st (l-1)) with 
	 'I' -> 1 + 2 * binToInt(sub st 0 (l-1)) 
	 |_-> 2 * binToInt(sub st 0 (l-1));;

(*
 * Rôle : sépare une string en n sous-string
 * Paramètres : st -> string à séparer
 *				n -> nombre de sous-listes souhaitées
 * Pré-conditions : n > 0
 *)
let rec separeWords = fun st n -> match (n) with
 	-1 -> [] 
 	|_ -> (separeWords (string_before st (8*n)) (n-1)) @ [(string_after st (8*n))];;

(*
 * Rôle : converti une liste de string "IO" en une liste des caractères correspondants
 * Paramètres : l -> liste à convertir 
 *				n -> nombre de caractères
 * Pré-conditions : n > 0
 *)
let translateStringList = fun l n -> List.map (fun st -> chr(binToInt(st))) (separeWords l (n-1));;

(*
 * Rôle : traduit une string de "IO" en une phrase
 * Paramètres : code -> string à convertir 
 				taille -> nombre de caractères de la phrase finale
 * Pré-conditions : code uniquement composée de 'I' et 'O'
 					taille > 0
 *)
let translate = fun code taille -> concat "" (List.map (String.make 1) ((translateStringList code (int_of_string taille))));; 

(*
 * Rôle : algorithme de lecture -> récupère, pour chaque case : la string à convertir et sa taille
 * Paramètres : fichier d'entrée
 *)
let algolec = fun flec -> (input_line flec, input_line flec);; 


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
|n -> let (taille,code) = (algolec flec) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb taille code)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;


let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;


(*jam "A-small-practice.in" "output.out" algolec translate;;*)





