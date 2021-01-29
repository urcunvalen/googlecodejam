(*
 * nom : dec_of_int
 * pre-conditions : n : int, le nombre dont on cherche la représentation binaire
 * post-conditions : retourne un int représentant le nombre n sous forme binaire
 *
 * nom : puiss 
 * pre-conditions : a : int, le nombre à mettre à la puissance b : int
 * post-conditions : retourne un int 
 *
 * nom : puiss_max
 * pre-conditions : n : int, le nombre dont on cherche la puissance la plus petite (i : int) de 2 le majorant
 * post-conditions : retourne un int
 *
 * nom : call_dec
 * pre-conditions : n : int, l'entier dont on veut la représentation binaire; i : int, la puissance de 2 que l'on va enlever à n et res : int, la représentation binaire de n
 * post-conditions : retourne la représentation binaire de n, en enlevant récursivement (2^i) à n si (n > (2^i)) et en ajoutant à res : (10^i)
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let puiss (a,b) = int_of_float((float_of_int (a)) ** (float_of_int (b)))
;;
let rec puiss_max (n,i) = if (puiss(2,i)) > n then i else puiss_max (n,i+1)
;;	
let rec call_dec (n,i,res) =
	if (n = 0) then 
		res
	else 
		let temp = (n - (puiss(2,i))) in 
		if (temp>=0) then
			call_dec(temp,i-1,res + puiss(10,i))
		else
			call_dec(n,i-1,res)
;;	
let dec_of_int (n)= let i = puiss_max (n,0) in call_dec (n,i,0)
;;
	
	
	
	
(*
 * nom : createlist
 * pre-conditions : n : int, la taille de la string (s : string) qui représentante le nombre de lampadaire à parcourir en binaire (1/2)
 * post-conditions : retourne une liste de string, chaque élément de la liste correspondant à une instructions à effectuer
 *							Il y a 2 types d'instructions : -les instructions fixes, qui sont globales à tout les cas (décalage et soustraction)
 *																	  -les instructions d'écriture : elles écrivent le nombre représenté par s sur les n premier lampadaire, et donc dépendent de n et s				
 *
 * nom : createlist_partN 
 * pre-conditions : n : int, taille de la string (s : string) ; res : string list, Liste des instructions d'écriture qui est récursivement crée ; i : int, itérateur
 * post-conditions : retourne la liste des instructions d'écriture
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)

 
let rec createlist_partN (n,s,res,i) =
	if (i = (n)) then 
		(*(  (((i-1)*100),0),1, (90,int_of_string(String.make 1 (String.get (s) (i-1)))))::res *)
		((string_of_int ((i-1)*100))^" 0 -> E 90 "^(String.make 1 (String.get (s) (i-1))))::res
	else
		let res = ((string_of_int ((i-1)*100))^" 0 -> E "^(string_of_int ((i)*100))^" "^(String.make 1(String.get (s) (i-1))))::res in 
		createlist_partN(n,s,res,i+1)		
		(*((((i-1)*100),0),1,(i*100,int_of_string(String.make 1 (String.get (s) (i-1)))))::res in *)
;;
let createlist (n,s) = 
let l = createlist_partN (n,s,[],1) in
let sous_A = "1 1 -> W 1 2" in (* soustraction *)
let sous_B = "1 2 -> W 2 1" in (* soustraction *)
let sous_C = "2 1 -> W 2 1" in (* soustraction *)
let sous_D = "2 2 -> W 2 2" in (* soustraction *)
let sous_FA = "1 0 -> E 42 2" in (* résolution *)
let a = "42 2 -> R" in (* résolution *)
let sous_FB = "2 0 -> E 10 0" in (* soustraction -> décalage *)
let copy_A = "10 1 -> E 20 0" in (* décalage *)
let copy_B = "10 2 -> E 40 0" in (* décalage *)
let copy_201 = "20 1 -> E 20 1" in (* décalage *)
let copy_202 = "20 2 -> E 40 1" in (* décalage *)
let copy_401 = "40 1 -> E 20 2" in (* décalage *)
let copy_402 = "40 2 -> E 40 2" in (* décalage *)
let copy_FA = "20 42 -> E 90 1" in (* décalage *)
let copy_FB = "40 42 -> E 90 2" in (* décalage *)
let change = "90 0 -> W 1 42" in (* décalage -> soustraction *)
a::sous_A::sous_B::sous_C::sous_D::sous_FA::sous_FB::copy_A::copy_B::copy_201::copy_202::copy_401::copy_402::copy_FA::copy_FB::change::l
;;
 

 
 
 
 
 (*
 * nom : program (fonction appelée par JAM)
 * pre-conditions : n : int, le nombre lu sur le fichier d'entrée, qui correspond au nombres de lampadaire à parcourir
 * post-conditions : retourne une string découpée par des "\n" séparant les lignes:  chaque ligne de la string correspond à une instructions à effectuer
 *							Les instructions sont envoyées pour être écrites sur le fichier de sortie.
 *							Le premier élément de la string renvoyé correspond aux nombres d'étapes qui vont suivre	
 *
 * nom : change_01_to_12
 * pre-conditions : sn : string, représentation binaire du nombre de lampadaire à parcourir
 * post-conditions : retourne une string représentant le nombre de lampadaire à parcourir en binaire (1/2) (les 0 sont des 1 et les 1 sont des 2)
 *
 * nom : string_of_list
 * pre-conditions : l : string list, la liste des instructions à effectuer
 * post-conditions : retourne une string concaténant toutes les instructions sous cette forme : "\nInstruction1\nInstruction2......"
 *
 * auteur : Dorian Gouzou
 * date création : 05/11/15
 *)
let rec change_01_to_12 (sn) = String.map (fun a -> if (a = '1') then '2' else '1' ) sn
;;
let string_of_list l = List.fold_left (fun res s -> "\n"^s^res) "" l
;;

let program (n) =
let n_bin = (change_01_to_12 (string_of_int(dec_of_int (n)))) in 
let taille = String.length (n_bin) in
let c = createlist (taille,n_bin) in
 ((string_of_int(16+taille)) ^ (string_of_list c)) 
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
 
let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
 0 -> ""
|n -> let line = (algolec flec) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (int_of_string line))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;

(* Exemple d'utilisation *) 
(* jam "C-small-practice.in" "C-small.out" input_line program;;*)
(* jam "C-large-practice.in" "C-large.out" input_line program;;*)