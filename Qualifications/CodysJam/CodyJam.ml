open List

type lprice = int list;;

(*
 * Nom : listeToString
 * But : Cree une string composée des éléments de la liste
 * Paramètres d'entrée : Une liste l   
 * Pré-conditions : l est une liste d'entiers
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let listeToString = fun liste -> fold_left(fun x y-> x ^ " " ^ string_of_int(y) ) "" liste;;

(*
 * Nom : suppr
 * But : Supprime un élément spécifique x de la liste
 * Paramètres d'entrée : x l'élement a supprimer
 						 l la liste	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec suppr x = fun l -> match l with
							(t::r) when (t = x) -> r
							|_-> (hd l) :: suppr x (tl l)

(*
 * Nom : lieNbr
 * But : Crée deux listes (prix bruts et soldés) rangés par ordre croissant
 * Paramètres d'entrée : l la liste totale, la listes lsold (prix soldés)
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let lieNbr x y = fun (l, lsold) -> match (l, lsold) with
								([],_) -> failwith "liste fausse"
								|_ -> ((suppr y (suppr x l)), (min x y) :: lsold);;


(*
 * Nom : mini
 * But : Trouve l'entier minimum de la liste
 * Paramètres d'entrée : une liste d'entiers l    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let mini = fun (l)-> match (l) with
					|(t::[]) -> t
					|(t::r) -> (fold_left (fun a b -> min a b) t r) ;;								




(*
 * Nom : separe
 * But : Ajoute pour chaque prix sa valeur brute dans ldep et sa valeur soldée dans lsold
 * Paramètres d'entrée : La liste totale l, 
 						 lsol et ldep deux listes vides 	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let separe = fun (l,lsold) -> let elemsold = mini(l) in
											  (lieNbr (find(fun x -> x = (elemsold*4)/3) l) elemsold (l,lsold));;


(*
 * Nom : mkCouple
 * But : Crée des couples de prix (bruts et réduits)
 * Paramètres d'entrée : La liste totale l, 
 						 lsol et ldep deux listes vides 	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec mkCouple = fun (l,lsold) -> match l with
								[]-> listeToString(sort compare lsold)
								|_-> mkCouple(separe(l,lsold));;


(*
 * Nom : mkListe/mkLigne
 * But : Cree une liste d'entiers
 * Paramètres d'entrée : nbr nul (stocke le nombre créé)
 						 liste vide
						 ligne, un string		    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)
let rec mkListe = fun (nbr,liste,ligne) -> match (nbr,liste,ligne) with
										(nbr,liste,"") -> liste @ [nbr]
										|(nbr,liste,ligne) when (Char.escaped(String.get ligne 0) = " ") -> mkListe(0,liste @ [nbr],(String.sub ligne 1 ((String.length ligne)-1)))			
										|(nbr,liste,ligne) -> mkListe((nbr*10) + int_of_string(Char.escaped(String.get ligne 0)) , liste, (String.sub ligne 1 ((String.length ligne)-1)));;

let mkLigne = fun flec -> let acc = input_line flec in
						let ligne = input_line flec in
							mkListe(0,[],ligne);; 
									
		
let algolec = fun flec -> mkLigne(flec) ;;
let algopb = fun liste -> mkCouple(liste,[]);;


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

(* Exemple d'utilisation *) 
(*jam "A-small-practice.in" "A-small.out" algolec algopb;;*)
(*jam "A-large-practice.in" "A-large.out" algolec algopb;;*)
	