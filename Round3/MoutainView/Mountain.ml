open List;;

type montagne = {num : int ; height : int ; best : int};;


(*
 * Nom : verif
 * But : Verifie si la configuration de la chaîne est possible
 * Paramètres d'entrée : la liste des montagnes lmont
 						 stock la montagne la plus haute vue par la premiere montagne
 						 (qui devient la montagne la plus haute vue par stock)	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec verif = fun (lmont,stock) -> match (lmont,stock) with
							|(t::[],_)-> true
							|((t::r),stock) when (t.num = stock) -> verif(r,t.best)
							|((t::r),stock) when ((t.best > stock) || fold_left (fun acc suiv -> acc || (t.best > suiv.num && (suiv.best > t.best))) false lmont)  -> false
							|_-> verif((tl lmont),stock);; 


(*
 * Nom : suppr
 * But : Supprime l'élément spécifique de la liste
 * Paramètres d'entrée : Une liste de montagnes lmont
 						 Une montagne elem  
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let suppr = fun (lmont,elem) -> fold_left(fun acc mont -> if mont = (find (fun x -> x.best = elem) lmont) then acc else acc @ [mont]) [] lmont;;

(*
 * Nom : resolheight
 * But : Reorganise la liste de montagne dans l'ordre des montagnes apercevants les montagnes les plus lointaines
 * Paramètres d'entrée : la liste initiale de montagnes lmont
 						 la liste des numéros de montagne (entiers) dans l'ordre des montagnes apercevants les montagnes les plus lointaines
 						 res la liste résultante (vide au départ)	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec resolheight = fun (lmont,lclass,res) -> match lmont with
											|[] -> res
											|_-> resolheight(suppr(lmont,(hd lclass)),(tl lclass),res @ [find (fun x -> x.best = (hd lclass)) lmont])

(*
 * Nom : bestmont
 * But : Attribue a la premiere montagne et a celle quelle voit comme la plus haute une hauteur maximale 
 		 et ainsi de suite (en repartant de la plus haute pour la montagne 1) jusqu'a la fin de la chaine
 * Paramètres d'entrée : la liste des montagnes lmont	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let bestmont = fun lmont -> fold_left(fun (bestmontagne,acc) mont -> if ((acc.best = mont.num) || (mont.num = 1)) then (bestmontagne @ [{mont with height = 1000000000}],mont) else (bestmontagne @ [{mont with height = 0}],acc)) ([],(hd lmont)) lmont ;;



(*
 * Nom : mkOrder
 * But : Trie la chaine de montagne suivant la montagne considérée comme la plus grande 
 		 (Renvoie la liste vide si la configuration est impossible)
 * Paramètres d'entrée : Une liste de montagnes lmont
 						 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let  mkOrder = fun lmont -> let evalmont = bestmont(lmont) in
										    match lmont with
											|_ when not (verif(lmont,(hd lmont).best)) -> []
											|_-> resolheight(fst(evalmont),(rev (sort compare (fold_left(fun acc mont -> acc @ [mont.best]) [] (fst evalmont)))),[]);;


(*
 * Nom : findbestmont
 * But : Trouve la montagne associée à son emplacement dans la chaîne de montagne (num)
 * Paramètres d'entrée : Le numéro elem
 						 La liste de montagnes lmont  
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let findbestmont = fun (elem,lmont) -> find(fun x -> x.num = elem.best) lmont;;


(*
 * Nom : trouveheight
 * But : Calcule la hauteur d'un mont relative à sa montagne perçue comme la plus grande et à la pente
 * Paramètres d'entrée : La montagne etudiee mont
 						 La pente 
 						 La plus grande montagne associée à mont 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let trouveheight = fun (mont,pente,bestmont) -> bestmont.height - pente*(bestmont.num - mont.num);;


(*
 * Nom : calcVal
 * But : Calcule la hauteur de chaque montagne de la liste
 * Paramètres d'entrée : Une liste de montagnes lmont
 						 Une pente (entier)
 						 La nouvelle liste de montagnes résultante newl
 * Pré-conditions : pente >= 0 
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec calcVal = fun (lmont,pente,newl) -> match lmont with
											|(t::[])-> newl @ [{t with height = trouveheight(t,pente,findbestmont(t,newl))}]
											|(t::r) when ((hd lmont).height = 0) -> calcVal(r,pente +1, newl @ [{t with height = trouveheight(t,pente,findbestmont(t,newl))}])
											|_-> calcVal((tl lmont),pente +1, newl @ [(hd lmont)]);;



(*
 * Nom : transfo
 * But : Réécrit la liste de montagne sous forme de string
 * Paramètres d'entrée : Une liste lmont
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let transfo = fun l -> fold_left(fun s elem -> s^string_of_int(elem)^" ") "" l;;


(*
 * Nom : classeNum
 * But : Classe la liste par emplacement dans la chaine de montagne (num)
 * Paramètres d'entrée : Une liste, lmont
 						 Un compteur, compte
 * Pré-conditions : compte = 0
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec classeNum = fun (compte,lmont) -> match compte with
						|compte when compte = (length lmont + 1)-> []
						|_-> let k = find(fun x -> x.num = compte) lmont in
							 k.height :: classeNum(compte +1, lmont);;


(*
 * Nom : suppr
 * But : Définit la hauteur des montagnes sous forme de chaîne de caractères de la liste si la configuration est possible
 * Paramètres d'entrée : Une liste lmont	 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let calcFin = fun lmont -> let lclass = mkOrder(lmont) in 
								if (lclass = []) then "Impossible" else transfo(classeNum(1,calcVal(lclass,0,[])));;


(*
 * Nom : mkListe/mkLigne
 * But : Crée une liste de montagnes à partir d'un fichier de lecture
 * Paramètres d'entrée : Un fichier de lecture flec
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec mkListe = fun (nbr,liste,ligne,acc)  -> match ligne with
										"" -> liste @ [{num = acc; height = 0; best = nbr}] @ [{num = nbr; height = 1000000000; best = nbr +1}] 
										|_ when (Char.escaped(String.get ligne 0) = " ") -> mkListe(0,liste @ [{num = acc; height = 0; best =nbr}],(String.sub ligne 1 ((String.length ligne)-1)),acc +1)			
										|_ -> mkListe((nbr*10) + int_of_string(Char.escaped(String.get ligne 0)) , liste, (String.sub ligne 1 ((String.length ligne)-1)),acc);;


let mkLigne = fun (flec) -> let acc = input_line flec in
 								let s =input_line flec in
 							 mkListe(0,[],s,1);;

let algolec = fun flec -> mkLigne(flec);;
let algopb = fun lmont -> calcFin(lmont);;

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
(*jam "C-small-practice.in" "C-small.out" algolec algopb;;*)
