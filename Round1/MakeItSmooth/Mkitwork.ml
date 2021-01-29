open List;;

(*
 * Nom : smaller
 * Paramètres d'entrée : Un couple de couples d'entiers
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
*)

let smaller = fun ((a,b),(c,d)) -> if c > a then (a,b) else (c,d);;

(*
 * Nom : calcReval
 * But : Définit le coût minimal entre deux valeurs en terme d'ajouts et modifications de termes
 * Pré-conditions : Trois entiers t,u et udep avec t >= u >= udep,
 				    i et m deux entiers
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec calcReval = fun (t,u,i,m,udep) -> match (t,u,i,m,udep) with
								 			|(t,u,i,m,udep) when (u = t) ->  ((udep - u ),(udep - u))
											|(t,u,i,m,udep) ->  smaller(((((abs(u - t )-1 )/m)*i  + (udep - u)  ),(udep - u)),calcReval(t,u-1 ,i,m,udep));;
							

(*
 * Nom : calcMax/calcMin
 * But : Renvoie le maximum (resp minimum) d'une liste
 * Paramètres d'entrée : Une liste de pixels (entier) lpix 				    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let calcMax  = fun lpix -> fold_left(fun acc x -> if acc < x then x else acc) (hd lpix) lpix ;;

let calcMin = fun lpix -> fold_left(fun acc x -> if acc < x then acc else x) (hd lpix) lpix ;;	

(*
 * Nom : maxdif
 * But : Renvoie la différence enre le maximum et le minimum d'une liste
 * Paramètres d'entrée : Une liste d'entiers lpix, le maximum de la liste et le minimum				    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec maxdif = fun (lpix,maxi,mini) -> match (lpix,maxi,mini) with
										(lpix,maxi,mini) when (maxi = mini) -> fold_left(fun acc pix -> acc + abs(maxi - pix)) 0 lpix	
										|_-> min (fold_left(fun acc pix -> acc +  abs(maxi - pix)) 0 lpix) (maxdif(lpix,maxi -1, mini));;

(*
 * Nom : minOP
 * But : Renvoie le minimum de coût pour une liste d'entiers
 * Paramètres d'entrée : 3 entiers soient un coût d, un coût i et un écart m
 						 une liste de pixels (entier) lpix 				    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec minOP = fun d i m lpix -> match lpix with
								[]-> 0
								|(t::[]) -> 0
								|(t::u::r) when abs(t-u) <= m -> 0 + (minOP d i m (u::r))
								|(t::u::r) when m = 0 -> 
											let x = min d (maxdif (lpix,calcMax(lpix),calcMin(lpix))) in
											 if (x = d) then x + (minOP d i m (t::r)) else x
								|(t::u::r) -> let res = calcReval ((min t u),(max t u),i,m,(max t u)) in
											   let k =(min d (fst (res))) in 
												(if k = d then k + (minOP d i m (t::r)) else  k + (minOP d i m ((if t < u then (u - (snd res)) else (if (snd res) = 0 then (u + m) else (u  + (snd res))))::r)));;
				


(*
 * Nom : ssListe
 * But : Cree une liste raccourcie d'une liste l
 * Paramètres d'entrée : Une liste l, un indice maximal n
 * Pré-conditions: /			    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let ssListe = fun l n -> fold_left(fun newl pix -> if length(newl) = (n+1) then newl else newl @ [pix]) [] l;;

(*
 * Nom : valFin
 * But : Additionne pour chaque élement le coût minimal ses termes à gauche et à droite
 * Pré-conditions : 3 entiers soient un coût d, un coût i et un écart m
 					Une liste de pixels (entier) lpix
 					L'indice de position k du pixel étudié  			    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec valFin = fun d i m l k -> let res1 = (minOP d i m (rev (ssListe l k ))) in
									let res2 = (minOP d i m (rev (ssListe (rev l) (length(l)- k -1 )))) in
								 			   (res1 + res2);;


(*
 * Nom : calcCostmin
 * But : Renvoie le coût minimum pour une liste de pixels
 * Pré-conditions : 3 entiers (coût d, coût i et écart m)
 					Une liste de pixels (entier) lpix 				    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let calcCostmin = fun (d,i,m,lpix) -> match lpix with
								     []-> 0
									 |_-> let res = fold_left(fun (acc,len) pix-> (min acc (valFin d i m lpix len),len +1)) ((valFin d i m lpix 0),0) lpix in
												   (fst res);;

(*
 * Nom : mkListe
 * But : Cree une liste de pixels (entiers)
 * Paramètres d'entrée : nbr nul (stocke le nombre créé)
 						 liste vide
						 ligne, un string		    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let rec mkListe = fun (nbr,liste,ligne)  -> match (nbr,liste,ligne) with
										(nbr,liste,"") -> liste @ [nbr]
										|(nbr,liste,ligne) when (Char.escaped(String.get ligne 0) = " ") -> mkListe(0,liste @ [nbr],(String.sub ligne 1 ((String.length ligne)-1)))			
										|(nbr,liste,ligne) -> mkListe((nbr*10) + int_of_string(Char.escaped(String.get ligne 0)) , liste, (String.sub ligne 1 ((String.length ligne)-1)));;


(*
 * Nom : mkDIMN
 * But : Cree les coûts d, i et l'écart m
 * Paramètres d'entrée : l1, un string	
 * Préconditions : /    
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let mkDIMN = fun l1 -> let liste = mkListe(0,[],l1) in
						let d = (hd liste) in
						let i = nth liste 1 in
						let m = nth liste 2 in
						(d,i,m);;


(*
 * Nom : mkVar/mkLigne
 * But : Cree une liste de pixels (entiers), les coûts d, i et l'écart m
 * Paramètres d'entrée : l1 et l2, 2 strings	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)

let mkVar = fun (l1,l2) -> (mkDIMN(l1),mkListe(0,[],l2));; 


let mkLigne = fun flec -> let ligne1 = input_line flec in
						  let ligne2 = input_line flec in 
							mkVar(ligne1,ligne2);;

									
let algolec = fun flec -> mkLigne(flec) ;;
let algopb = fun (l1,lpix) -> match (l1,lpix) with 
							((d,i,m),lpix) -> string_of_int(calcCostmin(d,i,m,lpix));;


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


(*Exemple d'utilisation*)
(*jam "B-small-practice.in" "B-small.out" algolec algopb;;*)
	
