
open List;;



(*
 * Nom : coupe
 * But : Récupère un nombre (sub) précis d'éléments (de type float) en début de liste 
 * Paramètres d'entrée : sub le nombre d'éléments à renvoyer
 						 lq la liste de float
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)
let rec coupe = fun (sub,lq) -> match lq with
							 |lq when sub = 0 -> []
							 |_ -> (hd lq)::coupe( (sub -1),(tl lq));;


(*
 * Nom : calcnewp/calcq/calcProb
 * But : Fait la multiplication des probabilité de réponses de chaque question par les probabilités de réponse de toutes les autres questions dans la liste
 * Paramètres d'entrée : lpIni les probabilités de la première question de la liste
 						 lq le reste de la liste de liste de probabilités
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)
let calcnewp = fun (pIni,lprob) -> fold_left(fun acc prob -> acc @ [pIni*.prob]) [] lprob;;

let calcq = fun lpIni lprob -> flatten(map (fun pIni -> calcnewp(pIni,lprob)) lpIni);; 		

let calcProb = fun lpIni lq -> fold_left(fun acc l -> calcq acc l) lpIni lq;;

(*
 * Nom : resFin
 * But : Renvoie les n (=sub) meilleures probabilités de la liste des produits de probabilités
 * Paramètres d'entrée : 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *) 

let resFin = fun (sub,lq) -> coupe(sub, (rev (sort compare (calcProb (hd lq) (tl lq)))));;

(*
 * Nom : calcult
 * But : Ajoute les probabilités (float) de la liste de probabilités (float)
 * Paramètres d'entrée : 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let calcult = fun lq -> fold_left(fun acc q -> acc +. q) 0. lq;;


(*
 * Nom : getn/addlistlist/spacelistcouple/callcouple/mkVar
 * But : Transforme les probabilités de réponses (dans une chaine de caractères) en liste de probabilités
 * Paramètres d'entrée : le fichier de lecture flec,
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec getn l n= match n with
	1 -> (hd l)
	|_-> (getn (tl l) (n-1)) ;;

let addlistlist (l, c, i) =
	match (List.rev(l),i) with
    ([],_) -> [[c;"" ; "";""]]
	|(a::r,1) -> List.rev ([(getn a 1)^c; getn a 2; getn a 3; getn a 4] :: r)
	|(a::r,2) -> List.rev ([getn a 1; (getn a 2)^c; getn a 3; getn a 4] :: r)
	|(a::r,3) -> List.rev ([getn a 1; getn a 2; (getn a 3)^c; getn a 4] :: r)
	|(a::r,4) -> List.rev ([getn a 1; getn a 2; getn a 3; (getn a 4)^c] :: r);;


let rec spacelistcouple (s,i,l,n,k) = if (i >= n) then l else
                                        match (s.[i]) with
	(' ') -> spacelistcouple(s,(i+1),l,n,k+1)
	|(_)-> spacelistcouple(s,(i+1),addlistlist(l,Char.escaped (s.[i]),k),n,k);;

let callcouple s = spacelistcouple (s,0,[],String.length(s),1);;

let rec mkVar = fun flec n -> match n with 
	0 -> []
	|_-> (callcouple (input_line flec)) @ (mkVar flec (n-1));;


(*
 * Nom : fList
 * But : Transforme la liste de string en liste de float
 * Paramètres d'entrée : une liste de string lq
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let fList = fun ql -> map(fun ql-> float_of_string(ql)) ql;;

(*
 * Nom : mkSandQ
 * But : Cree une liste de liste de float à partir d'une liste de liste de string
 * Paramètres d'entrée : la liste de liste de string l
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let sTof = fun l -> fold_left(fun acc l -> acc @ [fList(l)]) [] l;;

(*
 * Nom : mkSandQ/mkLigne
 * But : Cree la liste de probabilités de reponses (float) et le nombre de soumissions (entiers) à partir d'une string du fichier de lecture
 * Paramètres d'entrée : la chaine de caractères s
 						 i le compteur
 						 n la taille de s
 						 res1 et res2, respectivement le nombre de questions et le nombre de soumissions, à déterminer
 * Pré-conditions : flec ouvert
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec mkSandQ = fun (s,i,n,res1,res2) -> if (i >= n) then (res1,res2) else
									match (s.[i]) with
									|(' ')-> mkSandQ(s,i+1,n,"",res1)
									|(_)-> mkSandQ(s,i+1,n,res1^(Char.escaped (s.[i])),res2);;


let mkLigne = fun (flec) -> let s =input_line flec in
 							 let n = mkSandQ(s,0,String.length(s),"","") in
							  	(int_of_string(snd n),sTof(mkVar flec (int_of_string(fst n))));;

(*
 * Nom : prodl
 * But : Trouve le nombre de combinaisons possibles pour la liste 
 * Paramètres d'entrée : lq une liste de 4 réponses
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)
let prodl = fun lq -> fold_left(fun acc l -> acc * (length l) ) 1 lq;;

(*
 * Nom : mkRes
 * But : Trouve la probabilité maximale de réussite suite de questions suivant le nombre de soumissions
 * Paramètres d'entrée : l1 et l2, 2 strings	    
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 03/11/2016
 *)
let mkRes = fun(sub,lq) -> if (sub >= prodl(lq)) then "1.000000000" else string_of_float(calcult(resFin(sub,lq)));;


let algolec = fun flec -> mkLigne(flec);;
let algopb = fun (sub,lq) -> mkRes(sub,lq);;


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