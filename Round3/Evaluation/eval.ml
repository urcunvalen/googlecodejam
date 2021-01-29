open List;;

type assign = {vari : string ; lvar : string list};;


(*
 * Nom : verif
 * But : Verifie si toutes les variables dont dépend un élément sont comprises dans la liste des variables valides
 * Paramètres d'entrée : Une liste lmont
 						 
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let verif = fun (lvar,lok) -> (fold_left(fun acc var -> (fold_left(fun acc ok -> acc ||(var = ok.vari )) false lok) && acc) true lvar);;

(*
 * Nom : mkNewDepth
 * But : Cree la liste des assigns qui ont une liste de variables égale en nombre, au nombre depth 
 * Paramètres d'entrée : depth, le nombre de variables composant un assign voulu
						 ltotal la liste des assign 
						 lok la liste résultante
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let mkNewDepth = fun (depth,ltotal,lok) -> fold_left(fun res elemtotal -> if ((length(elemtotal.lvar) = depth ) && (if depth != 0 then (verif(elemtotal.lvar,lok)) else true) ) then res @ [elemtotal] else res) [] ltotal;; 

(*
 * Nom : maxdepth
 * But : Supprime d'une liste d'assign ltotal les élements communs avec lok
 * Paramètres d'entrée : ltotal et lok, listes d'assign
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let suppr = fun (ltotal,lok) -> fold_left(fun acc elemtotal -> if (fold_left(fun acc elemlok -> acc && (elemtotal.vari <> elemlok.vari)) true lok ) then acc @ [elemtotal] else acc) [] ltotal;;

(*
 * Nom : maxdepth
 * But : Trouve la variable qui dépend du plus de variables et renvoie ce nombre de variables
 * Paramètres d'entrée : ltotal la liste des assign
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let maxdepth = fun ltotal -> fold_left(fun acc total -> max acc (length total.lvar)) 0 ltotal;;

(*
 * Nom : repeatDepth
 * But : Renvoie "GOOD" si la configuration des expressions est possible, "BAD" sinon 
 * Paramètres d'entrée : ltotal la liste des assign
 						 depth, l'indice correspondant au nombre de variables dont dépend un assign
 						 lok, la liste des assigns valides
 						 maxi, l'indice maximal possible +1 pour depth (si depth = maxi, alors faux)
 * Pré-conditions : maxi > depth = 0 
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec repeatDepth = fun (ltotal,depth,lok,maxi) ->  match depth with
								|depth when (maxi = depth) -> "BAD"
								|_ when (ltotal = []) -> "GOOD"
								|_-> let lDepth = mkNewDepth(depth,ltotal,lok) in
										let newlok = lok @ lDepth in
												if (lDepth <> []) then repeatDepth( suppr(ltotal,lDepth),1,newlok,maxi) else 
													repeatDepth(ltotal,depth +1,newlok,maxi);;



(*
 * Nom : createList/mkListe/mkLigne
 * But : Crée une liste d'assign(= {vari : la variable de départ ; lvar : la liste des variables dont vari dépend}) à partir d'un string de flec
 * Paramètres d'entrée : Un fichier d'entrée flec
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let rec createList = fun (s,i,res,fin) -> match s.[i] with						
										|(')') -> {vari = (snd res) ; lvar = (if (s.[i-1] != '(' ) then (fin @ [fst res]) else [])}
										|('=')-> createList(s,i+1,("",fst res),fin)
										|('(') -> createList(s,i+1,("",snd res),fin)
										|(',') -> createList(s,i+1,("",snd res),fin @ [fst res])
										|_-> createList(s,i+1,((fst res)^(Char.escaped (s.[i])),snd res),fin);;


let rec mkListe = fun (s,flec) -> match s with
								|0 -> []
								|_-> createList(input_line flec,0,("",""),[]) :: mkListe(s-1,flec);;

let mkLigne = fun (flec) -> let s =input_line flec in
							mkListe(int_of_string(s),flec);; 

(*
 * Nom : mkRes
 * But : Renvoie "GOOD" si la configuration des expressions est possible, "BAD" sinon 
 * Paramètres d'entrée : Une liste d'expressions de type assign
 * Pré-conditions : /
 * Post-conditions : /
 * Auteur : Max JEAN
 * Date de création : 06/11/2016
 *)

let mkRes = fun ltotal -> repeatDepth(ltotal,0,[],maxdepth(ltotal)+1) 

let algolec = fun flec -> mkLigne(flec);;
let algopb = fun ltotal -> mkRes(ltotal);;



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
(*jam "C-small-practice.in" "C-small.out" algolec algopb;;*)
(*jam "C-large-practice.in" "C-large.out" algolec algopb;;*)