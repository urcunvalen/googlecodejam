(*
 * Nom : Hex
 * Description : Round 3 Projet Google Code Jam 
 * Language : Ocaml
 * Auteur : Valentin Urcun
 * Date : 05/11/16
 *)

(* ------------------------------------------------------- Calcul des configurations de fin de partie ------------------------------------------------------- *)

type hex = {pos : int; cost : int};; (* pion de l'hexagone : pos -> position; cost -> coût *)

(* 
 * Nom : middle
 * Rôle : récupère la valeur de la case centrale de l'hexagone
 *
 * Nom : getMiddle
 * Rôle : appelle middle avec les bons paramètres
 * Paramètres : s -> taille de l'hexagone
 *)
let rec middle = fun s n tmpS  -> match n with 
	0 -> tmpS
	|_-> tmpS + (middle s (n-1) (tmpS-1));;

let getMiddle = fun s -> 1 + (middle s ((s-1)/2) (s-1));;


(*
 * Nom : getEnding1
 * Rôle : récupère la disposition de fin de partie - de l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let rec ending1 = fun s h acc -> match acc with
	acc when acc=s -> [] 
	|_-> [acc+h] @ (ending1 s h (acc+1));;

let getEnding1 = fun s m -> let h = m - (s+1)/2 + 1 in (ending1 s h 0);;


(*
 * Nom : getEnding2
 * Rôle : récupère la disposition de fin de partie \ de l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let rec ending2 = fun h l -> match (l,List.rev l) with 
	(1::r,_) -> l
	|(a::r,b::l) -> ending2 (h-1) ((a-h) :: a :: r @ [b + h])
	|_-> [];;

let getEnding2 = fun s m -> ending2 s [m];;


(*
 * Nom : getEnding3
 * Rôle : récupère la disposition de fin de partie / de l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let rec ending3 = fun h l n-> match (l,List.rev l) with 
	(a::r,_) when a=n -> l
	|(a::r,b::l) -> ending3 (h-1) ((a-h) :: a :: r @ [b + h]) n 
	|_-> [];;

let getEnding3 = fun s m -> ending3 (s-1) [m] ((s+1)/2);;


(*
 * Nom : getAllEndings
 * Rôle : récupère les 3 positions de fin de partie 
 * Paramètres : s -> taille de l'hexagone
 *)
let getAllEndings = fun s -> let m = (getMiddle s) in (getEnding1 s m, getEnding2 s m, getEnding3 s m);; 



(* ------------------------------------------------------- Création des matrices de coûts  ------------------------------------------------------- *)


(*
 * Nom : leftBorder
 * Rôle : renvoie la liste de toutes les cases sur la frontière gauche de l'hexagone 
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let rec getLeftBorder = fun h l -> match (l,List.rev l) with 
	(1::r,_) -> l
	|(a::r,b::l) -> getLeftBorder (h-1) ((a-h+1) :: a :: r @ [b + h])
	|_-> [];;

let leftBorder = fun s m -> getLeftBorder s [m-(s-1)/2];;


(*
 * Nom : rightBorder
 * Rôle : renvoie la liste de toutes les cases sur la frontière droite de l'hexagone 
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let rec getRightBorder = fun h l n -> match (l,List.rev l) with 
	(a::r,_) when a=n -> l
	|(a::r,b::l) -> getRightBorder (h-1) ((a-h) :: a :: r @ [b + h - 1]) n 
	|_-> [];;

let rightBorder = fun s m -> (getRightBorder s [m + ((s-1)/2)] ((s+1)/2) );;


(*
 * Nom : getBorders
 * Rôle : renvoie les frontières "ouest" et "est" de l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 *)
let getBorders = fun s -> let m = (getMiddle s) in (leftBorder s m, rightBorder s m);;


(*
 * Nom : onTheLine
 * Rôle : renvoie la coordonnée en y d'un pion
 * Paramètres : rightBorder -> frontière Est 
 				hex -> pion 
 				acc -> res, initialisé à 1
 *)
let rec onTheLine = fun rightBorder hex acc -> match rightBorder with 
	a::r when hex <= a -> acc  
	|_::r-> (onTheLine r hex (acc+1))
	|_-> -1;;


(*
 * Nom : getPosInLine
 * Rôle : renvoie la position d'un pion relative à sa ligne
 * Paramètres : s -> taille de l'hexagone 
 				m -> case centrale de l'hexagone
 *)
let getPosInLine = fun hex rightBorder line -> match line with 
	line when line <= 1 -> hex 
	|_-> hex - (List.nth rightBorder (line-2));;

(*
 * Nom : getTrueX
 * Rôle : renvoie la position en x d'un pion sur l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 				mid -> case centrale de l'hexagone
 				hex -> pion 
 				rightBorder -> frontière Est
 *)
let getTrueX = fun line s mid hex rightBorder -> let p = (getPosInLine hex rightBorder line) in 
													if line = mid then (-1 + 2*p) 
													else 
														(abs (line - mid)-1) + 2*p;;

(*
 * Nom : getCoord
 * Rôle : renvoie les coordonnées d'un pion sur l'hexagone
 * Paramètres : s -> taille de l'hexagone 
 				hex -> pion
 *)
let getCoord = fun hex s -> let rightBorder = (rightBorder s (getMiddle s)) in 
							let line = (onTheLine rightBorder hex 1) in 
								((getTrueX line s ((s+1)/2) hex rightBorder),line);;


(*
 * Nom : itGo
 * Rôle : renvoie Vrai si l'écart entre les coordonnées en x de 2 pion est plus grand que celui entre les coordonnées en y
 * Paramètres : a -> premier pion
 				b -> second pion
 *)
let itGo = fun a b -> match (a,b) with 
	((x,y),(i,j)) when (abs (x - i)) > (abs (y - j)) -> true 
	|_ -> false;;


(*
 * Nom : distance
 * Rôle : calcul la distance entre 2 pions a et b de l'hexagone
 * Paramètres : a -> pion a 
 				b -> pion b
 				s -> taille de l'hexagone 
 *)
let getDistance = fun a b -> let xory = (itGo a b) in  match (a,b,xory) with 
	((x,y),(i,j),true) -> ((abs (x - i)) - (abs (y-j)))/2 + (abs (y-j))
	|((x,y),(i,j),false) -> (abs (y - j));;

let distance = fun a b s -> getDistance (getCoord a s) (getCoord b s);;


(*
 * Nom : hexFromEnding
 * Rôle : renvoie la liste des coûts qu'il faudrait à un pion donné pour aller sur chaque case d'une postion finale
 * Paramètres : s -> taille de l'hexagone 
 				hex -> position du pion 
 				cost -> coût du pion
 				ending -> disposition de fin
 *)
let hexFromEnding = fun s hex cost ending -> List.map (fun a -> cost*(distance a hex s)) ending;;


(*
 * Nom : getAllCosts
 * Rôle : renvoie la liste des coûts de chaque pion pour aller sur chaque case d'une position finale
 * Paramètres : hexList -> liste des pions
 				ending -> position finale
 				s -> taille de l'hexagone
 *)
let getAllCosts = fun hexList ending s -> List.map (fun a -> hexFromEnding s a.pos a.cost ending) hexList;;



(* ------------------------------------------------------- Réduction des éléments de la matrice ------------------------------------------------------- *)

(*
 * Nom : minOfLine
 * Rôle : trouve le minimum d'une ligne
 * Paramètres : line -> int list
 *)
let minOfLine = fun line -> match line with 
	a :: r -> List.fold_left (min) a r 
	|_-> -1;;


(*
 * Nom : getListOfMin
 * Rôle : récupère la liste des minimums de chaque colonne
 * Paramètres : mat -> matrice (int list list)
 *)
let rec getListOfMin = fun mat -> match mat with 
	a::r -> (minOfLine a) :: (getListOfMin r)
	|_-> [];;


(*
 * Nom : applyMin
 * Rôle : enlève le minimum correspondant à chaque ligne
 * Paramètres : listInt -> ligne 
 				minList -> liste des minimums
 *)
let rec applyMin = fun listInt minList -> match (listInt,minList) with 
	(a::r,b::t) -> (a-b) :: (applyMin r t) 
	|_-> [];;


(*
 * Nom : rotateMat
 * Rôle : "pivote" une matrice -> les lignes deviennent des colonnes et vice-versa 
 * Paramètres : mat -> matrice, int list list
 *)
let getListOfFirstElems = fun mat -> List.map (fun a -> match a with a::r -> a |_-> -1) mat;;

let rec rotateMat = fun mat -> match mat with 
	[]::r -> []
	|_-> (getListOfFirstElems mat) :: (rotateMat (List.map (fun a -> match a with a::r -> r |_-> []) mat));;


(*
 * Nom : reduceLines
 * Rôle : enlève à chaque élément de chaque ligne le minmum de la ligne
 * Paramètres : costMat -> matrice (int list list)
 *)
let reduceLines = fun costMat -> let minList = (getListOfMin (rotateMat costMat)) in 
									List.map (fun a -> (applyMin a minList)) costMat;;


(*
 * Nom : reduceColumn
 * Rôle : enlève à chaque élément de chaque colonne le minmum de la colonne
 * Paramètres : costMat -> matrice (int list list)
 *)
let rec reduceColumn = fun costMat -> match costMat with 
	(a::r) -> (List.map (fun b -> b - (minOfLine a)) a) :: (reduceColumn r)
	|_-> [];;


(*
 * Nom : reduce
 * Rôle : reduit les valeurs d'une matrice
 * Paramètres : costMat -> matrice (int list list)
 *)
let reduce = fun costMat -> (reduceColumn (reduceLines costMat));;



(* ------------------------------------------------------- Sélection des 0 indépendants de la matrice + marquage des lignes / colonnes ------------------------------------------------------- *)

(*
 * Nom : notCovered
 * Rôle : détermine si un point est déjà selectionné
 * Paramètres : acc -> liste de points 
 				i,j -> points à vérifier
 *)
let rec notCovered = fun acc i j -> match acc with 
	                            |((a,b)::r) when (a = i || b = j) -> false
	                            |((a,b)::r) -> (notCovered r i j )
	                            |_-> true;;


(*
 * Nom : updateaAcc
 * Rôle : update un acc passé en paramètre, si le point (x,y) n'y est pas déjà
 * Paramètres : acc -> liste de points 
 				x,y -> coordonnées du points à ajouter
 *)
let updateAcc = fun acc a x y -> match (acc,a) with 
	                           ((sel,notCov),0) -> if (notCovered sel x y) then (sel @ [(x,y)],notCov) else (sel,notCov @ [(x,y)])
	                          |_-> acc;;


(*
 * Nom : selectZeros
 * Rôle : parcourt la matrice de manière "naïve" et renvoie la liste des 0 indépendants
 * Paramètres : costMat -> matrice
 				acc -> accumulateur, initialisé à []
 				x,y -> coordonnées, initialisées à 1
 *)
let rec selectZeros = fun costMat acc x y -> match (costMat) with
	                                       (a::t)::r -> (selectZeros (t::r) (updateAcc acc a x y) x (y+1))
	                                      |[]::r -> (selectZeros r acc (x+1) 1)
	                                      |_-> acc ;;


let fold_append (a,b) = List.fold_left (fun br ar -> ar::br) (b) a;;


(*
 * Nom : paire
 * Rôle : trouve le 0' sur la ligne d'un point
 * Paramètres : (x,y) -> point
 				prime -> liste des 0'
 *)
let paire ((x,y),prime) = List.find (fun (a,b) -> b = y) prime;;


(*
 * Nom : impaire
 * Rôle : trouve le 0 sélectionné sur la colonne d'un point
 * Paramètres : (x,y) -> point
 				sel -> liste des 0 sélectionnés
 *)
let impaire ((x,y),sel) = List.find (fun (a,b) -> a = x) sel;;
 

(*
 * Nom : stepTwoPrime
 * Rôle : trouve le maximum de 0 indépendants dans le cas où on n'a pas sélectionné le maximum de 0 indépendants lors de l'étape 1
 * Paramètres : z_liste -> liste de 0
 				sel -> 0 sélectionnés
 				prime -> 0'
 *)
let rec stepTwoPrime (z_liste,sel,prime) =  let zi_1 = List.hd (z_liste) in
											let z_liste = (paire(zi_1,prime)::z_liste) in
											let zi_1 = List.hd (z_liste) in
												if (List.exists (fun (x,y) -> x = fst (zi_1)) sel) then
	  												let z_liste = (impaire (zi_1,sel)::z_liste) in
	   													stepTwoPrime (z_liste,sel,prime)
												else
	    											fold_append(List.find_all (fun a -> not (List.exists (fun b -> b = a)z_liste)) sel,List.find_all (fun a -> not (List.exists (fun b -> b = a)sel))  z_liste);;

	    											
(*
 * Nom : docoverLigne
 * Rôle : couvre les lignes et colonnes de façon à ce que tous les 0 soient couverts et en utilisant le minimum de couvertures possible, utilise la méthode de Munkres
 * Paramètres : notcov -> 0 non couverts
 				notsel -> 0 non sélectionnés
 				sel -> 0 sélectionnés
 				covcol -> colonnes couvertes
 				covlin -> lignes couvertes
 				prime -> 0'
 *)
let rec docoverligne (notcov,notsel,sel,covcol,covlin,prime) = match (notcov) with
   [] -> (sel,covcol,covlin,true)
   |((a,b)::r) ->
     	if (List.exists (fun (x,y) -> b = y)sel) then
       		let zero_here = List.find (fun (x,y) -> b = y) sel in
       		let prime = ((a,b)::prime) in
       		let covcol = List.find_all (fun x -> x != (fst (zero_here))) covcol in
       		let covlin = (b::covlin) in
       		let notcov = List.find_all (fun (a,b) -> (List.for_all (fun x -> x != a) covcol)&&(List.for_all (fun y -> y != b) covlin )) notsel in
      		 docoverligne (notcov,notsel,sel,covcol,covlin,prime)
    	 else
        	(stepTwoPrime (impaire((a,b),sel)::[(a,b)],sel,prime),covcol,covlin,false);;


(*
 * Nom : stepOne & stepTwo
 * Rôle : sélectionne le maximum de 0 indépendants, les renvoies et renvoie les colonne / lignes couvertes
 * Paramètres : costMat -> matrice
 				sel -> 0 sélectionnés
 				covCol -> colonnes couvertes 
 				covLin -> lignes couvertes
 				sel -> 0 sélectionnés
 				notsel -> 0 non sélectionnés
 *)
let rec stepOne costMat sel covcol covlin = let (sel,notsel) = (selectZeros costMat (sel,[]) 1 1) in
  									if ((List.length sel) = (List.length costMat)) then
    									(sel,covcol,covlin)
  									else
    									stepTwo(costMat,sel,notsel)

and stepTwo (costMat,sel,notsel) =  let covcol = List.fold_left (fun acc (a,b) -> a::acc)[] sel in
  									let notcov = List.find_all (fun (a,b) -> List.for_all (fun x -> x != a) covcol) notsel in
  									let (sel,covcol,covlin,res_fin) = docoverligne(notcov,notsel,sel,covcol,[],[]) in
										if (res_fin) then
	    									(sel,covcol,covlin)
	  									else
	    									stepOne costMat sel covcol [];;

(*
 * Nom : call_step
 * Rôle : renvoie les zéros indépendants d'une matrice, ainsi que les lignes / colonnes couvertes
 * Paramètres : costMat -> matrice (int list list)
 *)
let call_step costMat = stepOne costMat [] [] [];;



(* ------------------------------------------------------- Ajustement des valeurs en fonction du minimum non couvert  ------------------------------------------------------- *)
(*
 * Nom : inList
 * Rôle : vérifie si un élément appartient à une liste
 * Paramètres : covList -> int list 
 				x -> int
 *)
let rec inList = fun covList x -> List.exists (fun a -> a = x) covList;;


(*
 * Nom : getMin
 * Rôle : parcoure la matrice et en détermine son minimum non couvert
 * Paramètres : costMat -> matrice (int list list)
 				covCol -> colonnes couvertes
 				covLin -> lignes couvertes
 				x,y -> coordonnées actuelles lors du parcours de la matrice 
 				min -> res final
 *)
let rec getMin = fun costMat covCol covLin x y min -> match costMat with 
	(a::t)::r when ((a<min) && (not((inList covCol x) || (inList covLin y)))) -> (getMin (t::r) covCol covLin x (y+1) a)
	|(a::t)::r-> (getMin (t::r) covCol covLin x (y+1) min)
	|[]::r -> (getMin r covCol covLin (x+1) 1 min)
	|_-> min;;


(*
 * Nom : getunHayden
 * Rôle : déterminer le minimum non couvert
 * Paramètres : costMat -> matrice (int list list)
 				covCol -> colonnes couvertes
 				covLin -> lignes couvertes
 *)
let getunHaydenMin = fun costMat covCol covLin -> match costMat with 
	(a::t)::r -> (getMin costMat covCol covLin 1 1 (max_int))
	|_-> -1;; 


(*
 * Nom : updateMat
 * Rôle : met à jour les valeurs les valeurs d'une matrice en fonction de la valeur minimum non "couverte" : on enlève min aux valeurs non convertes et on l'ajoute aux valeurs couvertes 2 fois
 * Paramètres : mat -> matrice à mettre jour (int list list)
 				miniVal -> minimum non couvert 
 				covCol -> liste des colonnes couvertes 
 				covLin -> liste des lignes couvertes 
 				x,y -> coordonnées de l'élément actuel lors du parcours de la matrice
 				acc1 -> matrice en cours de reconstruction, résultat final 
 				acc2 -> colonne en cours de reconstruction
 *)
let rec updateMat = fun mat miniVal covCol covLin x y acc1 acc2->  match mat with 
	(a::t)::r when ((inList covCol x) && (inList covLin y)) -> (updateMat (t::r) miniVal covCol covLin x (y+1) acc1 (acc2 @ [a+miniVal]))
	|(a::t)::r when ((inList covCol x) || (inList covLin y)) -> (updateMat (t::r) miniVal covCol covLin x (y+1) acc1 (acc2 @ [a]))
	|(a::t)::r -> (updateMat (t::r) miniVal covCol covLin x (y+1) acc1 (acc2 @ [(a-miniVal)]))
	|[]::r -> (updateMat r miniVal covCol covLin (x+1) 1 (acc1 @ [acc2]) [])
	|_-> acc1;;



(* ------------------------------------------------------- Calcul du score minimum possible  ------------------------------------------------------- *)


(*
 * Nom : hungarianMethod
 * Rôle : Applique la méthode hongroise à une matrice, et retourne une liste contenant les coordonnées des points idéaux afin de minimiser le coût (soit le couplage parfait de poids minimum)
 * Paramètres : costMat -> matrice (int list list)
 				s -> taille de costMat (et de l'hexagone)
 *)
let rec hungarianMethod = fun costMat s -> let (cov,covCol,covLin) = (call_step costMat) in 
										if ((List.length cov) = s) then cov 
										else 
											let miniVal = (getunHaydenMin costMat covCol covLin) in 
												hungarianMethod (updateMat costMat miniVal covCol covLin 1 1 [] []) s;;


(*
 * Nom : getSum
 * Rôle : calcule la somme de tous les pions situés aux coordonnées optimales
 * Paramètres : costMat -> matrice (int list list)
 				bestCoords -> liste des coordonnées optimales
 				acc -> accumulateur, résultat final
 				x,y -> coordonnées 
 *)
let rec getSum = fun costMat bestCoords acc x y -> match costMat with 
	(a::t)::r when (List.exists (fun (i,j) -> ((x = i) && (y = j))) bestCoords) -> (getSum (t::r) bestCoords (acc+a) x (y+1))
	|(a::t)::r -> (getSum (t::r) bestCoords acc x (y+1))
	|([]::r) -> (getSum r bestCoords acc (x+1) 1)
	|_-> acc;;


(*
 * Nom : hungarianRhapsody
 * Rôle : calcul le couplage parfait des poids minimum d'une matrice et en renvoie la somme
 * Paramètres : costMat -> matrice de coûts (int list list)
 *)
let hungarianRhapsody = fun costMat -> 	let s = (List.length costMat) in 
										let reducedMat = (reduce costMat) in (* on réduit la matrice *)
										let bestCoords = (hungarianMethod reducedMat s) in (* on applique la méthode hongroise *)
											(getSum costMat bestCoords 0 1 1);; (* on calcule le score minimum *)


(*
 * Nom : optimusPrice
 * Rôle : renvoie le minimum de 3 valeurs
 * Paramètres : a,b,c trois entiers
 *)
let optimusPrice = fun a b c -> min (min a b) (min b c) ;;  


(*
 * Nom : fusion
 * Rôle : fusionne une liste de position avec une liste de coût, et renvoie donc une liste de hex
 * Paramètres : pos -> liste de position
 				cost -> liste de coût
 *)
let rec fusion = fun pos cost -> match (pos,cost) with 
	(a::r,b::t) -> {pos = a; cost = b} :: (fusion r t)
	|_-> [];;  


(*
 * Nom : resolve
 * Rôle : calcul le score minimum pour finir la partie, et le renvoie sous forme de string
 * Paramètres : cost -> liste de coûts
 				pos -> liste de positions 
 				s -> taille de l'hexagone
 *)
let resolve = fun cost pos s -> let hexList = (fusion pos cost) in  
								let (ending1,ending2,ending3) = (getAllEndings s) in 
								let (rep1,rep2,rep3) = (hungarianRhapsody (getAllCosts hexList ending1 s), hungarianRhapsody (getAllCosts hexList ending2 s), hungarianRhapsody (getAllCosts hexList ending3 s)) in 
								string_of_int ((optimusPrice rep1 rep2 rep3));;



(* ------------------------------------------------------- Fonctions de lecture  ------------------------------------------------------- *)

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
 * Nom : algolec
 * Rôle : algorithme de lecture, renvoie la liste des postitions et la liste des coûts
 * Paramètres : flec -> fichier d'entrée
 *)
let algolec = fun flec -> (List.map (int_of_string) (call (input_line flec)),List.map (int_of_string) (call (input_line flec)));;


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
|n -> let (pos,cost) = (algolec flec) in 
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb pos cost (List.length pos))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;		

(*jam "D-small-practice.in" "output.out" algolec resolve;; *)

(*jam "D-large-practice.in" "output.out" algolec resolve;; *)
 