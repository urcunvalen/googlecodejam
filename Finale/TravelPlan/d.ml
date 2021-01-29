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
 * nom : travel
 * pre-conditions : c_p : int list, Liste des distances entre chaque planète voisine (pas de planète entre) ; fuel : string, capacité de fuel
 * post-conditions : renvoi sous forme de string la capacité maximale de fuel que l'on peut utiliser en voyageant sur toutes les planètes 1 seules fois et en revenant sur la Terre. 
 *                          S'il n'existe pas de possibilité renvoi "NO SOLUTION"
 *
 * nom : call_i
 * pre-conditions : l : int list, List des distances ; f : int, capacité de fuel ; j : int, itérateur de postision ; acc : int, accumulateur pour chaque trjajet possible ; res : indice de dernière position
 * post-conditions : On calcul de la manière suivante : chaque intervalle doit être parcourru au moins 2 fois, donc tj >=2. Et pour j=1 et j=n-1 = 0. De plus tj - t(j-1) = 0||-2||2						
 * Donc on construit un chemin de la manière suivante : t1 = 2 -> [t2= 2 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....]] ou [t2= 4 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....] ou [t3 = 6 -> ....]] 
 *	On fait ensuite la somme des tj*dj, et on prend le chemin qui donne la valeur maximum sans dépasser f						
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let rec call_i (l,f,j,acc,res) = if acc > f then 0 else match (l,j,res) with
    ([],_,_) -> acc
   |(a::r,1,_) -> call_i (r,f,j+1,acc + (a * 2),2)
   |(a::[],_,2)|(a::[],_,4) -> let acc = (acc + (a * 2)) in if (acc > f ) then 0 else acc
   |(a::[],_,_) -> 0
   |(a::r,_,2) -> max (call_i (r,f,j+1,acc + (a * 2),2)) (call_i (r,f,j+1,acc + (a * 4),4))
   |(a::r,_,_)-> max (max (call_i (r,f,j+1,acc + (a * (res + 2)),res+2)) (call_i (r,f,j+1,acc + (a * res),res))) (call_i (r,f,j+1,acc + (a * (res - 2)),res-2))
;;
let travel (c_p,fuel) = let a = call_i (c_p,int_of_string fuel,1,0,0) in if a = 0 then "NO SOLUTION" else string_of_int a 
;;




(*
 * nom : to_int
 * pre-conditions : l : string list, liste de string d'entier
 * post-conditions : renvoi une liste d'entier
 *
 * nom : create_I
 * pre-conditions : l : int list, la liste des  planètes rangées par coordonnées ; res : int list, la liste des distances entre chaque planète voisine
 * post-conditions : 
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let to_int l = List.map (fun x -> int_of_string x) l
;;
let rec create_I l res = match (l) with
    ([]) -> []  
   |(pa::[]) -> res
   |(pa::pb::r) -> create_I (pb::r) (res@[abs (pb - pa)])
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
let algolec flec = (input_line flec, input_line flec, input_line flec)
;;
let rec jam_rec flec algolec algopb nblignes total = match nblignes with 
    0 -> ""
   |n -> let (fuel,c_p,nb_p) = (algolec flec) in 
	 "Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (create_I(List.sort compare (to_int(call(c_p)))) [],fuel))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

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
(* jam "D-small-practice.in" "D-small.out" algolec travel;; *)