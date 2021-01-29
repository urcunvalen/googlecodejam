(* travel plan *)

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
 * nom : call_before
 * pre-conditions : l : int list, list des distances ; f : int, capacité du fuel ; n : int, valeur max de l'indice de départ t du chemin 2 ; i : int, itérateur des indices de départ du chemin 2 ; acc : int list, list des valeurs de fuel possible dans le chemin 2
 * post-conditions : on renvoie la list de tout les possibilités du chemin 2, pour i/2 dans [1,(n+2)/2] 
 *
 * nom : call_i
 * pre-conditions : l : int list, List des distances ; f : int, capacité de fuel ; j : int, itérateur de postision ; acc : int list, accumulateur pour chaque trjajet possible ; res : indice de dernière position
 * post-conditions : On calcul de la manière suivante : chaque intervalle doit être parcourru au moins 2 fois, donc tj >=2. Et pour j=1 et j=n-1 , t= 2. De plus tj - t(j-1) = 0||-2||2						
 * Donc on construit un chemin de la manière suivante : t1 = 2 -> [t2= 2 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....]] ou [t2= 4 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....] ou [t3 = 6 -> ....]] 
 *	on renvoie la liste des chemins possible dans la liste 1				
 *
 * nom : call_i2
 * pre-conditions : l : int list, List des distances ; f : int, capacité de fuel ; j : int, itérateur de postision ; acc : int list, accumulateur pour chaque trjajet possible ; res : indice de dernière position
 * post-conditions : On calcul de la manière suivante : chaque intervalle doit être parcourru au moins 2 fois, donc tj >=2.  De plus tj - t(j-1) = 0||-2||2						
 * Donc on construit un chemin de la manière suivante : t1 = 2 -> [t2= 2 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....]] ou [t2= 4 -> [t3 = 2 -> ....] ou [t3 = 4 -> ....] ou [t3 = 6 -> ....]] 
 *	on renvoie la liste des chemins possible dans la liste 2				
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let rec call_i (l,f,j,acc,res,i) =
  if acc > f then
    [(0,i)]
  else
    match (l,j,res) with
      ([],_,2) -> [(acc,i)]
     |([],_,_) -> [(0,1)]
     |(a::r,1,_) -> call_i (r,f,j+1,acc + (a * i),i,i)
     |(a::[],_,2)|(a::[],_,4) -> let acc = (acc + (a * 2)) in if (acc > f ) then [(0,i)] else [(acc,i)]
     |(a::[],_,_) -> [(0,i)]
     |(a::r,_,2) -> let atemp = (call_i (r,f,j+1,acc + (a * 2),2,i)) in let atemp2 = (call_i (r,f,j+1,acc + (a * 4),4,i)) in atemp@atemp2
     |(a::r,_,_)-> let atemp = (call_i (r,f,j+1,acc + (a * (res + 2)),res+2,i)) in let atemp2 = (call_i (r,f,j+1,acc + (a * res),res,i)) in let atemp3 = (call_i (r,f,j+1,acc + (a * (res - 2)),res-2,i)) in atemp@atemp2@atemp3
;;
let rec call_i2 (l,f,j,acc,res) =
  if acc > f then
    [(0,res)]
  else
    match (l,j,res) with
      ([],_,_) -> [(acc,res)]
     |(a::r,1,_) -> call_i2 (r,f,j+1,acc + (a * 2),2)
     |(a::r,_,2) -> (call_i2 (r,f,j+1,acc + (a * 2),2))@(call_i2 (r,f,j+1,acc + (a * 4),4))
     |(a::r,_,_)-> (call_i2 (r,f,j+1,acc + (a * (res + 2)),res+2))@(call_i2 (r,f,j+1,acc + (a * res),res))@(call_i2 (r,f,j+1,acc + (a * (res - 2)),res-2))
;;
let rec call_before (l,f,n,i,acc) = if ( i = n+2 ) then acc  else
let acc = acc@call_i(l,f,1,0,0,i) in call_before(l,f,n,i+2,acc)
;;
    
	
	
(*
 * nom : cruz
 * pre-conditions : la : int list, la liste correspondant aux valeur de fuel consommées sur le mi chemin 1 ; lb : int list, la liste correspondant aux valeur de fuel consommées sur le mi chemin 2 ; f : int, la capacité du fuel
 * post-conditions : renvoi la valeur max du matching la&lb < f
 *
 * nom : check
 * pre-conditions : (a,b) : (int * int), les indices de t en j et j-1
 * post-conditions : vérifie que l'on peut passer de t(j-1) à tj
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let check (a,b) = (a=b)||(b - a = 2)||(b - a = -2)
;;
                                                                                                        
let cruz (la,lb,f) = if (la != [])&&(lb != []) then
                       let restemp = (List.fold_left
                                        (fun acc1 (lav,nothing) -> let res = (List.fold_left (fun acc2 (lab,nothing2) ->
                                                             if (((lav + lab) <= f)&&(lav + lab > acc2)&&(check(nothing,nothing2))) then
                                                               lav + lab
                                                             else
                                                               acc2)0 lb ) in
                                                                     if res > acc1 then
                                                                      res
                                                                    else
                                                                      acc1
                                                                   )0 la) in if restemp = 0 then "NO SOLUTION" else string_of_int restemp 
                     else  "NO SOLUTION"
;;



(*
 * nom : travel
 * pre-conditions : c_p : int list, Liste des distances entre chaque planète voisine (pas de planète entre) ; fuel : string, capacité de fuel
 * post-conditions : renvoi sous forme de string la capacité maximale de fuel que l'on peut utiliser en voyageant sur toutes les planètes 1 seules fois et en revenant sur la Terre. 
 *                          S'il n'existe pas de possibilité renvoi "NO SOLUTION"
 *
 * nom : cut_list
 * pre-conditions : l : int list, liste des distances ; n : int, indice où l'on doit couper la liste
 * post-conditions : Coupe l en 2 listes à l'indice n
 *
 * nom : testdist 
 * pre-conditions : c : int list, la liste des planètes clasées par coordonnées ; fuel : la capacité de fuel maximum
 * post-conditions : vérifie que la valeur du fuel permet d'effectuer le trajet le plus court
 *
 * nom : loop
 * pre-conditions : l :  int list, list de valeur pour un demi chemin 
 * post-conditions : l privé des éléments égal à 0
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let cut_list l n = fst (List.fold_left (fun ((a,b),i) x -> if (i> n-1) then ((a,b@[x]),i) else ((a@[x],b),i+1)  ) (([],[]),0) l)
;;
let testdist (c,fuel) = ((((abs(List.hd (List.rev c))) + (abs (List.hd c)))*2) > fuel                   ) 
;;
let loop (l) = if List.exists(fun (a,b) -> (a!=0)) l then List.find_all (fun (a,b) -> (a != 0)) l else []
;;
let travel (c_p,c,n_p,fuel) = if n_p = 2 then (string_of_int ((List.hd c_p) * 2)) else
                                if (testdist (c,fuel)) then
                                  ("NO SOLUTION")
                               else
                                  let n1 = n_p / 2 in
                                  let (li1,li2) = cut_list c_p n1 in
                                  let a = List.sort compare (call_i2 (li1,fuel,1,0,0)) in
                                  let b = List.sort compare (call_before (li2,fuel,2*(n1+1),2,[])) in
                                  (* let ra = a in let rb = b in *)
                                  let a = List.rev (loop(a)) in
                                  let b = List.rev (loop(b)) in
                                  (cruz(a,b,fuel))
      
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
let rec create_I l i = match (l) with
    ([]) -> []  
   |(pa::[]) -> i
   |(pa::pb::r) -> create_I (pb::r) (i@[abs (pb - pa)])
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
   |n -> let (fuel,c_p,nb_p) = (algolec flec) in let c_p = List.sort compare (to_int(call(c_p))) in
	 "Case #"^(string_of_int(total-nblignes+1))^": "^(algopb (create_I c_p [],c_p,int_of_string nb_p,int_of_string fuel))^"\n"^(jam_rec flec algolec algopb (nblignes-1) total) ;;

let jam fin fout algolec algopb =
  let fecr = open_out fout in
  let flec = open_in fin in
  let nb = (int_of_string(input_line flec)) in
  begin
    output_string fecr (jam_rec flec algolec algopb nb nb);
    close_in flec;
    close_out fecr;
  end;;
  (*  jam "D-small-practice.in" "D-small.out" algolec travel*)