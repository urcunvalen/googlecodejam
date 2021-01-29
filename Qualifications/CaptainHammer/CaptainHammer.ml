(*
 * nom : g
 * post-conditions : 9.8 (constante de gravité)			
 *
 * nom : pi
 * post-conditions : 3.14159265 (valeur de pi)
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let g = 9.8
;;
let pi = 3.14159265
;;


(*
 * nom : first
 * pre-conditions : s : string, string de la vitesse & de la distance
 * post-conditions : string de la vitesse
 *
 * nom : second
 * pre-conditions :s : string, string de la vitesse & de la distance
 * post-conditions : string de la distance
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let first s i = String.sub s 0 i
;;
let second s i = String.sub s (i+1) ((String.length(s))-(i+1))
;;



(*
 * nom : callcaptain
 * pre-conditions : s : string, sous la forme "vitesse distance"
 * post-conditions : renvoi le l'angle nécessaire pour arriver en d avec une vitesse v
 *
 * nom : captain
 * pre-conditions : sv/sd : string, vitesse et distance
 * post-conditions : renvoi le résultat d'obvious sous forme de string arrondi à 10^(-6)
 *
 * nom : obvious
 * pre-conditions : v : float, vitesse ; d : float, distance
 * post-conditions : applique le calcul de l'angle avec la distance et la vitesse
 *
 * nom : myround
 * pre-conditions : x : float
 * post-conditions : x arrondi à 10^(-6)
 *
 *
 * auteur : Dorian Gouzou
 * date création : 06/11/15
 *)
let myround x =  (float_of_int(int_of_float(x *. 1000000.))) /. 1000000.
;;                                                      
let obvious v d = ((0.5) *. ((180. /. pi) *. (asin((myround ((d) *. g)) /. (myround((v) ** 2.)  )))))
;;
let captain sv sd = string_of_float(myround(obvious (float_of_string(sv)) (float_of_string(sd))))
;;
let callcaptain s = let r = (String.index s ' ') in (captain (first s r) (second s r))
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
	"Case #"^(string_of_int(total-nblignes+1))^": "^(algopb line)^"\n"^(jam_rec flec algolec algopb (nblignes-1) total)
;;
let jam fin fout algolec algopb =
	let fecr = open_out fout in
	let flec = open_in fin in
	let nb = (int_of_string(input_line flec)) in
	begin
		output_string fecr (jam_rec flec algolec algopb nb nb);
		close_in flec;
		close_out fecr;
	end;;
  
  (* jam "B-small-practice.in" "B-small.out" input_line callcaptain *)