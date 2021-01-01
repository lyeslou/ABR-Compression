(* ---------- Module fonctions sur les arbres binaires de recherches ------------ *)

open Structures;; (* Importation du module Structures *)



(*
 * @param  liste  : Une liste d'entiers représentant une séquence d'insertion 
 * @return        : Un arbre binaire de recherche de type 'arb' 
 * Cette fonction prend en paramètre une séquence d'entiers 'liste' et renvoie un arbre binaire de recherche   
 * contenant les éléments de 'liste'
 *)
let newAbr liste = 
  let rec addElemToAbr abr elem = (* Finction qui rajoute un élément à un arbre de recherche binaire *)
    match abr with
      | Empty           -> Node(elem, Empty, Empty)
      | Node(key, g, d) -> if elem < key then Node(key, (addElemToAbr g elem), d) else Node(key, g, (addElemToAbr d elem))
  in
  let rec aux l abr =
    match l with 
    | x::xx -> aux xx (addElemToAbr abr x)
    | []    -> abr
  in aux liste Empty
;;


(*
 * @param  arb  : Un arbre binaire de recherche de type 'arb' 
 * @param    v  : Un entier 
 * @return      : Un boolean
 * Cette fonction prend en paramètre un ABR 'arb' et un entier 'v' et renvoie un boolean de l'existance de 'v'   
 * dans l'arbre 'arb'
 *)
let rec rechercheABR arb v = match arb with
| Empty -> false
| Node(value, g, d) -> if(value = v) then true else if v < value then rechercheABR g v else rechercheABR d v
;;


(*
 * @param  abr  : Un arbre binaire de recherche de type 'arb' 
 * @return      : Une chaine de caractères representant la topologie de l'arbre 'abr'
 * Cette fonction prend en paramètre un ABR 'abr' et renvoie un représentation de sa topologie 
 * sous forme de string avec l'alphabet '(' ')' et la règle suivante  φ(A) = ( φ(G) ) φ(D)
 * φ(X) : est la string représentant la topologie de l'arbre X
 * A    : est l'arbre binaire
 * G    : est le fils gauche de A
 * D    : est le fils droit de A
 *)          
let chaine_caractere_associe_abr abr=
  let rec loop abr =
    match abr with
    |Empty->[]
    |Node(cle,g,d)->["("] @ loop g @ [")"] @ loop d
  in
  String.concat "" (loop abr)
;;                             

(*
 * @param  abr1  : Un arbre binaire de recherche de type 'arb' 
 * @param  abr2  : Un arbre binaire de recherche de type 'arb' 
 * @return       : Un boolean
 * Cette fonction prend en paramètre deux arbres binaires de type 'arb, 'abr1' et 'arb2' 
 * et renvoie true si leurs formes topologiques sont équivalentes et false sinon
 *)
let isEqv abr1 abr2=
  chaine_caractere_associe_abr abr1 = chaine_caractere_associe_abr abr2
;;