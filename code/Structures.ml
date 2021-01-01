(* ------------------------------ Module Structure Arbre ------------------------------- *)


(* 
 * Un nœud dans un arbre binaire de recherche est soit :
 *        - Une feuille: Empty
 *        - Avec un fils gauche et un fils droite; qu'ils seront eux aussi des nœuds: Node of int*arb*abr  
 *)

type arb = Empty | Node of int * arb * arb ;;


(* 
 * Un nœud dans un arbre binaire compresse est soit :
 *        - Une feuille : EmptyCL
 *        - Compose de :
 *                  * (valuesNode)    : Un ensemble de valeurs (Une map ou une Liste )
 *                  * labelArcsRouge  : Un couple pour les labels des arcs fils  
 *                  * Une reference vers un fils gauche et un fils droit    
 * 
 *)
type ('a,'b) pointeurAbr = Null | Pointeur of ('a, 'b) abrCompresse ref
and ('a,'b) abrCompresse = EmptyCL | NodeC of ('a, 'b) contentNode
and ('a,'b) contentNode  = { 
  mutable valuesNode: 'a;
  labelArcsRouge: ('b * 'b); 
  abrGauche: ('a,'b) pointeurAbr ; 
  abrDroite: ('a,'b) pointeurAbr 
}
;;



