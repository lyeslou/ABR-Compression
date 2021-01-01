type arb = Empty | Node of int * arb * arb ;;


type ('a,'b) pointeurAbr = Null | Pointeur of ('a, 'b) abrCompresse ref
and ('a,'b) abrCompresse = EmptyCL | NodeC of ('a, 'b) contentNode
and ('a,'b) contentNode  = { 
  mutable valuesNode: 'a;
  labelArcsRouge: ('b * 'b); 
  abrGauche: ('a,'b) pointeurAbr ; 
  abrDroite: ('a,'b) pointeurAbr 
}
;;



