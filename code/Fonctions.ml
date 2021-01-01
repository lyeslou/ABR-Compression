(* ------------------------------ Module Fonctions Diverses ------------------------------- *)

(*
 * La fonction "extraction_alea" prend en entré deux listes d’entiers l et p. La fonction choisit 
 * aléatoirement un entier r entre 1 et la taille de l, puis retourne un couple de listes dont la première 
 * est la liste l dans laquelle on a retiré le r-ième élément et la deuxième est la liste P dans laquelle 
 * on a ajouté en tête le r-ième élément extrait de L. 
 * List.length l : revoie la taille de l
 * List.nth l n  : revoie le n element de l
 * Random.init() : Initialisation du module Random de OCaml
 * Random.int n  : renvoie un nombre aléatoire en 0 et n
 *)
let extraction_alea (l: int list) (p: int list)=
  Random.self_init(); 
  let taille= List.length(l) in
  let r=Random.int (taille)in
  let rec loop ind l  =
    match l with
    |[]->[]
    |e::ee ->if ind=0 then ee else e::(loop (ind-1) ee)
  in
  (loop r l ,(List.nth l r)::p)
  ;;


(*
 * n : La taille de la liste à générer
 * @return : Une liste aléatoire de taille n contenant les entiers de 1 à n 
 * Cette fonction est une implementation de mélange Fisher-Yates 
 * Cette fonction génère une liste L des entiers de 1 à n et une liste vide P puis qui vide
 * entièrement la liste L et remplit la liste P en appelant extraction_alea.
 *)
let gen_permutation n =
  let rec initialisation i =
    if i=n then n::[] else i::(initialisation (i+1))
  in
  let rec loop l p=
    if l=[] then p
    else
      match extraction_alea l p with |(l,p)->loop l p
  in
  loop (initialisation 1) []
;;
