(* ------------ Module fonctions de compression de l'arbre de recherche binaire ------------ *)

open Structures;; (* Importation du module Structures *)
open ABR_utils;;  (* Importation du module ABR_utils *)

(*------------------- Fonctions Utilitaires --------------------*)
(* Fonctions permetant la manipulation notre structure de données  *)

(*
 * getContentNodeFromAbrCompresseList "abr": renvoie le contenu du nœud à la racinde de "abr" sous forme 
 * de l'enregistrement "ContentNode". 
 *)
let getContentNodeFromAbrCompresseList abr  = 
  match abr with
  | EmptyCL -> failwith "abrCompresse Vide"
  | NodeC(content)-> content
;;

(*
 * getAbrCompresseListFromPointeurAbr "pointeura": renvoie le nœud réferencer par "pointeura" si c'est 
 * pas une reference vers une feuille, sinon une exception avec un message d'erreur. 
 *)
let getAbrCompresseListFromPointeurAbr (pointeura )  =
  match pointeura with
  | Null -> failwith "pointeur null"
  | Pointeur(abrComp) -> !abrComp
;;

(*
 * getContentNodeFromPointeurAbr "pointeura": cette fonction fait appel aux deux fonctions précédentes
 *)
let getContentNodeFromPointeurAbr (pointeura)  = 
   getContentNodeFromAbrCompresseList (getAbrCompresseListFromPointeurAbr pointeura)
;;

(*
 * getNewPointeurAbrFromAbrCompresseList "abr": Créer une réference vers la racine "abr"
 *)
let getNewPointeurAbrFromAbrCompresseList abrC =
  Pointeur( ref abrC )
;;


(*
 * @param  c      : Une chaine de caractère représentant la topologie d'un arb 
 * @param  liste  : Une liste de couple (string, PointeurAbr) 
 * @return        : Un couple (boolean, PointeurAbr)
 * Cette fonction prend en paramètre un string qui représente la topologie d'un 'arb' 
 * et une liste de couple (string, PointeurAbr) de string de topologies rencontrées et de pointeur vers les equivalents
 * en abrCompresse et renvoie un couple (boolean, PointeurAbr) true si la topologie a déjà été rencontrée et un pointeurAbr
 * vers l'abrCompresse correspondant à la topologie
 *)
let rec haveSeen c liste = 
  match liste with 
   | [] -> (false, Null)
   | (e, nodeRef)::ee -> if (e = c) then (true, nodeRef ) else  (haveSeen c ee)
;;

(*
 * @param  c      : Une chaine de caractère représentant la topologie d'un arb 
 * @param  node   : Un pointeurAbr vers l'arbre compressé  
 * @param  liste  : Une liste de couple (string, PointeurAbr) 
 * Cette fonction ajoute le couple (c,node) à la liste des couples déjà visités
 *)
let insertToHaveSeen c node liste  = 
  liste := (c,node) :: !liste
;;


(*------------------------------------------------------------------------------------------------------*)
(*----------- Compression avec la structure de données: Liste pour sauvegarder les noeuds ------------  *)
(*------------------------------------------------------------------------------------------------------*)

(* ----------- Variables Globales --------------- *)
let labelCpt = ref 0 ;; (* Un compteur à incrémenter à chaque fois qu'on veut rajouté un label *)
let noLabel = 0;; (* Ce label par défaut veux dire que y a pas d'arc rouge, c-a-d l'arc est déjà dans arb initial *)

(*
 * @param  value  : Entier, la valeur contenu dans le npeud à créer 
 * @param  lg     : Entier, la valeur de l'arc gauche (noLabel pour un arc normal)  
 * @param  ld     : Entier, la valeur de l'arc droit (noLabel pour un arc normal)
 * @param  g      : Un pointeurAbr vers un arbre compressé (le fils gauche)  
 * @param  d      : Un pointeurAbr vers un arbre compressé (le fils droit)
 * @return        : Un noeud de type ( (int, int list) list , int)contentNode
 * Cette fonction crée un nouveau noeud de type  ( (int, int list) list , int)contentNode
 *)
let getNewValueNodeList (value:int) lg ld g d = 
  let values = [ (value,[]) ]  in
  let content = {
    valuesNode = values;
    labelArcsRouge = ( lg , ld);  
    abrGauche = g;
    abrDroite = d  
  } in 
  NodeC(content)
;;

(*
 * @return        : Un entier correspondant au nouveau label créé
 * Cette fonction crée un nouveau label
 *)
let newLabel () = 
  labelCpt := !labelCpt + 1 ; 
  !labelCpt
;;

(*
 * @param  abr      : Un noeud de type ( (int, int list) list , int)contentNode 
 * @param  v        : Un entier, la valeur à ajouter au noeud 'abr'  
 * @param  tmpL     : Une liste d'entiers pour l'identifiant label de 'v'
 * @return          : () unit
 * Cette fonction ajoute une valeur dans le contenu du noeud abr
 *)
let insertValueInRefNode abr v tmpL =
    abr.valuesNode <- List.append abr.valuesNode  ((v,tmpL)::[]) ;() 
;;


(*
 * @param  arbre    : Un pointeurAbr 
 * @param  toAdd    : Un arbre binaire de type 'arb' ayant une topologie equivalent que 'arbre'
 * @param  lab      : Une liste de labels 
 * @return          : Un pointeurAbr dont les valeurs de 'toAdd' ont été ajouté à 'arbre'
 * Cette fonction ajoute toutes les valeurs contenues dans 'toAdd' à 'arbre' avec des labels correspondant
 *)
let insertCompresse arbre toAdd lab  =
  let rec aux abr a listLabel labelArc =
    let p = getAbrCompresseListFromPointeurAbr abr in 
    match a with
    | Empty ->  abr 
    | Node(value, g, d) -> 
      begin
        let cn = getContentNodeFromAbrCompresseList p in
          let newListLabel = if labelArc = noLabel then listLabel else List.append listLabel [labelArc] in
  
            insertValueInRefNode cn value newListLabel;
            aux cn.abrGauche g newListLabel (fst cn.labelArcsRouge);
            aux cn.abrDroite d newListLabel (snd cn.labelArcsRouge); 
            getNewPointeurAbrFromAbrCompresseList p
      end
  in 
      aux arbre toAdd [] lab
;;


(*
 * @param  arbre    : Un arbre binaire de type 'arb'
 * @return          : Un arbre compressé liste de type  ( (int, int list) list , int) abrCompresse
 * C'est la fonction pricipale de création abrCompresse Liste à partir d'un arbre binaire 'arb' 
 * Cette fonction renvoie un abrCompresse list équivalent à l'arbre binaire 'arbre' passé en paramètre
 *)
(*------------- Fonction de compression de l'arbre binaire --------------*)
let getArbreCompresseListeFromAbr arbre  = 
  let seen = ref [] in 
    let rec aux (abr: arb)  = 
      match abr with 
      | Empty -> ( noLabel , getNewPointeurAbrFromAbrCompresseList EmptyCL )
      | Node (value , g, d ) -> 
        begin 
          let chaine = (chaine_caractere_associe_abr abr) in 
          let (hs, pointeurABR) = (haveSeen chaine !seen) in 
          begin 
            if hs then 
              let tempL = (newLabel ()) in 
                (tempL , (insertCompresse pointeurABR abr tempL) ) 
            else 
              begin
                let (labelG, abrCompresseG) = (aux g )  
                and (labelD, abrCompresseD) = (aux d ) in
                  let node = getNewValueNodeList value labelG labelD abrCompresseG abrCompresseD in
                    let pointeurNode = getNewPointeurAbrFromAbrCompresseList node in 
                    (insertToHaveSeen chaine pointeurNode seen) ; (noLabel, pointeurNode )
              end
          end
        end
      in
        let (_, res) = aux arbre in
          getAbrCompresseListFromPointeurAbr res
;;


(*---------------------------- Fonctions pour la recherche ---------------------------------*)
(*
 * La fonction getFirstElement renvoie le premier élément de la liste "liste"   
 *)

let getFirstElement liste = 
  match liste with
  | [] -> (-1, [])
  | x::xx -> x
;;

(*
 * La fonction compareVs compare si les deux liste V1 et V2 sont égaux ou pas       
 *)
let rec compareVs v1 v2 = 0
  match v1, v2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | x::xs, y::ys -> x = y && compareVs xs ys
;;

(*
 * La fonction elementIsInList vérifie si "listelable" est dans la liste des noeuds et c'est si vrai elle
 * verifie aussi si l'element "e" si bien le contenu de la case        
 *)
let elementIsInList listeNode (e: int) listeLabel :(bool * int) =
  let rec aux (listeNode) i :(bool * int) = 
    match listeNode with 
    | [] -> failwith "Error"
    | (v, l)::xx ->   
    if (compareVs l listeLabel) 
      then if v = e then (true, i) else (false, i)
    else aux xx (i+1)
  in aux listeNode 0
;;
(*------------ La fonction de recherche dans l'arbre compressé avec les listes---------------*)
let rechercheArbreCompresse arbreCompresse  (elem : int) : bool = 
  let rec aux abr labels : bool = 
    match abr with
    | EmptyCL -> false
    | NodeC(content) -> match labels with 
      |[] -> 
        begin 
          let valNoeud = fst (getFirstElement content.valuesNode ) 
          and lg = fst content.labelArcsRouge
          and ld = snd content.labelArcsRouge
          in 
          if valNoeud = elem then
            true
          else 
            if valNoeud < elem then
              aux (getAbrCompresseListFromPointeurAbr content.abrDroite) (if ld = noLabel then [] else [ld] )
            else
              aux (getAbrCompresseListFromPointeurAbr content.abrGauche) (if lg = noLabel then [] else [lg] )
        end
      | _ ->
        begin
          let (found, indiceElement) = (elementIsInList content.valuesNode elem labels) in
          if found then true 
          else 
            begin
              let valN = fst (List.nth content.valuesNode indiceElement) 
              and lg = fst content.labelArcsRouge
              and ld = snd content.labelArcsRouge
              in
              if valN < elem then
                aux (getAbrCompresseListFromPointeurAbr content.abrDroite) (labels @ if ld = noLabel then [] else [ld])
              else
                aux (getAbrCompresseListFromPointeurAbr content.abrGauche) (labels @ if lg = noLabel then [] else [lg])
            end
      end
    in
      aux arbreCompresse []
;;


(*------------------------ Affichage de l'arbre compressé avec les listes --------------------------*)
let afficheIndontation v = 
  for i=0 to v do 
    print_string "      "
  done;
;;

let rec afficheNode n i = 
  print_string"\n"; afficheIndontation i; print_string "|-----------------------------|\n";
  let rec aux listeNode = 
    match listeNode with
    | [] -> afficheIndontation i;print_string "|\n"
    | a::aa -> 
    begin 
      afficheIndontation i;print_string "|   v -> "; print_int (fst a) ;print_string " | labels -> ";
      let rec affListeLabel labels = 
        match labels with 
        | [] -> print_string "/";print_newline ()
        | e::ee -> print_int e; print_string ", "; affListeLabel ee
      in affListeLabel (snd a); ; aux aa
    end ;
  in aux n.valuesNode;

  afficheIndontation i;print_string "|       arcR = ("; print_int (fst n.labelArcsRouge);
  print_string ", "; print_int (snd n.labelArcsRouge); print_string ")\n";
  afficheIndontation i;print_string "|-----------------------------|\n";
;;

(* --------------------- *)
let printABRListe abr = 
  print_string " \n ************************** Affichage ************************** \n";
  let rec aux abrCompresse ind = 
    match abrCompresse with 
    | EmptyCL -> print_string"Empty"; print_newline ()
    | NodeC(values) ->  
      begin
        afficheNode values ind;
        afficheIndontation ind; print_string "-----------> Gauche : ";
        match values.abrGauche with
        | Null -> print_string "Empty \n"
        | Pointeur x -> aux !x (ind+1) ;
        afficheIndontation ind; print_string "-----------> Droite : ";
        match values.abrDroite with
        | Null -> print_string "Empty \n"
        | Pointeur x -> aux !x (ind+1)
      end
  in aux abr 0;
  print_string " \n *************************************************************** \n";
;;



(*------------------------------------------------------------------------------------------------------*)
(*----------- Compression avec la structure de données: Map pour sauvegarder les noeuds *-------------  *)
(*------------------------------------------------------------------------------------------------------*)

module M=Map.Make(String);; (* Creation d'une map avec une clé de type string *)


let labelCptMap= ref 0;;
let noLabelMap="0";;


let insertValueInRefNodeMap abr v tmpL =
  abr.valuesNode <-   M.add tmpL v abr.valuesNode;() 
;;

let getNewValueNodeMap value lg ld g d = 
  let values = M.add "_" value M.empty in
    let content = {
      valuesNode = values;
      labelArcsRouge = ( lg , ld);  
      abrGauche = g;
      abrDroite = d  
    } in 
      NodeC(content)
;;

let newLabelMap () = 
  labelCptMap := !labelCptMap + 1 ; 
  string_of_int !labelCptMap (* Pour caster le int pour un string qui represente la clé *)
;;

let insertCompresseMap (arbre) (toAdd:arb) lab =
  let rec aux abr a listLabel labelArc =
    let p = getAbrCompresseListFromPointeurAbr abr in 
    match a with
    | Empty ->  abr 
    | Node(value, g, d) -> 
      begin
        let cn = getContentNodeFromAbrCompresseList p in
        (*****************on concatene label********************)
          let newListLabel = if labelArc = noLabelMap then listLabel else if listLabel="" then labelArc else (listLabel ^ "." ^ labelArc  ) in
            insertValueInRefNodeMap cn value newListLabel;
            aux cn.abrGauche g newListLabel (fst cn.labelArcsRouge);
            aux cn.abrDroite d newListLabel (snd cn.labelArcsRouge); 
            getNewPointeurAbrFromAbrCompresseList p
      end
  in 
      (***************string prau lieu de [] ******************)
      aux arbre toAdd "" lab
;;

(*------------------------ Compression avec map -----------------------------*)
let getArbreCompresseMapFromAbr arbre = 
  let seen = ref [] in 
    let rec aux abr = 
      match abr with 
      | Empty -> ( noLabelMap , getNewPointeurAbrFromAbrCompresseList EmptyCL )
      | Node (value , g, d ) -> 
        begin 
          let chaine = (chaine_caractere_associe_abr abr) in 
          let (hs, pointeurABR) = (haveSeen chaine !seen) in 
          begin 
            if hs then 
              let tempL = (newLabelMap ()) in 
                (tempL , (insertCompresseMap pointeurABR abr tempL) ) 
            else 
              begin
                let (labelG, abrCompresseG) = (aux g )  
                and (labelD, abrCompresseD) = (aux d ) in
                  let node = getNewValueNodeMap value labelG labelD abrCompresseG abrCompresseD in
                    let pointeurNode = getNewPointeurAbrFromAbrCompresseList node in 
                    (insertToHaveSeen chaine pointeurNode seen) ; (noLabelMap, pointeurNode )
              end
          end
        end
      in
        let (_, res) = aux arbre in
          getAbrCompresseListFromPointeurAbr res
;;

(*------------------------ Recherche avec map -----------------------------*)

let getFirstElementMap map = 
  M.find "_" map
;;

let elementIsInMap map (e: int) listeLabel :(bool * int) =    
  let value = M.find listeLabel map in
  if (value = e) then (true,value)
  else (false,value) 
;;

let rechercheArbreCompresseMap arbreCompresse  (elem : int) : bool = 
  let rec aux abr labels : bool = 
    match abr with
    | EmptyCL -> false
    | NodeC(content) -> match labels with 
      |"" -> 
        begin 
          let valNoeud = getFirstElementMap content.valuesNode
          and lg = fst content.labelArcsRouge
          and ld = snd content.labelArcsRouge
          in 
          if valNoeud = elem then
            true
          else 
            if valNoeud < elem then
              aux (getAbrCompresseListFromPointeurAbr content.abrDroite) (if ld = noLabelMap then "" else ld )
            else  
              aux (getAbrCompresseListFromPointeurAbr content.abrGauche) (if lg = noLabelMap then "" else lg )
        end
      | _ ->
        begin
          let (found, valN) = (elementIsInMap content.valuesNode elem labels) in
          if found then true 
          else 
            begin
              let lg = fst content.labelArcsRouge
              and ld = snd content.labelArcsRouge
              in
              if valN < elem then
                aux (getAbrCompresseListFromPointeurAbr content.abrDroite) (if ld = noLabelMap then labels else labels  ^ "." ^ ld   )
              else   
                aux (getAbrCompresseListFromPointeurAbr content.abrGauche) (if lg = noLabelMap then labels else  labels  ^ "." ^ lg )
              end
            end

    in
      aux arbreCompresse ""
;;




let calculTauxCompression (arbre) = 
  let rec aux abrC nbNoeuds nbElemList = 
    match abrC  with
    | EmptyCL -> (nbNoeuds, nbElemList)
    | _       -> 
      begin
        let content = getContentNodeFromAbrCompresseList abrC in
        let (lg, ld) = content.labelArcsRouge in
        match lg, ld with
        | 0, 0 -> 
          begin
            let abrG = (getAbrCompresseListFromPointeurAbr content.abrGauche)
            and abrD = (getAbrCompresseListFromPointeurAbr content.abrDroite)
            and newList = ((List.length content.valuesNode )::nbElemList) in
            let (m_nbNoeuds, m_nbElemList) = (aux abrG (nbNoeuds+1) newList) in 
            aux abrD m_nbNoeuds m_nbElemList 
          end
        | 0, _ -> 
          begin
            let abrG = (getAbrCompresseListFromPointeurAbr content.abrGauche)
            and newList = ((List.length content.valuesNode )::nbElemList) in
            aux abrG (nbNoeuds+1) newList
          end
        | _, 0 -> 
          begin
            let abrD = (getAbrCompresseListFromPointeurAbr content.abrDroite)
            and newList = ((List.length content.valuesNode )::nbElemList) in
            aux abrD (nbNoeuds+1) newList
          end
        | _, _ -> let newList = ((List.length content.valuesNode )::nbElemList) in ( (nbNoeuds+1), newList)
      end
  in aux arbre 0 []
;;