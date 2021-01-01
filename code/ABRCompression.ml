(* ------------------------------ Module Principale/Tests ------------------------------- *)

(*------- Les imports -------*)
open  Structures;;
open Fonctions;;
open ABR_utils;;
open ABR_compresse_utils;;
open Printf;; (* Le module est importé pour gérer le format d'écriture dans les fichiers de tests *)

(*------------------------------------------------------------------------------------*)
(*--------------------------------- Les Tests ----------------------------------------*)
(*------------------------------------------------------------------------------------*)

(*
* testConstructionTime nb name: la fonction note dans le dossier "test/testConstruction"
* le temps de construction des arbre compressés avec des listes en se basant sur les "nb" liste presente 
* dans le fichier portant le nom "name"
* Ex: testConstructionTime 7 "test/Jeu_de_tests": construction des 7 arbres compressé de jeu de test donné 
*     dans le site du module. 
*)
let testConstructionTime (nb:int) (name:string)= 
  let out_channel = open_out "test/testConstruction" in
  for i=1 to nb do 
    let in_channel = open_in (name ^ (string_of_int) i ^ ".txt") in
      let l = List.map int_of_string (Str.split (Str.regexp "[^0-9]+") ( input_line in_channel ) ) in
        let abr = newAbr l  in
        fprintf out_channel "%s " ( string_of_int (List.length l)) ; 
        let start = Unix.gettimeofday () in 
        let arbreC = getArbreCompresseListeFromAbr abr in 
        fprintf out_channel "%fs \n" (Unix.gettimeofday () -. start) ; 
    close_in in_channel
    done;
  close_out out_channel
;;

(*
 * testRechercheTime nb: la fonction note dans le dossier "test/testRecherche"
 * le temps de recheche d'une valeur dans les arbres binaires, compresses avec une liste et compressé avec une Map.
 * la fonction génère "nb" arbre de taille à chaque fois plus 1000 valeurs, et pour chaque arb les fonctions
 * de recherche sont appelés (2*la taille de l'arbre) sur une valeurs aléatoire entre 0 à taille de l'arbre.
 * Le résultat saisi sur le fichier pour chaque arbre c'est la moyen des temps de recherche.

 * Le module utilisé pour calculer le temps c'est Unix.
 *)
let testRechercheTime (nb:int)= 
  let out_channel = open_out "test/testRecherche" in
  for i=1 to nb do 
    let l = gen_permutation (i*1000) in 
    (* Creation *)
    let abr = newAbr l in 
    let compListe = getArbreCompresseListeFromAbr abr and compMap = getArbreCompresseMapFromAbr abr in
    let tailleL = (List.length l) and someTimeListe = ref 0. and someTimeMap = ref 0. and someTimeABR = ref 0. in 
    fprintf out_channel "%s " ( string_of_int tailleL ) ;
    
    for j=0 to (tailleL*2) do 
      let r = Random.int (tailleL) in
      let chiffre = List.nth l r in
      (* Recherche ABR *)
      let start = Unix.gettimeofday () in 
      rechercheABR abr chiffre; 
      someTimeABR := !someTimeABR +. (Unix.gettimeofday () -. start ) ;
       
      (* Recherche compresse with liste *)
      let start = Unix.gettimeofday () in 
      rechercheArbreCompresse compListe chiffre; 
      someTimeListe := !someTimeListe +. (Unix.gettimeofday () -. start ) ;

      (* Recherche compresse with map *)
      let start = Unix.gettimeofday () in 
      rechercheArbreCompresseMap compMap chiffre;
      someTimeMap := !someTimeMap +. (Unix.gettimeofday () -. start ) ;

    done;

    fprintf out_channel "%fs " ( !someTimeABR /. (float_of_int)(tailleL * 2)) ; 
    fprintf out_channel "%fs " ( !someTimeListe /. (float_of_int)(tailleL * 2)) ; 
    fprintf out_channel "%fs \n" ( !someTimeMap /. (float_of_int)(tailleL * 2)) ; 
  done;
  close_out out_channel
;;


let testConstructionMemory (nb:int) = 
  let out_chanelALM = open_out ("test/memoryListeALM") in
  let out_chanelAM = open_out ("test/memoryListeAM") in
  for i=1 to nb do
    let l = gen_permutation (i*1000) in 
        let startA = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let abr = newAbr l  in
        let endA = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let tempA =  int_of_float(endA -. startA) in
        let startM = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let arbreCM = getArbreCompresseMapFromAbr abr in 
        let endM = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let tempM =  int_of_float(endM -. startM) in 
        let startL = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let arbreCL = getArbreCompresseListeFromAbr abr in 
        let endL = ( (Gc.stat()).minor_words +. (Gc.stat()).major_words -. (Gc.stat()).promoted_words ) in 
        let tempL =  int_of_float(endL -. startL) in 
        
        Printf.fprintf out_chanelALM "%d %d %d %d\n" (List.length l) (tempA) (tempL) (tempM);
        Printf.fprintf out_chanelAM "%d %d %d\n" (List.length l) (tempA) (tempM);
  done;
;;

let calculMoy liste length = 
  let sum = List.fold_left (+) 0 liste in
  ( (float_of_int sum) /. (float_of_int length))

;;

let testNoeudsInterne (nb:int) (name:string) = 
  let out_chanel_NB = open_out ("test/nombreInterne")
  and out_chanel_moy = open_out ("test/moyenneNoeud") in
  for i=1 to nb do
    let in_channel = open_in (name ^ (string_of_int) i ^ ".txt") in
      let l = List.map int_of_string (Str.split (Str.regexp "[^0-9]+") ( input_line in_channel ) ) in
      let abr = newAbr l  in
      let arbreCL = getArbreCompresseListeFromAbr abr in 
      let (nbNoeuds, listeNoeuds) = calculTauxCompression arbreCL in
      Printf.fprintf out_chanel_NB "%d %d\n" (List.length l) nbNoeuds;
      Printf.fprintf out_chanel_moy "%d %f\n" (List.length l) (calculMoy listeNoeuds nbNoeuds);
  done;
;;

let testNoeudsInterne11 (nb:int) = 
  let out_chanel_NB = open_out ("test/nombreInterneAlea")
  and out_chanel_moy = open_out ("test/moyenneNoeudAlea") in
  for i=1 to nb do
      let l = gen_permutation (i*1000) in 
      let abr = newAbr l  in
      let arbreCL = getArbreCompresseListeFromAbr abr in 
      let (nbNoeuds, listeNoeuds) = calculTauxCompression arbreCL in
      Printf.fprintf out_chanel_NB "%d %d\n" (List.length l) nbNoeuds;
      Printf.fprintf out_chanel_moy "%d %f\n" (List.length l) (calculMoy listeNoeuds nbNoeuds);

  done;
;;



(*------------------------------------------------------------------------------------*)
(*------------------------------ Fonction Principale ---------------------------------*)
(*------------------------------------------------------------------------------------*)
 
let main () =
  (* Random.self_init ();
  let l = [4;11;2;3;12;8;1;9;6;7;5;20;25] in
 (* * dans le fichier portant le nom "name" *)
  let abr = newAbr l  in
  let arbreC = getArbreCompresseListeFromAbr abr in 
  printABRListe arbreC ;
  let (nbN,n) = calculTauxCompression arbreC in
  print_float (calculMoy n nbN) *)
  (* printABRListe arbreC  *)
  (* printABRListe arbreC *)
  (* testConstructionMemory 10;; *)
  (* testNoeudsInterne 7 "test/Jeu_de_tests/" *)
  (* testNoeudsInterne11 50  *)
  testConstructionMemory 18
;;
main () ;;

