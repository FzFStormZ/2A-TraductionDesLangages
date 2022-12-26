open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/type/avec_pointeur/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testGrosDebugCorrect" = 
  let _ = compiler (pathFichiersRat^"testGrosDebugCorrect.rat") in ()

let%test_unit "testAdress"= 
  try 
    let _ = compiler (pathFichiersRat^"testAdress.rat") 
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Pointeur Rat, Pointeur Int) -> ()

let%test_unit "testAffect"= 
try 
  let _ = compiler (pathFichiersRat^"testAffect.rat") 
  in raise ErreurNonDetectee
with
| TypeInattendu(Rat, Int) -> ()

let%test_unit "testAffect2"= 
try 
  let _ = compiler (pathFichiersRat^"testAffect2.rat") 
  in raise ErreurNonDetectee
with
| TypeInattendu(Int, Rat) -> ()

let%test_unit "testNew"= 
try 
  let _ = compiler (pathFichiersRat^"testNew.rat") 
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur Int) -> ()

let%test_unit "testNew2"= 
try 
  let _ = compiler (pathFichiersRat^"testNew2.rat") 
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Int, Pointeur Rat) -> ()

let%test_unit "testNull"= 
  let _ = compiler (pathFichiersRat^"testNull.rat") in ()

(* Fichiers de tests de la génération de code -> doivent passer la TDS *)
open Unix
open Filename

let rec test d p_tam = 
  try 
    let file = readdir d in
    if (check_suffix file ".rat") 
    then
    (
     try
       let _ = compiler  (p_tam^file) in (); 
     with e -> print_string (p_tam^file); print_newline(); raise e;
    )
    else ();
    test d p_tam
  with End_of_file -> ()

let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_pointeur/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
