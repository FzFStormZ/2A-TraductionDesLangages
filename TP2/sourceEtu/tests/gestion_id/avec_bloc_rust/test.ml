open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_bloc_rust/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testBoucle1" = 
  let _ = compiler (pathFichiersRat^"testBoucle1.rat") in ()

let%test_unit "testBoucleNommee1" = 
  let _ = compiler (pathFichiersRat^"testBoucleNommee1.rat") in ()

let%test_unit "testBoucleNommee2" = 
  let _ = compiler (pathFichiersRat^"testBoucleNommee2.rat") in ()

let%test_unit "testBoucleNommee3" = 
  let _ = compiler (pathFichiersRat^"testBoucleNommee3.rat") in ()

let%test_unit "testBoucleNommee4"= 
  try 
    let _ = compiler (pathFichiersRat^"testBoucleNommee4.rat") 
    in raise ErreurNonDetectee
  with
  | DoubleDeclarationBoucle("first") -> ()

let%test_unit "testBreak" = 
  let _ = compiler (pathFichiersRat^"testBreak.rat") in ()

let%test_unit "testBreakNommee1" = 
  let _ = compiler (pathFichiersRat^"testBreakNommee1.rat") in ()

let%test_unit "testBreakNommee2"= 
  try 
    let _ = compiler (pathFichiersRat^"testBreakNommee2.rat") 
    in raise ErreurNonDetectee
  with
  | BoucleNommeeNonDeclare("first") -> ()

let%test_unit "testBreakNommee3" = 
  let _ = compiler (pathFichiersRat^"testBreakNommee3.rat") in ()

let%test_unit "testContinue" = 
  let _ = compiler (pathFichiersRat^"testContinue.rat") in ()

let%test_unit "testContinueNommee1" = 
  let _ = compiler (pathFichiersRat^"testContinueNommee1.rat") in ()

let%test_unit "testContinueNommee2"= 
  try 
    let _ = compiler (pathFichiersRat^"testContinueNommee2.rat") 
    in raise ErreurNonDetectee
  with
  | BoucleNommeeNonDeclare("first") -> ()

let%test_unit "testContinueNommee3" = 
  let _ = compiler (pathFichiersRat^"testContinueNommee3.rat") in ()

let%test_unit "testExemple1" = 
  let _ = compiler (pathFichiersRat^"testExemple1.rat") in ()

let%test_unit "testExemple2" = 
  let _ = compiler (pathFichiersRat^"testExemple2.rat") in ()

let%test_unit "testExemple3" = 
  let _ = compiler (pathFichiersRat^"testExemple3.rat") in ()
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
  let p_tam = "../../../../../tests/tam/sans_fonction/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
