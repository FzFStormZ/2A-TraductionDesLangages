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
  let _ = compiler (pathFichiersRat^"testBoucleNommee4.rat") in ()

let%test_unit "testBreak" = 
  let _ = compiler (pathFichiersRat^"testBreak.rat") in ()

let%test_unit "testBreakNommee1" = 
  let _ = compiler (pathFichiersRat^"testBreakNommee1.rat") in ()

let%test_unit "testBreakNommee2"= 
  try 
    let _ = compiler (pathFichiersRat^"testBreakNommee2.rat") 
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("first") -> ()

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
  | IdentifiantNonDeclare("first") -> ()

let%test_unit "testContinueNommee3" = 
  let _ = compiler (pathFichiersRat^"testContinueNommee3.rat") in ()

let%test_unit "testBlocRustExemple1" = 
  let _ = compiler (pathFichiersRat^"testBlocRustExemple1.rat") in ()

let%test_unit "testBlocRustExemple2" = 
  let _ = compiler (pathFichiersRat^"testBlocRustExemple2.rat") in ()

let%test_unit "testBlocRustExemple3" = 
  let _ = compiler (pathFichiersRat^"testBlocRustExemple3.rat") in ()

let%test_unit "testBreakHorsBoucle" = 
  try 
    let _ = compiler (pathFichiersRat^"testBreakHorsBoucle.rat")
    in raise ErreurNonDetectee
  with
  | BreakHorsBoucle -> ()

let%test_unit "testContinueHorsBoucle" =   
  try 
    let _ = compiler (pathFichiersRat^"testContinueHorsBoucle.rat")
    in raise ErreurNonDetectee
  with
  | ContinueHorsBoucle -> ()


let%test_unit "testBreakNommeeAutreBoucle" =   
  try 
    let _ = compiler (pathFichiersRat^"testBreakNommeeAutreBoucle.rat")
    in raise ErreurNonDetectee
  with
  | BreakNommeeAutreBoucle("first") -> ()

let%test_unit "testContinueNommeeAutreBoucle" =   
  try 
    let _ = compiler (pathFichiersRat^"testContinueNommeeAutreBoucle.rat")
    in raise ErreurNonDetectee
  with
  | ContinueNommeeAutreBoucle("first") -> ()
  
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
