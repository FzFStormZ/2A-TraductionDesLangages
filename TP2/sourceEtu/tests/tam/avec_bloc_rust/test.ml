open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/avec_bloc_rust/fichiersRat/"

(**********)
(*  TESTS *)
(**********)


let%expect_test "testExemple0" =
  runtam (pathFichiersRat^"testExemple0.rat");
  [%expect{| 01 |}]

let%expect_test "testExemple1" =
  runtam (pathFichiersRat^"testExemple1.rat");
  [%expect{| 012345678910 |}]

let%expect_test "testExemple2" =
  runtam (pathFichiersRat^"testExemple2.rat");
  [%expect{| 1266645 |}]

let%expect_test "testExemple3" =
  runtam (pathFichiersRat^"testExemple3.rat");
  [%expect{| 00010203040506101112131415162021222324252630313233343536 |}]

let%expect_test "testExemple4" =
  runtam (pathFichiersRat^"testExemple4.rat");
  [%expect{| WARNING : 2 boucles imbriquées de même nom !00011011 |}]
