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
  (* Sys.remove tamfile; *)    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/avec_pointeur/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%expect_test "testAffect" =
  runtam (pathFichiersRat^"testAffect.rat");
  [%expect{| 42 |}]

let%expect_test "testAdress" =
  runtam (pathFichiersRat^"testAdress.rat");
  [%expect{| 3 |}]

let%expect_test "testAffect2" =
  runtam (pathFichiersRat^"testAffect2.rat");
  [%expect{| 423 |}]

let%expect_test "testPointeur2" =
  runtam (pathFichiersRat^"testPointeur2.rat");
  [%expect{| 42 |}]
