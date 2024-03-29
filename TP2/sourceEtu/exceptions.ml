open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string 
exception IdentifiantNonDeclare of string 
exception MauvaiseUtilisationIdentifiant of string
exception BreakHorsBoucle (* Break qui n'est dans aucune boucle *)
exception ContinueHorsBoucle (* Conitnue qui n'est dans aucune boucle *)
exception BreakNommeeAutreBoucle of string (* Dans le cas où on a un break nomme qui n'est pas dans la boucle du même nom *)
exception ContinueNommeeAutreBoucle of string (* Dans le cas où on a un continue nomme qui n'est pas dans la boucle du même nom *)

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ (* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)
exception TypeTernaireDifferent of typ * typ (* le premier typ de la condition vrai et le deuxieme typ de la condition fausse *)

(* Utilisation illégale de return dans le programme principal *)
exception RetourDansMain
