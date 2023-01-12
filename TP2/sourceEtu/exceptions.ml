open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string 
exception IdentifiantNonDeclare of string 
exception MauvaiseUtilisationIdentifiant of string
exception DoubleDeclarationBoucle of string (* on ne l'accepte pas comparé à Rust qui lève seulement un warning *)
exception BoucleNommeeNonDeclare of string (* dans le cas où l'indentifiant d'un break ou continue n'appartient à aucune boucle *)
exception BreakHorsBoucle
exception ContinueHorsBoucle

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ (* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)
exception TypeTernaireDifferent of typ * typ (* le premier typ de la condition vrai et le deuxieme typ de la condition fausse *)

(* Utilisation illégale de return dans le programme principal *)
exception RetourDansMain
