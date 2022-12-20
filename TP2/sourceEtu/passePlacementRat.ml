open Tds
open Exceptions
open Ast

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme


(* analyse_placement_expression : AstType.expression -> AstPlacement.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie le bon placement de l'expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_placement_expression exp = exp

(* analyse_placement_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_placement_instruction i reg depl = 
  match i with
  | AstType.Declaration (iast, exp) ->
    modifier_adresse_variable depl reg iast;
    (AstPlacement.Declaration (iast, exp), getTaille(getType iast))
  | AstType.Affectation(iast, exp) ->
    (AstPlacement.Affectation(iast, exp), 0)
  | AstType.AffichageInt(exp) -> (AstPlacement.AffichageInt(exp), 0)
  | AstType.AffichageBool(exp) -> (AstPlacement.AffichageBool(exp), 0)
  | AstType.AffichageRat(exp) -> (AstPlacement.AffichageRat(exp), 0)
  | AstType.Conditionnelle(exp, bt, be) ->
    let nbt = analyse_placement_bloc bt reg depl in
    let nbe = analyse_placement_bloc be reg depl in
    (AstPlacement.Conditionnelle(exp, nbt, nbe), 0)
  | AstType.TantQue(exp, bloc) ->
    let nb = analyse_placement_bloc bloc reg depl in
    (AstPlacement.TantQue(exp, nb), 0)
  | AstType.Retour(exp, iast) ->
    begin
      match info_ast_to_info iast with
      | InfoFun(_, typ, typList) ->
        let taille_ret = getTaille typ in
        let taille_params =  List.fold_right (fun t res -> res + getTaille (t)) typList 0 in
        (AstPlacement.Retour(exp, taille_ret, taille_params), 0)
      | _ -> failwith "Cas impossible"
    end
  | AstType.Empty -> (AstPlacement.Empty, 0)

(* analyse_placement_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_placement_bloc li registre deplacement =
  match li with
  | [] -> ([], 0)
  | i::q -> 
    let (ni, taille_i) = analyse_placement_instruction i registre deplacement in
    let (nq, taille_q) = analyse_placement_bloc q registre (deplacement + taille_i) in
    (ni::nq, taille_i + taille_q)

(* analyse_placement_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie le bon typage de la fonction *)
(* Erreur si mauvais typage *)
let analyse_placement_fonction (AstType.Fonction (iast, lp, bloc)) = 
  (* parametres *)
  let rec placement_parametre lp basePointer = 
    match lp with
    | [] -> ()
    | head::tail ->
      begin
        match info_ast_to_info head with
        | InfoVar (_, t, _, _) ->
          modifier_adresse_variable (basePointer - getTaille t) "LB" head;
          placement_parametre tail (basePointer - getTaille t)
        | _ -> failwith "Cas impossible"
      end
    in
  placement_parametre (List.rev lp) 0;
  
  (* bloc d'instructions *)
  let nb = analyse_placement_bloc bloc "LB" 3 in

  AstPlacement.Fonction(iast, lp, nb)

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstType.Programme (lf,b)) = 
  let nlf = List.map analyse_placement_fonction lf in
  let nb = analyse_placement_bloc b "SB" 0 in
  AstPlacement.Programme (nlf,nb)