open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme


(* analyse_placement_affectable : AstType.affectable -> AstType.affectable *)
(* Paramètre a : l'affectable à analyser *)
(* Aucune modification durant la passe de placement en mémoire *)
let analyse_placement_affectable a = a


(* analyse_placement_expression : AstType.expression -> AstType.expression *)
(* Paramètre exp : l'expression à analyser *)
(* Aucune modification durant la passe de placement en mémoire *)
let analyse_placement_expression exp = exp


(* analyse_placement_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i    : l'instruction à analyser *)
(* Paramètre reg  : registre *)
(* Paramètre depl : deplacement par rapport au registre *)
(* Analyser le placement en mémoire d'une instruction et transforme 
l'instruction en une instruction de type AstPlacement.instruction *)
let rec analyse_placement_instruction i reg depl = 
  match i with
  | AstType.Declaration (iast, exp) ->
      modifier_adresse_variable depl reg iast;
      (AstPlacement.Declaration (iast, exp), getTaille(getType iast))
  | AstType.Affectation(a, exp) -> (AstPlacement.Affectation(a, exp), 0)
  | AstType.AffichageInt(exp) -> (AstPlacement.AffichageInt(exp), 0)
  | AstType.AffichageBool(exp) -> (AstPlacement.AffichageBool(exp), 0)
  | AstType.AffichageRat(exp) -> (AstPlacement.AffichageRat(exp), 0)
  | AstType.Conditionnelle(exp, bt, be) ->
      let nbt = analyse_placement_bloc bt reg depl in
      let nbe = analyse_placement_bloc be reg depl in
      (AstPlacement.Conditionnelle(exp, nbt, nbe), 0) (* bloc donc taille 0 *)
  | AstType.ElseOptionnel(exp, bt) ->
      let nbt = analyse_placement_bloc bt reg depl in
      (AstPlacement.ElseOptionnel(exp, nbt), 0) (* bloc donc taille 0 *)
  | AstType.TantQue(exp, bloc) ->
      let nb = analyse_placement_bloc bloc reg depl in
      (AstPlacement.TantQue(exp, nb), 0) (* bloc donc taille 0 *)
  | AstType.Retour(exp, iast) ->
    begin
      match info_ast_to_info iast with
      | InfoFun(_, typ, typList) ->
        (* taille du type de retour *)
        let tailleRet = getTaille typ in
        (* taille de l'ensemble des parametres *)
        let tailleParam =  List.fold_right (fun t res -> res + getTaille (t)) typList 0 in
        (AstPlacement.Retour(exp, tailleRet, tailleParam), 0) (* bloc donc taille 0 *)
      | _ -> failwith "Cas impossible"
    end
  | AstType.Empty -> (AstPlacement.Empty, 0)
  | AstType.BoucleInfinie (li) ->
      (AstPlacement.BoucleInfinie(analyse_placement_bloc li reg depl), 0) (* bloc donc taille 0 *)
  | AstType.BoucleInfinieNommee (ia, li) -> 
      (AstPlacement.BoucleInfinieNommee(ia, analyse_placement_bloc li reg depl), 0) (* bloc donc taille 0 *)
  | AstType.Break -> (AstPlacement.Break, 0)
  | AstType.BreakNommee (ia) -> (AstPlacement.BreakNommee (ia), 0)
  | AstType.Continue -> (AstPlacement.Continue, 0)
  | AstType.ContinueNommee (ia) -> (AstPlacement.ContinueNommee(ia), 0)


(* analyse_placement_bloc : AstType.bloc -> AstPlacement.bloc *)
(* Paramètre li   : liste d'instructions à analyser *)
(* Paramètre reg  : registre *)
(* Paramètre depl : deplacement par rapport au registre *)
(* Analyser le placement en mémoire d'un bloc et transforme
le bloc en un bloc de type AstPlacement.bloc *)
and analyse_placement_bloc li reg depl =
  match li with
  | [] -> ([], 0)
  | i::q -> 
    let (ni, taille_i) = analyse_placement_instruction i reg depl in
    let (nq, taille_q) = analyse_placement_bloc q reg (depl + taille_i) in
    (ni::nq, taille_i + taille_q)


(* analyse_placement_fonction : AstType.fonction -> AstPlacement.fonction *)
(* Paramètre f : la fonction à analyser *)
(* Analyser le placement en mémoire d'une fonction et transforme
la fonction en une fonction de type AstPlacement.fonction *)
let analyse_placement_fonction (AstType.Fonction (iast, lp, bloc)) = 
  (* parametres de la fonction *)
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
  (* bloc d'instructions de la fonction *)
  let nb = analyse_placement_bloc bloc "LB" 3 in
  AstPlacement.Fonction(iast, lp, nb)


(* analyser : AstType.programme -> AstPlacement.programme *)
(* Paramètre p : le programme à analyser *)
(* Analyser le placement en mémoire d'un programme et transforme le programme
en un programme de type AstPlacement.programme *)
let analyser (AstType.Programme (lf,b)) = 
  let nlf = List.map analyse_placement_fonction lf in
  let nb = analyse_placement_bloc b "SB" 0 in
  AstPlacement.Programme (nlf,nb)
  