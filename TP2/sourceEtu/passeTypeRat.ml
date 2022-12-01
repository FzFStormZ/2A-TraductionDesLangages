open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* TODO *)
let rec analyse_type_expression exp =
  match exp with
  | AstTds.Ident iast -> (getType iast, AstType.Ident iast)
  | AstTds.Entier i -> (Int, AstType.Entier i)
  | AstTds.Booleen b -> (Bool, AstType.Booleen b)
  | AstTds.Unaire (u, exp) -> 
      begin
        let (te, ne) = analyse_type_expression exp in
        match u,te with
        | Numerateur, Rat -> (Int, AstType.Unaire (Numerateur, ne))
        | Denominateur, Rat -> (Int, AstType.Unaire (Denominateur, ne))
        | _, _ -> raise (TypeInattendu (_, te))
        
      end
      
  | AstTds.Binaire (b, exp1, exp2) ->
    begin
      let (te_exp1, ne_exp1) = analyse_type_expression exp1 in
      let (te_exp2, ne_exp2) = analyse_type_expression exp1 in

      match b, te_exp1, te_exp2 with
      (* Plus valables *)
      | Plus, Int, Int -> (te_exp1, AstType.Binaire (PlusInt, ne_exp1, ne_exp2))
      | Plus, Rat, Rat -> (te_exp1, AstType.Binaire (PlusRat, ne_exp1, ne_exp2))
      (* Mult valables *)
      | Mult, Int, Int -> (te_exp1, AstType.Binaire (MultInt, ne_exp1, ne_exp2))
      | Mult, Rat, Rat -> (te_exp1, AstType.Binaire (MultRat, ne_exp1, ne_exp2))
      (* Eq valables *)
      | Equ, Int, Int -> (te_exp1, AstType.Binaire (EquInt, ne_exp1, ne_exp2))
      | Equ, Bool, Bool -> (te_exp1, AstType.Binaire (EquBool, ne_exp1, ne_exp2))
      (* Inf valable *)
      | Inf, Int, Int -> (te_exp1, AstType.Binaire (Inf, ne_exp1, ne_exp2))
      (* Fraction valable *)
      | Fraction, Int, Int -> (te_exp1, AstType.Binaire (Fraction, ne_exp1, ne_exp2))
      (* Dans tous les autres cas on s'insurge *)
      | _, _, _ -> raise (TypeBinaireInattendu(b, te_exp1, te_exp2))
    end


(* analyse_tds_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, iast, exp) ->
      let (te, ne) = analyse_type_expression exp in
      if (t = te) then
        begin
          modifier_type_variable t iast;
          AstType.Declaration (iast, ne)
        end
      else 
        raise (TypeInattendu (te, t))
        
  | AstTds.Affectation (iast, exp) ->
      let t = getType iast in
      let (te, ne) = analyse_type_expression exp in
      if (t = te) then
        begin
          modifier_type_variable t iast;
          AstType.Affectation (iast, ne)
        end
      else 
        raise (TypeInattendu (te, t))

  | Affichage exp ->
      let (te, ne) = analyse_type_expression exp in
      begin
        match te with
        | Int   -> AstType.AffichageInt ne
        | Bool  -> AstType.AffichageBool ne
        | Rat   -> AstType.AffichageRat ne
        | _     -> raise (TypeInattendu (te, t))
      end

  | AstTds.Conditionnelle (c, bt, be) ->
    let (te, ne) = analyse_type_expression c in
    if (te = Bool) then
      let nbt = analyse_type_bloc bt in
      let nbe = analyse_type_bloc be in
      AstType.Conditionnelle (ne, nbt, nbe)
    else
      raise (TypeInattendu (te, t))

  | AstTds.TantQue (c, b) ->
    let (te, ne) = analyse_type_expression c in
    if (te = Bool) then
      let nb = analyse_type_bloc b in
      AstType.TantQue (ne, nb)
    else
      raise (TypeInattendu (te, t))

  | AstTds.Retour (iast, exp) ->
    let (te, ne) = analyse_type_expression exp in
    if (t = te) then
      AstType.Retour (iast, ne)
    else
      raise (TypeInattendu (te, t))

(* analyse_tds_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li =
  (* Analyse des types du bloc avec la tds du nouveau bloc.*)
  List.map analyse_type_instruction li


(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (lf,b)) =
  let nlf = analyse_type_fonction lf in
  let nb = analyse_type_bloc b in
  AstType.Programme (nlf,b)




