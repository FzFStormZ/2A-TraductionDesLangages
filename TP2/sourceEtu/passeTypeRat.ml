(* Module de la passe de typage *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* analyse_type_expression : AstTds.expression -> type * AstType.expression*)
(* Paramètre exp : l'expression à analyser *)
(* Vérifie le bon typage de l'expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression exp =
  match exp with
  | AstTds.Ident iast -> (getType iast, AstType.Ident iast)
  | AstTds.Entier i -> (Type.Int, AstType.Entier i)
  | AstTds.Booleen b -> (Type.Bool, AstType.Booleen b)
  | AstTds.Unaire (u, exp) ->
      let (te, ne) = analyse_type_expression exp in 
      (* seulement le cas d'un Rationnel *)
      if (te <> Type.Rat) then 
        raise (TypeInattendu (te, Type.Rat))
      else
        begin
          match u with
          | AstSyntax.Numerateur -> (Type.Int, AstType.Unaire(AstType.Numerateur, ne))
          | AstSyntax.Denominateur -> (Type.Int, AstType.Unaire(AstType.Denominateur, ne))
        end
  | AstTds.Binaire (b, exp1, exp2) ->
    begin
      let (te_exp1, ne_exp1) = analyse_type_expression exp1 in
      let (te_exp2, ne_exp2) = analyse_type_expression exp2 in

      match b, te_exp1, te_exp2 with
      (* Plus valables *)
      | Plus, Int, Int -> (Type.Int, AstType.Binaire (PlusInt, ne_exp1, ne_exp2))
      | Plus, Rat, Rat -> (Type.Rat, AstType.Binaire (PlusRat, ne_exp1, ne_exp2))
      (* Mult valables *)
      | Mult, Int, Int -> (Type.Int, AstType.Binaire (MultInt, ne_exp1, ne_exp2))
      | Mult, Rat, Rat -> (Type.Rat, AstType.Binaire (MultRat, ne_exp1, ne_exp2))
      (* Eq valables *)
      | Equ, Int, Int -> (Type.Bool, AstType.Binaire (EquInt, ne_exp1, ne_exp2))
      | Equ, Bool, Bool -> (Type.Bool, AstType.Binaire (EquBool, ne_exp1, ne_exp2))
      (* Inf valable *)
      | Inf, Int, Int -> (Type.Bool, AstType.Binaire (Inf, ne_exp1, ne_exp2))
      (* Fraction valable *)
      | Fraction, Int, Int -> (Type.Rat, AstType.Binaire (Fraction, ne_exp1, ne_exp2))
      (* Dans tous les autres cas on s'insurge *)
      | _, _, _ -> raise (TypeBinaireInattendu(b, te_exp1, te_exp2))
    end
  | AstTds.AppelFonction (iast, expList) ->
    match info_ast_to_info iast with 
    (* Seul cas possible *)
    | InfoFun(_, typ, typList) -> 
        let (typListE, li) = List.split (List.map analyse_type_expression expList) in
        (* On compare les 2 listes de "typ" de la fonctions *)
        if (typListE = typList) then 
          (typ, AstType.AppelFonction(iast, li))
        else 
          raise (TypesParametresInattendus (typListE, typList))
    | _ -> failwith "Cas impossible"


(* analyse_tds_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et transforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, iast, exp) ->
      let (te, ne) = analyse_type_expression exp in
      if (t = te) then
        begin
          modifier_type_variable te iast;
          AstType.Declaration (iast, ne)
        end
      else 
        raise (TypeInattendu (te, t))
  | AstTds.Affectation (iast, exp) ->
      let t = getType iast in
      let (te, ne) = analyse_type_expression exp in
      if (t = te) then
        AstType.Affectation (iast, ne)
      else 
        raise (TypeInattendu (te, t))
  | AstTds.Affichage exp ->
      let (te, ne) = analyse_type_expression exp in
      begin
        match te with
        | Type.Int -> AstType.AffichageInt ne
        | Type.Bool -> AstType.AffichageBool ne
        | Type.Rat -> AstType.AffichageRat ne
        | t -> raise (TypeInattendu (te, t)) (*raise erreur interne*)
      end
  | AstTds.Conditionnelle (cond, bt, be) ->
    let (te, ne) = analyse_type_expression cond in
    if (te = Type.Bool) then
      let nbt = analyse_type_bloc bt in
      let nbe = analyse_type_bloc be in
      AstType.Conditionnelle (ne, nbt, nbe)
    else
      raise (TypeInattendu (te, Type.Bool))
  | AstTds.TantQue (cond, bloc) ->
    let (te, ne) = analyse_type_expression cond in
    if (te = Type.Bool) then
      let nbloc = analyse_type_bloc bloc in
      AstType.TantQue (ne, nbloc)
    else
      raise (TypeInattendu (te, Type.Bool))
  | AstTds.Retour (exp, iast) ->
    begin
      let (te, ne) = analyse_type_expression exp in
      match info_ast_to_info iast with
      | InfoFun(_, typ, _) -> 
        if (te = typ) then 
          AstType.Retour (ne, iast)
        else 
          raise (TypeInattendu (te, typ))
      | _ -> failwith "Cas impossible"
    end
  | AstTds.Empty -> AstType.Empty


(* analyse_tds_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et transforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li =
  (* Analyse des types du bloc avec la tds du nouveau bloc.*)
  List.map analyse_type_instruction li


(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre f : la fonction à analyser *)
(* Vérifie le bon typage de la fonction *)
(* Erreur si mauvais typage *)
let analyse_type_fonction (AstTds.Fonction (typ, info, lp, bloc)) = 
  (* pour chaque parametres de la fonction, on ajoute son type dans son iast *)
  List.iter (fun (typ, iast) -> modifier_type_variable typ iast) lp;
  (* on fait pareil pour l'iast de la fonction en ajoutant son type de retour 
  et l'ensemble des types de ses parametres *)
  let (typList, iastList) = List.split lp in
  modifier_type_fonction typ typList info;
  (* on analyse le bon typage des variables locales dans le bloc de la fonction *)
  let nb = analyse_type_bloc bloc in
  AstType.Fonction(info, iastList, nb)


(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et transforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (lf,b)) =
  let nlf = List.map analyse_type_fonction lf in
  let nb = analyse_type_bloc b in
  AstType.Programme (nlf,nb)
