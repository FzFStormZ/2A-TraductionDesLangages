open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* TODO *)
let rec analyse_type_expression exp =
  match exp with    
  | AstTds.Ident iast -> (getType iast, AstType.Ident iast)
  | AstTds.Entier i -> (Type.Int, AstType.Entier i)
  | AstTds.Booleen b -> (Type.Bool, AstType.Booleen b)
  | AstTds.Unaire (u, exp) ->
      let (te, ne) = analyse_type_expression exp in 
      if (te <> Type.Rat) then 
        raise (TypeInattendu (te, Type.Rat))
      else
        begin
          match u with
          | AstSyntax.Numerateur -> (Int, AstType.Unaire(AstType.Numerateur, ne))
          | AstSyntax.Denominateur -> (Int, AstType.Unaire(AstType.Denominateur, ne))
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
      (* | Fraction, _, Int -> raise (TypeBinaireInattendu(b, te_exp1, Type.Int))
      | Fraction, Int, _ -> raise (TypeBinaireInattendu(b, te_exp2, Type.Int)) *)
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
          modifier_type_variable te iast;
          AstType.Declaration (iast, ne)
        end
      else 
        raise (TypeInattendu (te, t))
        
  | AstTds.Affectation (iast, exp) ->
      let t = getType iast in
      let (te, ne) = analyse_type_expression exp in
      if (t = te) then
        begin
          AstType.Affectation (iast, ne)
        end
      else 
        raise (TypeInattendu (te, t))

  | AstTds.Affichage exp ->
      let (te, ne) = analyse_type_expression exp in
      begin
        match te with
        | Type.Int   -> AstType.AffichageInt ne
        | Type.Bool  -> AstType.AffichageBool ne
        | Type.Rat   -> AstType.AffichageRat ne
        | t     -> raise (TypeInattendu (te, t)) (*raise erreur interne*)
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
    let (te, ne) = analyse_type_expression exp in
    let t = getType iast in
    if (te = t) then
      AstType.Retour (ne, iast)
    else
      raise (TypeInattendu (te, t))
  
    | AstTds.Empty -> AstType.Empty

(* analyse_tds_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li =
  (* Analyse des types du bloc avec la tds du nouveau bloc.*)
  List.map analyse_type_instruction li

 
let ajouter_type te iast =
  let t = getType iast in
  if (te = t) then
    begin
      modifier_type_variable te iast;

    end


let analyse_type_fonction lf = 
  match lf with
  | (AstTds.Fonction(te,iast,lp_typ_iast,li))::tail -> 
    begin
      let t = getType iast in
      if (te = t) then
        (* TO DO *)
        let lie = analyse_type_bloc li in
        let lp_iast = List.map (fun (te, iast) -> ajouter_type te iast) lp in
        
        modifier_type_fonction te lp iast;
        AstType.Fonction(iast, lp_iast, lie)
      else
        raise (TypeInattendu (t, te))

    end

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (lf,b)) =
  (* let nlf = analyse_type_fonction lf in *)
  let nlf = List.map analyse_type_fonction lf in
  let nb = analyse_type_bloc b in
  AstType.Programme (nlf,nb)
