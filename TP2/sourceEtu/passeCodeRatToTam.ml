open Tds
open Ast
open Type
(*open Code*)

type t1 = Ast.AstPlacement.programme
type t2 = string

let rec analyse_code_expression exp =
  match exp with
  | AstType.Ident iast ->
    begin
      match info_ast_to_info iast with
      | InfoConst(_, i) -> Tam.loadl_int i
      | InfoVar(_, t, dep, reg) -> Tam.load (getTaille t) dep reg
      | _ -> failwith "Cas impossible"
    end
  | AstType.Entier i -> Tam.loadl_int i
  | AstType.Booleen b -> Tam.loadl_int (if b then 1 else 0)
  | AstType.Unaire (u, exp) ->
      (analyse_code_expression exp)
      ^
      begin
        match u with
        | AstType.Numerateur -> Tam.pop 0 1 (* supprime celui en haut de la pile *)
        | AstType.Denominateur -> Tam.pop 1 1 (* créé un trou -> store ce qu'il y a en haut en bas *)
      end
      
  | AstType.Binaire (b, exp1, exp2) ->
    begin
      (analyse_code_expression exp1)
      ^ (analyse_code_expression exp2)
      ^ 
      match b with
      (* Plus valables *)
      | PlusInt -> Tam.subr "IAdd"
      | PlusRat -> Tam.call "ST" "RAdd"
      (* Mult valables *)
      | MultInt -> Tam.subr "IMul"
      | MultRat -> Tam.call "ST" "RMul"
      (* Eq valables *)
      | EquInt -> Tam.subr "IEq"
      | EquBool -> Tam.call "ST" "REq"
      (* Inf valable *)
      | Inf-> Tam.subr "ILss"
      (* Fraction valable *)
      | Fraction -> "\n"(*Tam.call "ST" "norm"*)
    end
  | _ -> failwith "TODO"
  
  
let rec analyse_code_bloc (li, taille_bloc) =
  Tam.push taille_bloc 
  ^ List.fold_left (fun res t -> res ^ analyse_code_instruction t) "" li
  ^ Tam.pop 0 taille_bloc

and analyse_code_instruction i = 
  match i with
  | AstPlacement.Declaration(iast, exp) ->
    begin
      match info_ast_to_info iast with
        | InfoVar(_, t, dep, reg) ->
          Tam.push (getTaille t)
          ^ analyse_code_expression exp
          ^ Tam.store (getTaille t) dep reg
        | _ -> failwith "Erreur interne"
    end
  | AstPlacement.Affectation(iast, exp) ->
    begin
      match info_ast_to_info iast with
        | InfoVar(_, t, dep, reg) ->
          analyse_code_expression exp
          ^ Tam.store (getTaille t) dep reg
        | _ -> failwith "Erreur interne"
    end
  | AstPlacement.AffichageInt(exp) -> 
    (analyse_code_expression exp) 
    ^ Tam.subr "Iout"
  | AstPlacement.AffichageBool(exp) -> 
    (analyse_code_expression exp) 
    ^ Tam.subr "Bout"
  | AstPlacement.AffichageRat(exp) -> 
    (analyse_code_expression exp)
    ^ Tam.call "SB" "Rout"
  | AstPlacement.TantQue(exp, bloc) ->
    let lDebut = Code.getEtiquette() in
    let lFin = Code.getEtiquette() in
    Tam.label lDebut 
    ^ (analyse_code_expression exp) (* condition d'arret *)
    ^ Tam.jumpif 0 lFin
    ^ (analyse_code_bloc bloc) (* le bloc de la boucle *)
    ^ Tam.jump lDebut (* on jump au label de la boucle *)
    ^ Tam.label lFin
  | AstPlacement.Conditionnelle(exp, bt, be) ->
    let lDebutElse = Code.getEtiquette() in
    let lFin = Code.getEtiquette() in
    (* Condition du If *)
    (analyse_code_expression exp)
    ^ Tam.jumpif 0 lDebutElse
    (* bloc If *)
    ^ (analyse_code_bloc bt)
    ^ Tam.jump lFin
    (* bloc Else *)
    ^ Tam.label lDebutElse
    ^ (analyse_code_bloc be)
    (* Fin *)
    ^ Tam.label lFin
  | AstPlacement.Retour(exp, tailleRet, tailleParam) ->
    (analyse_code_expression exp)
    (* ^ dépiler variables locales *)
    ^ Tam.return tailleRet tailleParam
  | AstPlacement.Empty -> ""
    
let analyser_code_fonction f = ""

let analyser(AstPlacement.Programme(f,b)) =
  Tam.jump "main"
  ^ Code.getEntete() 
  ^ analyser_code_fonction f
  ^ Tam.label "main"
  ^ analyse_code_bloc b 
  ^ Tam.halt
