open Tds
open Ast
open Type
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

let analyse_code_expression exp =
  match exp with
  | AstType.Ident iast ->
    begin
      match info_ast_to_ast iast with
      | InfoConst(_, i) -> TAM.loadl_int i
      | InfoVar(_, t, dep, reg) -> TAM.load (getTaille t) dep reg
      | _ -> failwith "Cas impossible"
    end
  | AstType.Entier i -> TAM.loadl_int i
  | AstType.Booleen b ->
    if b then 
      TAM.loadl_int 1
    else 
      TAM.loadl_int 0
  | AstType.Unaire (u, exp) ->
      (analyse_code_expression exp)
      ^
      begin
        match u with
        | AstType.Numerateur -> TAM.pop 0 1
        | AstType.Denominateur -> TAM.pop 1 1
      end
      
  | AstType.Binaire (b, exp1, exp2) ->
    begin
      (analyse_code_expression exp1)
      ^ (analyse_type_expression exp2)
      ^ 
      match b with
      (* Plus valables *)
      | PlusInt -> Tam.subr "IAdd"
      | PlusRat -> Tam.call "SB" "RAdd"
      (* Mult valables *)
      | MultInt -> Tam.subr "IMul"
      | MultRat -> Tam.call "SB" "RMul"
      (* Eq valables *)
      | EquInt -> Tam.subr "IEq"
      | EquBool -> Tam.call "SB" "REq"
      (* Inf valable *)
      | Inf-> Tam.subr "ILss"
      (* Dans tous les autres cas on s'insurge *)
      | _ -> failwith "Cas impossible"
    end
  | _ -> failwith "TODO"
  
  
let rec analyse_code_bloc (li, taille_bloc) =
  TAM.push taille_bloc 
  ^ List.fold_left (fun acc t -> acc ^ analyse_code_instruction t) "" li
  ^ TAM.pop 0 taille_bloc

and analyse_code_instruction i = 
  match i with
  | AstPlacement.Declaration(iast, exp) ->
    (analyse_code_expression exp)
    ^ TAM.store (getTaille(getType iast)) dep "SB"
  | AstPlacement.Affectation(iast, exp) ->
    (analyse_code_expression exp) 
    ^ TAM.store (getTaille(getType iast)) dep "SB"
  | AstPlacement.AffichageInt(exp) -> 
    (analyse_code_exp exp) 
    ^ TAM.subr "Iout"
  | AstPlacement.AffichageBool(exp) -> 
    (analyse_code_exp exp) 
    ^ TAM.subr "Bout"
  | AstPlacement.AffichageRat(exp) -> 
    (analyse_code_exp exp)
    ^ TAM.call "SB" "Rout"
  | AstPlacement.TantQue(exp, bloc) ->
    let ld = Code.getEtiquette() in
    let lf = Code.getEtiquette() in
    ld 
    ^ (analyse_code_exp exp)
    ^ TAM.jumpif 0 lf
    ^ (analyse_code_bloc bloc)
    ^ TAM.jump ld
    ^ lf
  | AstPlacement.Conditionnelle(exp, bt, be) ->
    let lde = Code.getEtiquette() in
    let lf = Code.getEtiquette() in
    (analyse_code_exp exp)
    ^ TAM.jumpif 0 lde
    ^ (analyse_code_bloc bt)
    ^ TAM.jump lf
    ^ lde
    ^ (analyse_code_bloc be)
    ^ lf
  | AstPlacement.Empty -> ""
  | AstPlacement.Retour(exp, tailleRet, tailleParam) ->
    (analyse_code_exp exp)
    (* ^ dépiler variables locales *)
    ^ TAM.return tailleRet tailleParam

let analyse(AstPlacement.Programme(f,b)) =
  TAM.jump "main"
  ^ Code.getEntete() 
  ^ "" (* analyse_code_fonctions f *) 
  ^ "main\n" 
  ^ analyse_code_bloc b 
  ^ "HALT"