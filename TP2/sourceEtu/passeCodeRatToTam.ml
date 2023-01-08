open Tds
open Ast
open Type

type t1 = Ast.AstPlacement.programme
type t2 = string


(* analyse_code_expression : AstPlacement.expression -> string *)
(* Paramètre exp : l'expression à analyser *)
(* Analyser le code RAT d'une expression et le transforme en code TAM *)
let rec analyse_code_affectable a ecriture =
  match a with
  | AstType.Ident iast -> 
      begin
        match info_ast_to_info iast with (* Idem *)
        | InfoConst(_, i) -> Tam.loadl_int i
        | InfoVar(_, t, dep, reg) ->
          if ecriture then
            Tam.store (getTaille t) dep reg
          else
            Tam.load (getTaille t) dep reg
        | _ -> failwith "Cas impossible"
      end
  | AstType.Dref(da, ta) -> (* ajout du type dans la passe de typage à Dref pour cette passe *)
      let s = analyse_code_affectable da false in
      if ecriture then (* faire la difference entre les deux etats de Dref (a droite ou a gauche de l'affectation) *)
        s ^ Tam.storei (getTaille ta)
      else
        s ^ Tam.loadi (getTaille ta)
      

(* analyse_code_expression : AstPlacement.expression -> string *)
(* Paramètre exp : l'expression à analyser *)
(* Analyser le code RAT d'une expression et le transforme en code TAM *)
let rec analyse_code_expression exp =
  match exp with
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
        let nexp1 = analyse_code_expression exp1 in
        let nexp2 = analyse_code_expression exp2 in

        match b with
        (* Plus valables *)
        | PlusInt -> nexp1 ^ nexp2 ^ Tam.subr "IAdd"
        | PlusRat -> nexp1 ^ nexp2 ^ Tam.call "ST" "RAdd"
        (* Mult valables *)
        | MultInt -> nexp1 ^ nexp2 ^ Tam.subr "IMul"
        | MultRat -> nexp1 ^ nexp2 ^ Tam.call "ST" "RMul"
        (* Eq valables *)
        | EquInt -> nexp1 ^ nexp2 ^ Tam.subr "IEq"
        | EquBool -> nexp1 ^ Tam.subr "B2I" ^ nexp2 ^ Tam.subr "B2I" ^ Tam.subr "IEq"
        (* Inf valable *)
        | Inf-> nexp1 ^ nexp2 ^ Tam.subr "ILss"
        (* Fraction valable *)
        | Fraction -> nexp1 ^ nexp2 ^ "\n"(*Tam.call "ST" "norm"*)
      end
  | AstType.AppelFonction(iast, expList) ->
      begin 
        match info_ast_to_info iast with
        | InfoFun(n, _, _) ->
          (* on load l'ensemble des variables utilisées dans l'appel de la fonction *)
          List.fold_left (fun res t -> res ^ analyse_code_expression t) "" expList
          (* call sur le nom de la fonction *)
          ^ Tam.call "ST" n
        | _ -> failwith "Cas impossible"
      end
  | AstType.Affectable a ->
      (analyse_code_affectable a false) (* affectable en tant que expression est en lecture *)
  | AstType.Null -> 
      Tam.loadl_int 0 (* idk ??*)
  | AstType.New t ->
      Tam.loadl_int (getTaille t)
      ^ Tam.subr "MAlloc"
  | AstType.Adress iast -> 
      begin
        match info_ast_to_info iast with
        | InfoVar(_, _, dep, reg) -> 
          Tam.loada dep reg (* on empile l'adresse de la variable *)
        | _ -> failwith "Cas impossible"
      end
  | AstType.Ternaire (c, expVrai, expFaux) ->
      let lFaux = Code.getEtiquette() in
      let lFin = Code.getEtiquette() in

      (* Condition du de l'op ternaire *)
      (analyse_code_expression c)
      ^ Tam.jumpif 0 lFaux
      (* bloc Condition Vraie *)
      ^ (analyse_code_expression expVrai)
      ^ Tam.jump lFin
      (* bloc Condition Fausse *)
      ^ Tam.label lFaux
      ^ (analyse_code_expression expFaux)
      (* Fin *)
      ^ Tam.label lFin
    
  
(* analyse_code_bloc : AstPlacement.bloc -> string *)
(* Paramètre li         : liste d'instructions à analyser *)
(* Paramètre tailleBloc : taille du bloc à analyser *)
(* Analyser le code RAT d'un bloc et le transforme en code TAM *)
let rec analyse_code_bloc (li, tailleBloc) detiq fetiq =
  Tam.push tailleBloc 
  ^ List.fold_left (fun res t -> res ^ analyse_code_instruction t detiq fetiq) "" li
  ^ Tam.pop 0 tailleBloc

(* analyse_code_instruction : AstPlacement.instruction -> string *)
(* Paramètre i    : l'instruction à analyser *)
(* Analyser le code RAT d'une instruction et le transforme en code TAM *)
and analyse_code_instruction i detiq fetiq = 
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
  | AstPlacement.Affectation(a, exp) ->
      (analyse_code_expression exp)
      ^(analyse_code_affectable a true) (* affectable en tant qu'instruction est en ecriture *)
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
      ^ (analyse_code_bloc bloc detiq fetiq) (* le bloc de la boucle *)
      ^ Tam.jump lDebut (* on jump au label de la boucle *)
      ^ Tam.label lFin
    | AstPlacement.Conditionnelle(exp, bt, be) ->
      let lDebutElse = Code.getEtiquette() in
      let lFin = Code.getEtiquette() in

      (* Condition du If *)
      (analyse_code_expression exp)
      ^ Tam.jumpif 0 lDebutElse
      (* bloc If *)
      ^ (analyse_code_bloc bt detiq fetiq)
      ^ Tam.jump lFin
      (* bloc Else *)
      ^ Tam.label lDebutElse
      ^ (analyse_code_bloc be detiq fetiq)
      (* Fin *)
      ^ Tam.label lFin
  | AstPlacement.ElseOptionnel(exp, bt) ->
      let lFin = Code.getEtiquette() in

      (* Condition du If *)
      (analyse_code_expression exp)
      ^ Tam.jumpif 0 lFin
      (* bloc If *)
      ^ (analyse_code_bloc bt detiq fetiq)
      ^ Tam.jump lFin
      (* Fin *)
      ^ Tam.label lFin
  | AstPlacement.Retour(exp, tailleRet, tailleParam) ->
      (analyse_code_expression exp)
      ^ Tam.return tailleRet tailleParam
  | AstPlacement.Empty -> ""
  | AstPlacement.BoucleInfinie (li) ->
      let lDebut = Code.getEtiquette() in
      let lFin = Code.getEtiquette() in

      Tam.label lDebut 
      (* Je donne l'etiquette au bloc pour le break et continue *)
      ^ (analyse_code_bloc li lDebut lFin)
      ^ Tam.jump lDebut
      ^ Tam.label lFin
  | AstPlacement.BoucleInfinieNommee (ia, li) ->
      begin
        match info_ast_to_info ia with
        | InfoBoucle n -> 
            Tam.label ("debut@"^n)
            ^ (analyse_code_bloc li detiq fetiq)
            ^ Tam.jump ("debut@"^n)
            ^ Tam.label ("fin@"^n)
        | _ -> failwith "erreur interne"
      end
  | AstPlacement.Break -> Tam.jump fetiq
  | AstPlacement.BreakNommee (ia) ->
      begin
        match info_ast_to_info ia with
        | InfoBoucle n -> Tam.jump ("fin@"^n)
        | _ -> failwith "erreur interne"
      end 
  | AstPlacement.Continue -> Tam.jump detiq
  | AstPlacement.ContinueNommee (ia) ->
      begin
        match info_ast_to_info ia with
        | InfoBoucle n -> Tam.jump ("debut@"^n)
        | _ -> failwith "erreur interne"
      end 

(* analyser_code_fonction : AstPlacement.fonction -> string *)
(* Paramètre f : la fonction à analyser *)
(* Analyser le code RAT d'une fonction et le transforme en code TAM *)
let analyser_code_fonction (AstPlacement.Fonction (iast, _, b)) = 
  match info_ast_to_info iast with
  | InfoFun(n, _, _) ->
      Tam.label n
      (* analyse le bloc de la fonction *)
      ^ (analyse_code_bloc b "" "")  (* dépiler variables locales -> est dejà fait dans analyse_code_bloc *)
      ^ Tam.halt (* force l'arret si pas de RETURN *)
  | _ -> failwith "Cas impossible"

(* analyser : AstPlacement.programme -> string *)
(* Paramètre p : le programme à analyser *)
(* Analyser le code RAT d'un programme et le transforme en code TAM *)
let analyser(AstPlacement.Programme(lf,b)) =
  Code.getEntete() (* contient déjà un JUMP main *)
  ^ List.fold_left (fun res f -> res ^ analyser_code_fonction f) "" lf
  ^ Tam.label "main"
  ^ analyse_code_bloc b "" ""
  ^ Tam.halt