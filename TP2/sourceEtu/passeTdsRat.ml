(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


(* analyse_tds_affectable : tds -> AstSyntax.affectable -> AstTds.affectable *)
(* Paramètre tds      : la table des symboles courante *)
(* Paramètre a        : l'affectable à analyser *)
(* Paramètre ecriture : accès en ecriture (true) ou lecture (false) de la valeur pointée en a *)
(* Vérifie la bonne utilisation des identifiants et transforme l'affectable
en un affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds a ecriture =
  match a with
  | AstSyntax.Ident ident ->
    begin
      match chercherGlobalement tds ident with
      | None -> raise (IdentifiantNonDeclare ident)
      | Some iast -> 
        begin
          match info_ast_to_info iast with
          | InfoVar _ -> AstTds.Ident iast
          | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant ident)
          | InfoConst _ -> 
            if ecriture then (* On ne peut pas écrire dans une constante *)
              raise (MauvaiseUtilisationIdentifiant ident)
            else 
              AstTds.Ident iast
          | InfoBoucle _ -> raise (MauvaiseUtilisationIdentifiant ident)
        end
    end
  | AstSyntax.Dref da ->
      let nda = analyse_tds_affectable tds da ecriture in
      AstTds.Dref nda


(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds  : la table des symboles courante *)
(* Paramètre e    : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
  match e with
  | AstSyntax.Entier i -> AstTds.Entier i
  | AstSyntax.Booleen b -> AstTds.Booleen b
  | AstSyntax.Binaire (op, exp1, exp2) -> AstTds.Binaire (op, analyse_tds_expression tds exp1, analyse_tds_expression tds exp2)
  | AstSyntax.Unaire (unaire, expression) -> AstTds.Unaire (unaire, analyse_tds_expression tds expression)
  | AstSyntax.AppelFonction (n, lp) -> 
      begin
        match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some a ->
          begin
            match info_ast_to_info a with
            | InfoFun _ ->
                AstTds.AppelFonction (a, List.map (fun expr -> analyse_tds_expression tds expr) lp)
            | _ -> raise (MauvaiseUtilisationIdentifiant n)
          end
      end
  | AstSyntax.Affectable a -> (* Comme c'est dans le cas d'une expression, on n'est pas en mode écriture *)
      AstTds.Affectable (analyse_tds_affectable tds a false)
  | AstSyntax.Adress ident ->
    begin
      match chercherGlobalement tds ident with
      | None -> raise (IdentifiantNonDeclare ident)
      | Some iast -> 
        begin
          match info_ast_to_info iast with
          | InfoVar _ -> AstTds.Adress iast
          | _ -> raise (MauvaiseUtilisationIdentifiant ident)
        end
    end
  | AstSyntax.Null -> AstTds.Null
  | AstSyntax.New t -> AstTds.New t
  | AstSyntax.Ternaire (c, expVrai, expFaux) -> 
      AstTds.Ternaire (analyse_tds_expression tds c, analyse_tds_expression tds expVrai, analyse_tds_expression tds expFaux)


(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds  : la table des symboles courante *)
(* Paramètre oia  : None si l'instruction i est dans le bloc principal,
                    Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre loia : None si l'instruction i n'est pas dans une boucle,
                    Some ia où ia est l'information associée à la boucle dans laquelle est l'instruction i sinon *)
(* Paramètre i    : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia loia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
    begin
      match chercherLocalement tds n with
      | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
          il n'a donc pas été déclaré dans le bloc courant *)
          (* Vérification de la bonne utilisation des identifiants dans l'expression *)
          (* et obtention de l'expression transformée *)
          let ne = analyse_tds_expression tds e in
          (* Création de l'information associée à l'identfiant *)
          let info = InfoVar (n,Undefined, 0, "") in
          (* Création du pointeur sur l'information *)
          let ia = info_to_info_ast info in
          (* Ajout de l'information (pointeur) dans la tds *)
          ajouter tds n ia;
          (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
          et l'expression remplacée par l'expression issue de l'analyse *)
          AstTds.Declaration (t, ia, ne)
      | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
    end
  | AstSyntax.Affectation (a,e) ->
      let na = analyse_tds_affectable tds a true in
      let ne = analyse_tds_expression tds e in
      AstTds.Affectation (na,ne)
  | AstSyntax.Constante (n,v) ->
    begin
      match chercherLocalement tds n with
      | None ->
        (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v)));
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        AstTds.Empty
      | Some _ ->
        (* L'identifiant est trouvé dans la tds locale,
        il a donc déjà été déclaré dans le bloc courant *)
        raise (DoubleDeclaration n)
    end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia loia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia loia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.ElseOptionnel (c,t) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia loia t in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.ElseOptionnel (nc, tast)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia loia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
    begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
    end
  | AstSyntax.BoucleInfinieNommee (n,li) ->
      (* Nom remplacé pour respecter la contrainte d'avoir *)
      (* des variables et boucles de même nom *)
      let nr = "loop@"^n in      
      begin
        (* On vérifie si on est déjà dans une boucle nommée *)
        match loia with
        | None ->
            let info = InfoBoucle nr in
            let ia = info_to_info_ast info in
            ajouter tds nr ia;

            let nli = analyse_tds_bloc tds oia (Some ia) li in

            AstTds.BoucleInfinieNommee (ia,nli)
        | Some ia ->
            (* On vérifie si les 2 boucles imbriquées n'ont pas le même identifiant *)
            begin
              match info_ast_to_info ia with
              | InfoBoucle(ni) ->
                  if (nr = ni) then raise (DoubleDeclarationBoucle n)
                  else
                    let info = InfoBoucle nr in
                    let ia = info_to_info_ast info in
                    ajouter tds nr ia;

                    let nli = analyse_tds_bloc tds oia (Some ia) li in

                    AstTds.BoucleInfinieNommee (ia,nli)
              | _ -> failwith "erreur interne"
            end
      end
  | AstSyntax.BoucleInfinie (li) ->
      (* On analyse juste les instructions dans la boucle *)
      AstTds.BoucleInfinie (analyse_tds_bloc tds oia None li)
  | AstSyntax.BreakNommee (n) -> 
      (* On analyse si ce break nommé est associé à une boucle nommée *)
      let nr = "loop@"^n in
      begin  
        match chercherGlobalement tds nr with
        | None -> raise (BoucleNommeeNonDeclare n)
        | Some ia -> AstTds.BreakNommee(ia)
      end
  | AstSyntax.Break -> AstTds.Break
  | AstSyntax.ContinueNommee (n) ->
      (* On analyse si ce continue nommé est associé à une boucle nommée *)
      let nr = "loop@"^n in
      begin  
        match chercherGlobalement tds nr with
        | None -> raise (BoucleNommeeNonDeclare n)
        | Some ia -> AstTds.ContinueNommee(ia)
      end
  | AstSyntax.Continue -> AstTds.Continue


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds  : la table des symboles courante *)
(* Paramètre oia  : None si le bloc li est dans le programme principal,
                    Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre loia : None si le bloc li n'est pas dans une boucle,
                    Some ia où ia est l'information associée à la boucle dans laquelle est le bloc li sinon *)
(* Paramètre li   : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia loia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
  let nli = List.map (analyse_tds_instruction tdsbloc oia loia) li in
  (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
  nli


(* ajouter_pointeur : tds -> string -> Type.typ -> Type.typ * info_ast *)
(* Paramètre tds  : la table des symboles courante *)
(* Paramètre n    : le nom de l'identifiant *)
(* Paramètre t    : le type de la fonction *)
(* Ajout de chaque paramètre de la fonction dans la TDS fille *)
(* Erreur si mauvaise utilisation des identifiants *)
let ajouter_pointeur tds n t =
  match chercherLocalement tds n with
  | None ->
      (* L'identifiant n'est pas trouvé dans la tds locale,
      il n'a donc pas été déclaré dans le bloc courant *)
      (* Création de l'information associée à l'identifiant *)
      let info = InfoVar (n,t,0,"") in
      (* Création du pointeur sur l'information *)
      let ia = info_to_info_ast info in
      (* Ajout de l'information (pointeur) dans la tds *)
      let _ = ajouter tds n ia in
      
      (t,ia);
  | Some _ ->
      (* L'identifiant est trouvé dans la tds locale,
      il a donc déjà été déclaré dans le bloc courant *)
      raise (DoubleDeclaration n)


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li)) =
  (* Creer TDS de la fonction *)
  let tds_fun = creerTDSFille maintds in
  
  (* Analyser nom de la fonction *)
  match chercherGlobalement tds_fun n with
  | Some _ -> raise (DoubleDeclaration n)
  | None ->
        (* Récuperer la liste des types des parametres de la fonction *)
        let (lt, _) = List.split lp in

        (* Génerer une nouvelle information associée à la fonction *)
        let f = InfoFun (n,t,lt) in
        let ia = info_to_info_ast f in
        ajouter maintds n ia;

  (* Analyser identifiant des types de la liste des parametres de la function *)    
  (* Creer pointeurs pour chaque parametres de la fonction *)
  (* Ajouter les pointeurs dans la tds Fille *)
  let nlp = List.map (fun (t,n) -> ajouter_pointeur tds_fun n t) lp in

  (* Analyser les identifiants dans le bloc *)
  let lie = analyse_tds_bloc tds_fun (Some ia) None li in
  
  (* On renvoie la fonction de type AstTds.Fonction *)
  AstTds.Fonction(t, ia, nlp, lie)


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None None prog in
  AstTds.Programme (nf,nb)
