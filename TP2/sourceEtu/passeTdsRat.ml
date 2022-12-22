(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = (* (AstTds.Booleen true) ===> failwith "todo"*)
    match e with
    | AstSyntax.Ident ident ->
        begin
          match chercherGlobalement tds ident with
          | None -> raise (IdentifiantNonDeclare ident)
          | Some a -> 
            begin
              match info_ast_to_info a with
              | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant ident)
              | InfoConst(_, i) -> AstTds.Entier(i) (*rajouté*)
              | _ -> AstTds.Ident a
            end
        end
    | AstSyntax.Entier i -> AstTds.Entier i
    | AstSyntax.Booleen b -> AstTds.Booleen b
    | AstSyntax.Binaire (op, exp1, exp2) -> 
        AstTds.Binaire (op, analyse_tds_expression tds exp1, analyse_tds_expression tds exp2)
    | AstSyntax.Unaire (unaire, expression) -> 
        AstTds.Unaire (unaire, analyse_tds_expression tds expression)
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
    | _ -> failwith "autres"
      

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
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
  | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds globale. *)
          raise (IdentifiantNonDeclare n)
        | Some info ->
          (* L'identifiant est trouvé dans la tds globale,
          il a donc déjà été déclaré. L'information associée est récupérée. *)
          begin
            match info_ast_to_info info with
            | InfoVar _ ->
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *)
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
                 et l'expression remplacée par l'expression issue de l'analyse *)
              AstTds.Affectation (info, ne)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)
              raise (MauvaiseUtilisationIdentifiant n)
          end
      end
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
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
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
  | _ -> failwith "autres"


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli

(* *)
let ajouter_pointeur tds n t =
  match chercherLocalement tds n with
  | None ->
      (* L'identifiant n'est pas trouvé dans la tds locale,
      il n'a donc pas été déclaré dans le bloc courant *)
      (* Création de l'information associée à l'identfiant *)
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
  let lie = analyse_tds_bloc tds_fun (Some ia) li in
  
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
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb)
