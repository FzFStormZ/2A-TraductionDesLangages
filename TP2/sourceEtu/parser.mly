/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token EOF
(* token pour les pointeurs *)
%token NEW
%token NULL
%token ESP
(* token pour la condi sous forme ternaire *)
%token PI
%token DP
(* toekn pour les boucles "loop" à la rust *)
%token LOOP
%token BREAK
%token CONTINUE

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction> i
%type <typ> typ
%type <typ*string> param
%type <expression> e
(* non-terminal pour les pointeurs *)
%type <affectable> a

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi=prog EOF     {lfi}

prog : lf=fonc* ID li=bloc  {Programme (lf,li)}

fonc : t=typ n=ID PO lp=param* PF li=bloc {Fonction(t,n,lp,li)}

param : t=typ n=ID  {(t,n)}

bloc : AO li=i* AF      {li}


a :
| n=ID              {Ident n} (* A -> id *)
| PO MULT af=a PF   {Dref af} (* A -> ( *A ) *)

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| a1=a EQUAL e1=e PV                {Affectation (a1,e1)} (* I -> A = E *)
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| IF exp=e li=bloc                  {ElseOptionnel(exp,li)} (* I -> if E BLOC *)
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| n=ID DP LOOP li=bloc              {BoucleInfinieNommee(n,li)} (* I -> id : loop BLOC *)
| LOOP li=bloc                      {BoucleInfinie(li)} (* I -> loop BLOC *)
| BREAK n=ID PV                     {BreakNommee(n)} (* I -> break id; *)
| BREAK PV                          {Break} (* I -> break ; *)
| CONTINUE n=ID PV                  {ContinueNommee(n)} (* I -> continue id; *)
| CONTINUE PV                       {Continue} (* I -> continue ; *)

typ :
| BOOL          {Bool}
| INT           {Int}
| RAT           {Rat}
| t=typ MULT    {Pointeur t} (* TYPE -> TYPE* *)
| PO t=typ PF   {t}

e : 
| CALL n=ID PO lp=e* PF         {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF         {Binaire(Fraction,e1,e2)}
| af=a                          {Affectable af} (* E -> A *)
| NULL                          {Null} (* E -> null *)
| PO NEW t=typ PF               {New t} (* E -> (new TYPE) *)
| ESP n=ID                      {Adress n} (* E -> &id *)
| TRUE                          {Booleen true}
| FALSE                         {Booleen false}
| e=ENTIER                      {Entier e}
| NUM e1=e                      {Unaire(Numerateur,e1)}
| DENOM e1=e                    {Unaire(Denominateur,e1)}
| PO e1=e PLUS e2=e PF          {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF          {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF         {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF           {Binaire (Inf,e1,e2)}
| PO e1=e PI e2=e DP e3=e PF    {Ternaire(e1,e2,e3)} (* E -> ( E ? E : E ) *)
| PO exp=e PF                   {exp}
