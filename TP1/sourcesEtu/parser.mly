%{
    type interface = Auto of string | Iface of string
%}


%token <string> ID
%token AUTO
%token IFACE
%token INET
%token DHCP
%token STATIC
%token LOOPBACK
%token ADDRESS
%token GATEWAY
%token NETMASK
%token IP
%token EOF


(* Exercice 2 *)
(* Déclarations du type de l'attribut associé à un non terminal *)
(* Dans un premier temps on ignore cet attribut -> type unit *)
%type <string> i
%type <unit> t

(* Indication de l'axiom et du type de l'attribut associé à l'axiom *)
(* Dans un premier temps on ignore cet attribut -> type unit *)
%start <string list * string list> is

%%

(*
IS -> I IS
IS -> $

I -> ...
*)

is :
| ni=i lis=is {
    match ni with
    | 
    ni::lis
    
    } (* action sémantique associée à une règle de prodution -> dans un premier temps () *)
| EOF  {[]}

i : 
| AUTO n=ID { n }
| IFACE n=ID INET t { n }

t :
| LOOPBACK {()}
| DHCP {()}
| STATIC ADDRESS IP NETMASK IP GATEWAY IP {()}


(* {()} : permet de reconstruire l'ATS avant de faire la gestion des identifiants avec la TDS *)
(* {()} : utile pour faire remonter des choses *)

%%
