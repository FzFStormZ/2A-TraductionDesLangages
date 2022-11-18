{
(*type token = ID of string | AUTO | IFACE | INET 
                | DHCP | STATIC | LOOPBACK | ADDRESS | GATEWAY | NETMASK | IP | EOF (* A COMPLETER *)
*)
open Parser
exception Error of string
}

(* Définitions de macro pour les expressions régulières *)
let blanc = [' ' '\t' '\n']
let id = ['a'-'z']+['0'-'9']*
let nb = ("25"['0'-'5']) | ("2"['0'-'4']['0'-'5']) | (['0'-'1']['0'-'9']['0'-'9']) 
            | (['0'-'9']['0'-'9']) | (['0'-'9'])
let ip = nb '.' nb '.' nb '.' nb


 (* À compléter éventuellement *)


(* Règles léxicales *)
rule interface = parse
|  blanc (* On ignore les blancs *)
    { interface lexbuf }
| "auto"
    { AUTO }
| "iface"
    { IFACE }
| "inet"
    { INET }
| "dhcp"
    { DHCP }
| "static"
    { STATIC }
| "loopback"
    { LOOPBACK }
| "address" 
    { ADDRESS }
| "gateway"
    { GATEWAY }
| "netmask"
    { NETMASK }
| id as chaine (* L'expre régu est résolue et stockée dans "chaine" *)
    { ID chaine }
| ip
    { IP }
| eof
    { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
