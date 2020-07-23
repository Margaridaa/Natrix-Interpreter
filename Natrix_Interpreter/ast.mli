
(* árvore de sintaxe abstracta para Natrix *)

type ident = string

type unop =
  | Uneg | Unot

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq | Bneq | Blt | Ble | Bgt | Bge (* comparação estrutural *)
   (* equal, not equal, less than, less or equal, greater than, greater or equal *)
  | Band | Bor

type constant =
  | Cstring of string
  | Cint of int

type expr =
  | Ecst of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecall of ident * expr list
  | Elist of expr * expr
  | Eget of expr * expr (* e1[e2] *)
  | Edotdot of expr * expr
  | Emin
  | Emax
  | Eletin of ident * expr * expr

and stmt =
  | Sif of expr * stmt * stmt
  | Sassign of ident * expr	(* x := 1 *)
  | Sassign2 of ident * expr * expr	(* x := x+1 *)
  | Sprint of expr
  | Sblock of stmt list
  | Sforeach of ident * expr * stmt
  | Seval of expr
  | Stype of ident * expr (* type t = [0 .. 5]; *)
  | Stype2 of ident * ident (* type arr = array t of int; *)
  | Svar of ident * expr	(* var x : int = 1; *)
  | Svar2 of ident * ident * expr (* var tab1 : a filled by 0; *)
  | Svar3 of ident* expr * expr (* var tab : array 10 of int filled by 1; *)

and def = ident * ident list * stmt

and file = def list * stmt

