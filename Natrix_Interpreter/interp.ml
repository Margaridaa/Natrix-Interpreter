open Ast
open Format

(* Exceção levantada para assinalar um erro durante a interpretação *)
exception Error of string
let error s = raise (Error s)

type value =
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Visualização *)
let rec print_value = function
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"
	
(* Interpretação booleana *)

let is_false = function
  | Vnone
  | Vbool false
  | Vstring ""
  | Vlist [||] -> true
  | Vint n -> n = 0
  | _ -> false

let is_true v = not (is_false v)

let getInt= function
  | Vint n -> n 
  | _ -> error "nao se aplica"
  
let getList = function
	| Vlist l -> l
  | _ -> error "nao se aplica"
	
(* Comparações *)

let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else let c = compare_value a1.(i) a2.(i) in
       if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

and compare_value v1 v2 = match v1, v2 with
  | Vlist a1, Vlist a2 ->
    compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | Vbool b1, Vint _ -> compare_value (Vint (if b1 then 1 else 0)) v2
  | Vint _, Vbool b2 -> compare_value v1 (Vint (if b2 then 1 else 0))
  | _ -> compare v1 v2

(*	
		A função dotdot é usada sempre que 
			uma expressão se avalia como Vlist,
			como no caso:
				type t = [0 .. 10];
*)

let dotdot v1 v2 = match v1, v2 with
	| Vint _, Vint _ ->
			let lista = ref [||] in
			let n1 = (getInt v1) in
			let n2 = (getInt v2) in
			
			if (n1>=0 && n2>=0) then begin
			
				if (n1<n2) then begin
					for i=n1 to n2 do
						lista := Array.append !lista [|(Vint i)|]
					done;
					Vlist (!lista)
				end
				
				else error "an interval should have increasing order."
				
			end
			else error "unsupported types in interval definition."
	| _, _ -> error "unsupported types in interval definition"
	
(* Interpretação dos operadores binários

   - os operadores / e % devem levantar uma excepção se se tentar dividir
     por zero
*)

let binop op v1 v2 = match op, v1, v2 with
  | Badd, Vint n1, Vint n2 -> Vint (n1+n2)
  | Bsub, Vint n1, Vint n2 -> Vint (n1-n2)
  | Bmul, Vint n1, Vint n2 -> Vint (n1*n2)
  | (Bdiv | Bmod), Vint _, Vint 0 -> error "division by zero"
  | Bdiv, Vint n1, Vint n2 -> Vint (n1/n2)
  | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
  | Beq, _, _ -> Vbool (compare_value v1 v2 = 0)
  | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
  | Blt, _, _ -> Vbool (compare_value v1 v2 < 0)
  | Ble, _, _ -> Vbool (compare_value v1 v2 <= 0)
  | Bgt, _, _ -> Vbool (compare_value v1 v2 > 0)
  | Bge, _, _ -> Vbool (compare_value v1 v2 >= 0)
  | _ -> error "unsupported operand types"


(* As funções são aqui exclusivamente globais *)

let functions = (Hashtbl.create 17 : (string, ident list * stmt) Hashtbl.t)

(* variáveis locais
   (parâmetros de funções e variáveis introduzidas por atribuições) são
   arquivadas numa tabela de hash passada como argumento às funções
   seguintes com o nome 'ctx'
*)

type ctx = (string, value) Hashtbl.t

(* Interpretação de uma expressão (devolve um valor) *)

let rec expr ctx = function
  | Ecst (Cint n) ->
      Vint n
  | Ecst (Cstring s) ->
  		Vstring s
  | Emax -> Vint 99999
  | Emin -> Vint (-99999)
  | Edotdot (e1,  e2) ->
  	let v1 = expr ctx e1 in
  	let v2 = expr ctx e2 in
  	dotdot v1 v2
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then expr ctx e2 else v1
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then expr ctx e2 else v1
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      binop op (expr ctx e1) (expr ctx e2)
  | Eunop (Uneg, e1) ->
      begin match expr ctx e1 with
        | Vint n -> Vint (-n)
        | _ -> error "unsupported operand types" end
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  | Ecall ("size", [e1]) ->
  		let e = expr ctx e1 in
      begin match e with
        | Vstring s -> Vint (String.length s)
        | Vlist l -> Vint (Array.length l)
        | _ -> error (" this value has no 'size'") end
  | Elist (e1, e2)->
  	let v1 = expr ctx e1 in
  	let v2 = expr ctx e2 in
  	dotdot v1 v2
  | Eident id ->
      if not (Hashtbl.mem ctx id) then error ("unbound variable '"^id^"'");
      Hashtbl.find ctx id
   | Eletin (id,e1,e2) ->	(* var x := let y = x+1 in y*2; *)
      Hashtbl.replace ctx id (expr ctx e1) ;
			let v1 = expr ctx e1 in
			let v2 = expr ctx e2 in
			begin match v1, v2 with
			| Vint vv1, Vint vv2 ->  Vint vv2
			| _ ->  error "let in error" end
  | Eget (e1, e2) ->
      begin match expr ctx e1 with
      | Vlist l ->
          let i = expr_int ctx e2 in
          (try l.(i) with Invalid_argument _ -> error "index out of bounds")
      | _ -> error "list expected" end

(* interpretação de um valor e verificação de que se trata de um inteiro *)

and expr_int ctx e = match expr ctx e with
  | Vint n -> n
  | _ -> error "integer expected"

(* interpretação de uma instrução - não devolve nada *)

and stmt ctx = function
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
	| Svar (id, e1) ->
      Hashtbl.replace ctx id (expr ctx e1)
	| Svar2 (id, a, e1) ->
  		if not (Hashtbl.mem ctx a) then error ("unbound variable '"^id^"'");
  		let temp = getList (Hashtbl.find ctx a) in
  		let b = Vlist (Array.make (Array.length temp) (expr ctx e1)) in
      Hashtbl.replace ctx id (b)
  | Svar3 (id, e1, e2) ->
  		let n1 = (expr ctx e1) in
  		let n2 = (expr ctx e2) in
  		let tab = Vlist (Array.make (getInt n1) (n2)) in
     		Hashtbl.replace ctx id tab
  | Stype (id, e1) ->
  		Hashtbl.replace ctx id (expr ctx e1)
  | Stype2 (id, t) ->
  		if not (Hashtbl.mem ctx t) then error ("unbound variable '"^id^"'");
      Hashtbl.replace ctx id (Hashtbl.find ctx t)
  | Sassign (id, e1) ->
      Hashtbl.replace ctx id (expr ctx e1)
  | Sassign2 (id, e1, e2) ->
  		if not (Hashtbl.mem ctx id) then error ("unbound variable '"^id^"'");
  		let n1 = getInt (expr ctx e1) in
  		let n2 = expr ctx e2 in
  		begin
  		match n2 with
  		| Vint _ ->
				let a = getList (Hashtbl.find ctx id) in
				Array.set a n1 n2;
		    	Hashtbl.replace ctx id (Vlist a)
		  | _ -> error "the right side must be an integer."
		  end
  | Sprint e ->
      print_value (expr ctx e); printf ""
  | Sblock bl ->
      block ctx bl
  | Sforeach (x, e, s) ->
      begin match expr ctx e with
      | Vlist l ->
        Array.iter (fun v -> Hashtbl.replace ctx x v; stmt ctx s) l
      | _ -> error "list expected" end  
  | Seval e ->
      ignore (expr ctx e)

(* interpretação de um bloco, i.e. uma sequência de instruções *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* interpretação de um ficheiro *)

let file (fl, s) =
  List.iter
    (fun (f,args,body) -> Hashtbl.add functions f (args, body)) fl;
  stmt (Hashtbl.create 17) s
