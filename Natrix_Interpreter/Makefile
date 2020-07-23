CMO=lexer.cmo parser.cmo interp.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
FLAGS=-annot -g

all: natrix
	./natrix test.nx
	
foreach_if: natrix
	./natrix foreach_if.nx
	
fatorial: natrix
	./natrix fatorial.nx
	
maior_elemento: natrix
	./natrix maior_elem.nx
	
menor_elemento: natrix
	./natrix menor_elem.nx
	
size: natrix
	./natrix size.nx
	
div_zero: natrix
	./natrix div_zero.nx

neg_interval: natrix
	./natrix neg_interval.nx
	
unbound_var: natrix
	./natrix unbd.nx
	
relatorio: natrix
	./natrix relatorio.nx
	
free: natrix
	./natrix free.nx
	
sintaxe: natrix
	./natrix syntax_error.nx
	
lexico: natrix
	./natrix lexical_error.nx
	
tipo: natrix
	./natrix type_error.nx

interval_decr: natrix
	./natrix int_decreasing_order.nx

natrix: $(CMO)
	ocamlc $(FLAGS) -o $@ nums.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	menhir -v $<

clean:
	rm -f *.cm[io] *.o *.annot *~ natrix $(GENERATED)
	rm -f parser.output parser.automaton

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



