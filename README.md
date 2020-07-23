# Natrix-Interpreter
Interpretador *Natrix*.

## A linguagem *Natrix*

### Código exemplo
```
// intervalo
type i = [10 .. 20];
print("Tamanho do intervalo i = ");
print(size(i));
print("\nTamanho do intervalo [1 .. 9] = ");
print(size([1 .. 9]));

// vetor
type v = array i of int;

// inicializar variavel comum
var x : int = 5;
var count : int = 0;

// inicializar vetores
var tab1 : array 5 of int filled by x;
var tab2 : v filled by 2;
print("\ntab1 = ");
print(tab1);
print("\ntab2 = ");
print(tab2);

// condicao if
if (size(tab2)==size(v)) then { print("\ntab2 tem o mesmo comprimento que v.\n"); } else { print("tab2 e v diferem no seu comprimento.\n"); }

// atribuicao
x := x + 10;
print(x);
tab2[1] := 1;
tab2[6] := 1;
print("\ntab1 = ");
print(tab1);
print("\ntab2 = ");
print(tab2);

// foreach
foreach n in [0 .. 1] do { print("\n"); print("Foreach iteração número "); print(n+1); }
print("\n");
foreach t in tab1 do { print(t*2); print("   "); }
foreach t in tab2 do { if (t!=1) then { count := count + 1; } else { count := count; } }
print("\nNúmero de valores diferentes de '1' = ");
print(count);			// número de posições com valor diferente de 1.
print("\n");

x := let y = 9 + x in y*x;
print("\nResultado do let in = ");
print(x);
print("\n");
```

## Manual de Utilizador

### Dependências
O utilizador deverá ter a linguagem *OCaml* instalada no seu sistema.

O utilizador poderá ter de instalar `menhir`:
`opam install menhir`

O utilizador poderá ter de instalar a biblioteca `num`:
`opam install num`

### Execução

A diretoria `Natrix_Interpreter` fornece programas exemplo, que poderão ser executados recorrendo aos comandos:
```
make relatorio
make maior_elemento
make menor_elemento
make fatorial
make foreach_if
make size
make size_error
make div_zero
make neg_interval
make unbound_var
make interval_decr
make sintaxe
make lexico
make free
```

Cada uma destas variações corresponde à execução de um ficheiro de teste.

O ficheiro `free.nx` poderá ser utilizado pelo utilizador caso pretenda programar na linguagem `Natrix`.
