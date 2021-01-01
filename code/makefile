all : exec

exec : main.o Fonctions.ml Structures.ml ABR_utils.ml ABR_compresse_utils.ml ABRCompression.ml
	make main.o && ./main.o

main.o : Fonctions.ml Structures.ml ABR_utils.ml ABR_compresse_utils.ml ABRCompression.ml
	ocamlc unix.cma str.cma -o main.o Structures.ml ABR_utils.ml Fonctions.ml ABR_compresse_utils.ml ABRCompression.ml

clear :
	rm -rfv *.cmi *.cmo *.o


