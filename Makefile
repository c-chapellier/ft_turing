NAME = ft_turing
 
all:
		dune build

run:	all
		dune exec ./ft_turing.exe

install:
		brew install opam
		opam init
		eval `opam env`
		opam switch create 4.11.1
		eval `opam env`
 
clean:

# dune init exe ft_turing


# ocamlopt and ocamlc
