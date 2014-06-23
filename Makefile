main: main.hs lisp.hs
	ghc --make $< -Wall
%: %.hs
	ghc --make $< # -Wall
