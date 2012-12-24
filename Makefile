all:
	ghc -Wall -fno-warn-missing-signatures -rtsopts --make PdfViewer.hs

# Two stage profile build, due to template haskell not playing nice with profiling.
# See: http://www.haskell.org/ghc/docs/6.12.1/html/users_guide/template-haskell.html#id3029367
prof: all
	ghc --make PdfViewer.hs -rtsopts -prof -osuf p_o