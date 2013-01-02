all:
	@echo !!! This Makefile is for development purposes only. !!!
	@echo !!! Users should instead use Setup.hs or cabal. !!!
	@exit 1

WARNING_FLAGS=-W -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-unused-do-bind -fno-warn-missing-signatures
GHC=ghc $(WARNING_FLAGS) -rtsopts 

std:
	$(GHC) --make PdfViewer.hs

# Two stage profile build, due to template haskell not playing nice with profiling.
# See: http://www.haskell.org/ghc/docs/6.12.1/html/users_guide/template-haskell.html#id3029367
prof: std
	$(GHC) --make PdfViewer.hs -prof -osuf p_o

clean:
	rm -f PdfViewer PdfViewer.hi PdfViewer.hp PdfViewer.o PdfViewer.p_o