DOCDIR   = docs
BINARIES = HRubik
OBJECTS  = *.o *.hi
SOURCES  = HRubik.hs
HSCOLOUROPT = -lit -html -anchor
HSCOLOUR = ~/.cabal/bin/HsColour
PDF = *.pdf *.log *.aux *.out
LIBRARIES = RubikCube.hs GI.hs Display.hs Bindings.hs ReadImage.hs Sprites.hs

all:	$(BINARIES)

$(BINARIES): $(SOURCES) $(LIBRARIES)
	ghc --make $(SOURCES)

clean:
	rm -rf $(OBJECTS) $(BINARIES) $(DOCDIR) $(PDF)

haddock: $(SOURCES)
	mkdir -p $(DOCDIR)/src
	if test -f $(HSCOLOUR); \
	then for file in $(SOURCES); \
			  do $(HSCOLOUR) $(HSCOLOUROPT) $$file \
			        >$(DOCDIR)/src/`basename $$file .hs`.html;\
			  done; \
		haddock --html --title=$(LIBRARY) --odir=$(DOCDIR) \
			  --source-module="./src/$(SOURCES).html" \
			  --source-entity="./src/$(SOURCES).html#%{NAME}" \
			  $(SOURCES); \
	else haddock --html --title=$(LIBRARY) --odir=$(DOCDIR) $(SOURCES); \
	fi

pdf: $(SOURCES)
	pdflatex $(SOURCES)
