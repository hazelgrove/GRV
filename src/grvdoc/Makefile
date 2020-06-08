DOC_DIR = ../../`cd ../.. && make echo-doc`

DOTS := $(wildcard ./*.dot)
SVGS := $(patsubst ./%.dot,./%.svg,$(DOTS))
PDFS := $(patsubst ./%.dot,./%-dot.pdf,$(DOTS)) master.pdf

all: $(SVGS) $(PDFS)
	@touch done

./%.svg: ./%.dot
	dot -Tsvg $< -o $@

./%-dot.pdf: ./%.dot
	dot -Tpdf $< -o $@

master.pdf: master.tex
	latexmk -pdf master.tex

clean:
	@rm $(SVGS)
	@rm $(PDFS)
	@latexmk -C
	@rm done

.PHONY : all clean
