DOC_DIR = ../../`cd ../.. && make echo-doc`

DOTS := $(wildcard ./*.dot)
SVGS := $(patsubst ./%.dot,./%.svg,$(DOTS))

TEXS := master.tex
PDFS := master.pdf

all: $(SVGS) $(PDFS)
	@touch done

./%.svg: ./%.dot
	dot -Tsvg $< -o $@

master.pdf: master.tex
	latexmk -pdf master.tex

clean:
	@latexmk -C
	@rm $(SVGS)
	@rm done

.PHONY : all clean
