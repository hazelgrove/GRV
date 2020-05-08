DOC_DIR = ../../`cd ../.. && make echo-doc`

DOTS := $(wildcard ./*.dot)
SVGS := $(patsubst ./%.dot,./%.svg,$(DOTS))

all: $(SVGS)
	@touch done

./%.svg: ./%.dot
	dot -Tsvg $< -o $@

clean:
	@rm -i $(SVGS)
	@rm done

.PHONY : all clean
