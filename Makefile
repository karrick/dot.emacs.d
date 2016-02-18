MARKDOWN = pandoc --from markdown --to html5 --standalone --highlight-style=zenburn

SOURCE = $(shell find . -name \*.md)
TARGET = $(patsubst %.md,%.html,$(SOURCE))

EL = $(shell find . -name \*.el)
ELC = $(patsubst %.el,%.elc,$(EL))

packages:
	emacs --script install-packages.el

compile:
	cd lisp && emacs --batch --eval "(progn (add-to-list 'load-path \".\") (byte-recompile-directory \".\" 0))"

markdown: $(TARGET)

$(TARGET): Makefile

clean:
	rm -f $(TARGET)
	rm -f *.bak *~
	rm -rf elpa

%.html: %.md
	$(MARKDOWN) --output $@ $<

.PHONY: clean markdown packages
