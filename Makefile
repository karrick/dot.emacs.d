MARKDOWN = pandoc --from markdown --to html5 --standalone --highlight-style=zenburn

SOURCE = $(shell find . -name \*.md)
TARGET = $(patsubst %.md,%.html,$(SOURCE))

packages:
	emacs --script install-packages.el

markdown: $(TARGET)

$(TARGET): Makefile

clean:
	rm -f $(TARGET)
	rm -f *.bak *~
	rm -rf elpa

%.html: %.md
	$(MARKDOWN) --output $@ $<

.PHONY: clean markdown packages
