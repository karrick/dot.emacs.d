MARKDOWN = pandoc --from markdown --to html5 --standalone --highlight-style=zenburn

SOURCE = $(shell find . -name \*.md)
TARGET = $(patsubst %.md,%.html,$(SOURCE))

all: $(TARGET)

$(TARGET): Makefile

clean:
	rm -f $(TARGET)
	rm -f *.bak *~

%.html: %.md
	$(MARKDOWN) --output $@ $<

.PHONY: all clean
