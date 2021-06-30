compile:
	emacs --batch --eval "(progn (add-to-list 'load-path \".\") (byte-recompile-directory \".\" 0))"

clean:
	rm -f *.elc
	rm -f *.bak *~

.PHONY: compile clean
