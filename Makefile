EMACS = emacs

.PHONY: all
all: init.elc

init.elc: init.el
	$(EMACS) -batch -l init.el -f batch-byte-compile $<

.PHONY: clean
clean:
	rm *.elc

.PHONY: distclean
distclean:
	$(MAKE) clean
	rm -fr elpa
	echo "" > custom.el
