EMACS = emacs

.PHONY: all
all: init.elc custom.elc minit.elc

init.elc: init.el
	$(EMACS) -batch -l init.el -f batch-byte-compile $<

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

.PHONY: clean
clean:
	rm *.elc

.PHONY: distclean
distclean:
	$(MAKE) clean
	rm -fr elpa
	echo "" > custom.el
