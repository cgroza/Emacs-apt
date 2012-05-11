VERSION=0.1
EMACS=emacs
PREFIX=/usr/local
ELS=emacs-apt.el apt-mode.el
ELCS=$(ELS:.el=.elc)
DIST_FILES=$(ELS) Makefile README.md

.PHONY=install

BATCH=$(EMACS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: $(ELCS) 

install:
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -m 644 $(ELS) $(ELCS) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp

