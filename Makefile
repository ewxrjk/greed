# Makefile for GREED

# ELISPDIR is the directory for local Emacs LISP programs 
#   (add it to `load-path' if necessary).
# INFODIR is the directory for Info files
#   (add it to `Info-default-directory-list' if necessary).
# DOT_EMACS is the user's .emacs file
# If UPDATE_DOT_EMACS is "true", then the user's .emacs file
#   will be updated with LISP code to set up `load-path' and
#   `Info-default-directory-list' appropriately.
# EMACS_PROGRAM is the Emacs program

ELISPDIR = $(HOME)/elisp/
INFODIR = $(HOME)/info/
DOT_EMACS = $(HOME)/.emacs
UPDATE_DOT_EMACS = false
EMACS_PROGRAM = xemacs

# You shouldn't need to change anything below here.

all: greed.info greed-md5.elc greed.elc greed-help-macro.elc

greed.elc: greed.el
	$(EMACS_PROGRAM) -batch -l greed-make.el -f batch-byte-compile greed.el

greed-md5.elc: greed-md5.el
	$(EMACS_PROGRAM) -batch -l greed-make.el -f batch-byte-compile greed-md5.el

greed-help-macro.elc: greed-help-macro.el
	$(EMACS_PROGRAM) -batch -l greed-make.el -f batch-byte-compile greed-help-macro.el

greed.info: greed.texi
	$(EMACS_PROGRAM) -batch -l texinfmt -f batch-texinfo-format greed.texi

greed.dvi: greed.texi
	texi2dvi greed.texi

clean:
	rm greed.elc greed-md5.elc greed-help-macro.elc greed.info

dist:
	echo greed-`sed -e '/defconst greed-version/!d' \
	  -e 's/[^0-9]*\([0-9.]*\).*/\1/' -e q greed.el` > version
	if test ! -d `cat version` ; then mkdir `cat version`; fi
	cp Makefile greed-make.el greed.el greed-md5.el greed-help-macro.el greed.texi CHANGELOG README `cat version`
	tar cf `cat version`.tar `cat version`
	gzip `cat version`.tar
	rm -r `cat version` version

dvi: greed.dvi

info: greed.info

install: all
	if test ! -d $(ELISPDIR); then mkdir $(ELISPDIR); fi
	if test ! -d $(INFODIR); then mkdir $(INFODIR); fi
	cp greed.elc greed-md5.elc greed-help-macro.elc $(ELISPDIR)
	cp greed.info $(INFODIR)
	if test "$(UPDATE_DOT_EMACS)" = "true"; then \
	  echo "(autoload 'greed \"greed\" \"Read Groggs\" t)" >> $(DOT_EMACS); \
	  echo "(setq load-path (cons \"$(ELISPDIR)\" load-path))" >> $(DOT_EMACS); \
	  echo "(setq Info-default-directory-list (cons \"$(INFODIR)\" Info-default-directory-list))" >> $(DOT_EMACS); \
	fi

uninstall:
	rm $(ELISPDIR)/greed.elc $(ELISPDIR)/greed-md5.elc $(ELISPDIR)/greed-help-macro.elc $(INFODIR)/greed.info
