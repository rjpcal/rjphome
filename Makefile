# Makefile for basic home directory ~/.*rc files, etc.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

TARGETS := \
$(HOME)/.aspell.en.pws \
$(HOME)/.bashrc \
$(HOME)/.cshrc \
$(HOME)/.emacs \
$(HOME)/.galeon \
$(HOME)/.inputrc \
$(HOME)/.login \
$(HOME)/.profile \
$(HOME)/.signature \
$(HOME)/.twmrc \
$(HOME)/.Xdefaults \
$(HOME)/.Xmodmap \
$(HOME)/.Xresources \

DARWIN_TARGETS := \
$(HOME)/.xinitrc \

default: $(TARGETS) worldread

darwin: $(DARWIN_TARGETS)

worldread:
	@for f in plan; do \
		install \
			--mode=0444 \
			--preserve-timestamps \
			--verbose \
			$$f $(HOME)/.$$f; \
	done


$(HOME)/.%:
	ln -s `pwd`/$* $@

$(HOME)/.Xresources:
	ln -s `pwd`/Xdefaults $@
