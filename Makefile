# Makefile for basic home directory ~/.*rc files, etc.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

TARGETS := \
$(HOME)/.aspell.en.pws \
$(HOME)/.bashrc \
$(HOME)/.cshrc \
$(HOME)/.elisp \
$(HOME)/.emacs \
$(HOME)/.git-prompt.sh \
$(HOME)/.gitconfig \
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

CYGWIN_TARGETS := \
$(HOME)/_emacs \

default: $(TARGETS) worldread

darwin: $(DARWIN_TARGETS)

cygwin: $(CYGWIN_TARGETS)

worldread:
	@for f in plan; do \
		install \
			-m 0444 \
			-p \
			-v \
			$$f $(HOME)/.$$f; \
	done


$(HOME)/.%:
	ln -s `pwd`/$* $@

$(HOME)/.Xresources:
	ln -s `pwd`/Xdefaults $@

$(HOME)/_emacs: ./emacs
	cp $< $@
