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
$(HOME)/.Xdefaults \
$(HOME)/.Xmodmap \
$(HOME)/.Xresources \

DARWIN_TARGETS := \
$(HOME)/.xinitrc \

default: $(TARGETS)

darwin: $(DARWIN_TARGETS)

$(HOME)/.%:
	ln -s `pwd`/$* $@

$(HOME)/.Xresources:
	ln -s `pwd`/Xdefaults $@
