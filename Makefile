# Makefile for basic home directory ~/.*rc files, etc.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

TARGETS := \
$(HOME)/.bashrc \
$(HOME)/.cshrc \
$(HOME)/.emacs \
$(HOME)/.login \
$(HOME)/.Xdefaults \
$(HOME)/.Xmodmap \
$(HOME)/.Xresources \

default: $(TARGETS)

$(HOME)/.%:
	ln -s `pwd`/$* $@

$(HOME)/.Xresources:
	ln -s `pwd`/Xdefaults $@
