# Makefile for basic home directory ~/.*rc files, etc.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

TARGETS := \
$(HOME)/.cshrc \
$(HOME)/.emacs \
$(HOME)/.gnome_init \
$(HOME)/.login \
$(HOME)/.Xdefaults \

default: $(TARGETS)

$(HOME)/.%:
	ln -s `pwd`/$* $@
