# Makefile for basic home directory ~/.*rc files, etc.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

default: $(HOME)/.cshrc

$(HOME)/.cshrc:
	ln -s `pwd`/cshrc $@
