#!/bin/bash -*- shell-script -*-

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

# This doesn't exist universally
if test -x /usr/games/bin/fortune; then
    /usr/games/bin/fortune -a;

elif test -x /usr/games/fortune; then
    /usr/games/fortune
fi

if test -f ~/.bashrc; then
    . ~/.bashrc
fi
