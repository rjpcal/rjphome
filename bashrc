#!/bin/bash -*- shell-script -*-

# Rob Peters <rjpeters@klab.caltech.edu>

# $Header$

umask 077

if test -x /bin/arch; then
    export ARCH=`/bin/arch`
elif test `uname -m` = "Power Macintosh"; then
    export ARCH="ppc"
fi

export MANPATH=${HOME}/local/${ARCH}/man
export MANPATH=${MANPATH}:/usr/share/man:/usr/man
export MANPATH=${MANPATH}:/usr/local/share/man:/usr/local/man

export PATH=.:${HOME}/local/bin:${HOME}/local/${ARCH}/bin:/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin

export LD_LIBRARY_PATH=${HOME}/local/${ARCH}/lib

case $ARCH in
    hp9000s700)
        export PATH=${PATH}:/opt/aCC/bin:/opt/langtools/bin:/opt/imake/bin:/usr/ucb:/usr/ccs/bin
        export MANPATH=${MANPATH}:/opt/langtools/share/man:/opt/audio/share/man
        breaksw
	;;
    ppc)
	# Initialize Fink if we're on ppc and it's available
	if test -r /sw/bin/init.sh; then
	    . /sw/bin/init.sh
	fi

	# Darwin for some reason has a limited stacksize by default
	ulimit -s unlimited #stacksize
	ulimit -d unlimited #datasize
	;;
esac

# for interactive shells:
if test "$PS1" != ""; then

    set matchbeep nomatch # only beep for missing, but not for ambiguous, matches
    FIGNORE="~"  # filename suffixes to be ignored by completion
    HISTSIZE=500 # number of commands to store in history
    HISTFILE=""  # don't save/load command history from a file

    #"\e[" is the start of the escape sequence
    #"1" means bold
    #"m" is end of escape sequence.

    # Attribute codes:
    # 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
    # Text color codes:
    # 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
    # Background color codes:
    # 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white

    # [hostname HH:MM]$

    PS1="\[\e[40;33;1m\][\h \A \!]$\[\e[0m\] "

    case $HOST in
	curie*)
	    alias ls='ls -F'
	    ;;
	*)
	    alias ls='ls -F --color=tty'
	    ;;
    esac

    alias pwd='dirs -l'

    # to keep case-sensitive sorting for 'ls', for example
    export LC_COLLATE=C
fi

export GRSH_LIB_DIR=${HOME}/projects/grsh/share

export MAILTO=rjpeters@klab.caltech.edu

export PVM_ROOT=/usr/share/pvm3

export CVS_RSH=ssh

### Source a system-local init file, if it exists
if test -r ~/.bashrc_local; then
    . ~/.cshrc_local
fi