#!/bin/bash -*- shell-script -*-

# Rob Peters <rjpeters@klab.caltech.edu>

# $Header$

umask 077

if test -x /bin/arch; then
    export ARCH=`/bin/arch`
elif test -x /usr/bin/arch; then
    export ARCH=`/usr/bin/arch`
elif test "`uname -m`" = "Power Macintosh"; then
    export ARCH="ppc"
else
    echo "Warning: unknown architecture, setting ARCH to 'unknown'"
    export ARCH="unknown"
fi

export MANPATH=${HOME}/local/${ARCH}/man
export MANPATH=${MANPATH}:/usr/share/man:/usr/man
export MANPATH=${MANPATH}:/usr/local/share/man:/usr/local/man
export MANPATH=${MANPATH}:/usr/X11R6/man

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin

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
	#ulimit -s unlimited #stacksize
	#ulimit -d unlimited #datasize
	;;
esac

export PATH=${HOME}/local/bin:${HOME}/local/${ARCH}/bin:${PATH}

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

    # Apparently older bash versions don't have the \A sequence for
    # substituting the time in 24-hour format, so we use the 12-hour am/pm
    # sequence \@ instead

    case $TERM in
	dumb)
	    PS1="[\h \@ \!]$ "
	    ;;
	*)
	    case $BASH_VERSION in
		2.02*|2.04*)
		    # [hostname HH:MMam/pm]$
		    PS1="\[\e[40;33;1m\][\h \@ \!]$\[\e[0m\] "
		    ;;
		*)
		    # [hostname HH:MM]$
		    PS1="\[\e[40;33;1m\][\h \A \!]$\[\e[0m\] "
		    ;;
	    esac
    esac

    case $HOSTNAME in
	goethe*)
	    alias ls='ls -F --color=tty'
	    ;;
	*)
	    alias ls='ls -F --color=tty'
	    ;;
    esac

    # A color init string consists of one or more of the following numeric codes:
    # (run /usr/bin/dircolors --print-database for more detail)

    # Attribute codes:
    # 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed

    # Text color codes:
    # 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white

    # Background color codes:
    # 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white

    # file types:
    # no = normal, default
    # fi = normal file
    # di = directory
    # ln = symbolic link
    # pi = pipe
    # so = socket
    # do = door
    # bd = block device driver
    # cd = character device driver
    # or = orphan link to nonexistent file
    # ex = file with execute permission
    LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:'

    # file extensions for archive files
    archive_style="01;31"
    archive_exts="tar tgz arj taz lzh zip z Z gz bz2 deb rpm jar"
    for ext in $archive_exts; do
	LS_COLORS="${LS_COLORS}*.${ext}=${archive_style}:"
    done

    # file extensions for media files
    media_style="01;35"
    media_exts="jpg jpeg gif bmp pbm pgm ppm pnm tga xbm xpm tif tiff png mpg mpeg avi fli gl dl xcf xwd ogg mp3 wav"
    for ext in $media_exts; do
	LS_COLORS="${LS_COLORS}*.${ext}=${media_style}:"
    done

    LS_COLORS=${LS_COLORS}'*.ps=01:*.pdf=01:'

    export LS_COLORS

    alias pwd='dirs -l'
    alias matlab='matlab -nojvm -nosplash'

    # to keep case-sensitive sorting for 'ls', for example
    export LC_COLLATE=C
fi

export GROOVX_LIB_DIR=${HOME}/projects/groovx/share

export MAILTO=rjpeters@klab.caltech.edu

export PVM_ROOT=/usr/share/pvm3

export CVS_RSH=ssh

export INPUTRC=${HOME}/.inputrc

### Source a system-local init file, if it exists
if test -r ~/.bashrc_local; then
    . ~/.bashrc_local
fi
