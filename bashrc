#!/bin/bash -*- shell-script -*-
#
# Rob Peters <rjpeters at klab dot caltech dot edu>
#
# $Id$
######################################################################


######################################################################
# Figure out which known location we're in

function identify_location ()
{
    if test -r $HOME/.location; then
	export LOCATION=`cat $HOME/.location`
	echo "Location is '$LOCATION'"
    else
	echo "Warning! Location unknown!"
	export LOCATION="unknown"
    fi
}

######################################################################
# Setup PS1 variable to define command-line prompt

function setup_prompt ()
{
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

    prompt_color="40;37;1;4"  # default color is bold-white-on-black
    case $USER in
	# Only set the colors if the user is me!
	rjpeters)
	    case $HOSTNAME in
		sideswipe*)
		    prompt_color="40;33;1;4" # yellow on black
		    ;;
		hume*)
		    prompt_color="40;35;1;4" # magenta on black
		    ;;
		*.klab.caltech.edu|montaigne)
		    prompt_color="40;32;1;4" # green on black
		    ;;
		ilab*|iLab*)
		    prompt_color="40;31;1;4" # red on black
		    ;;
		fortune*)
		    prompt_color="40;34;1;4" # blue on black
		    ;;
		quantum*)
		    prompt_color="40;36;1;4" # cyan on black
	    esac
	    ;;
    esac

    escape1=""
    main_prompt=""
    prompt_content=""
    prompt_token="$"
    escape2=""
    term_title=""

    # This fragment: [\h \@ \!]$ is the actual prompt itself, giving
    # the hostname, the current time, and the history number

    case $BASH_VERSION in
	2.02*|2.04*)
	    # [hostname HH:MMam/pm]$
	    prompt_content="\h \@ \!"
	    ;;
	*)
	    # [hostname HH:MM]$
	    prompt_content="\h \A \!"
	    ;;
    esac

    case $USER in
	rjpeters)
	    prompt_token="$"
	    ;;
	*)
	    prompt_content="\u@${prompt_content}"
	    prompt_token="#"
	    ;;
    esac

    case $SSH_CLIENT in
	"")
	    main_prompt="[${prompt_content}]$prompt_token"
	    ;;
	*)
	    main_prompt="((${prompt_content}))$prompt_token"
	    ;;
    esac

    case $TERM in
	dumb)
	    escape1=""
	    escape2=""
	    ;;
	*)
            # This fragment: \[\e[${prompt_color}m\] is for setting
            # the text properties (color, bold, underlining, etc.) of
            # the prompt text
            escape1="\[\e[${prompt_color}m\]"

	    # This fragment: \[\e[0m\] is for restoring the text
	    # properties to their default values following the prompt
	    escape2="\[\e[0m\]"
    esac

    case $TERM in
	xterm*)
	    # This fragment: \[\e]0;\w@\H\a\] is for setting the
	    # window title of the containing terminal
	    term_title="\[\e]0;\W@\H\a\]"
	    ;;
	*)
	    term_title=""
	    ;;
    esac

    PS1="${escape1}${main_prompt}${escape2}${term_title} "
}

######################################################################
# Setup LS_COLORS environment variable to define what colors are used with ls

function setup_ls_colors ()
{
    # A color init string consists of one or more of the following
    # numeric codes:

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
}

######################################################################
# Setup ARCH enviroment variable giving the host machine's architecture

function setup_arch ()
{
    if test -x /bin/arch; then
	ARCH=`/bin/arch`
    elif test -x /usr/bin/arch; then
	ARCH=`/usr/bin/arch`
    elif test "`uname -m`" = "Power Macintosh"; then
	ARCH="ppc"
    else
	echo "Warning: unknown architecture, setting ARCH to 'unknown'"
	ARCH="unknown"
    fi

    export ARCH
}

######################################################################
# main initialization code

umask 077

setup_arch
identify_location

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
	;;
esac

export PATH=${HOME}/local/bin:${HOME}/local/${ARCH}/bin:${PATH}

# for interactive shells:
if test "$PS1" != ""; then

    FIGNORE=""   # filename suffixes to be ignored by completion
    HISTSIZE=500 # number of commands to store in history
    HISTFILE=""  # don't save/load command history from a file

    setup_prompt

    setup_ls_colors

    alias ls='ls -F --color=tty'
    alias pwd='dirs -l'
    alias matlab='matlab -nojvm -nosplash'

    # specify location of readline startup file
    export INPUTRC=${HOME}/.inputrc
fi

### Output from cron jobs gets sent to $MAILTO
export MAILTO=rjpeters@klab.caltech.edu

export PVM_ROOT=/usr/share/pvm3

export CVS_RSH=ssh

export SVN_EDITOR="emacs -nw"

# to keep case-sensitive sorting for 'ls', for example
export LC_COLLATE=C

# MATLAB path
# we don't need to add ${HOME}/matlab because this is done
# automatically (somehow); if it is added explicitly here, then MATLAB
# complains of a 'duplicate directory'
export MATLABPATH=./matlab:${HOME}/local/matlab

### Source a system-local init file, if it exists
if test -r ~/.bashrc_local; then
    . ~/.bashrc_local
fi
