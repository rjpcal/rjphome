#!/bin/bash -*- shell-script -*-
#
# Rob Peters <rjpeters at klab dot caltech dot edu>
#
# $Id$
######################################################################

case $BASH_VERSION in
    2.*)
	:
	;;
    3.*)
	set -o pipefail
	;;
    *)
	echo "unknown bash version $BASH_VERSION"
	;;
esac

######################################################################
# Figure out which known location we're in

function identify_location ()
{
    if test -r $HOME/.location; then
	export LOCATION=`cat $HOME/.location`
    else
	export LOCATION="unknown"
    fi
}

######################################################################
# Setup PS1 variable to define command-line prompt

function setup_prompt ()
{
    if test $# -gt 0; then
	local default_prompt_token=$1
    else
	local default_prompt_token="$"
    fi

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

    local bkg=40
    local prompt_color="$bkg;37;1;4"  # default color is bold-white-on-black
    case $USER in
	# Only set the colors if the user is me!
	rjpeters)
	    case `hostname` in
		sideswipe*|laserbeak*|cosmos*)
		    prompt_color="$bkg;33;1;4" # yellow on black
		    ;;
		mirage*|computron*)
		    prompt_color="$bkg;35;1;4" # magenta on black
		    ;;
		*.klab.caltech.edu|montaigne|hume*)
		    prompt_color="$bkg;32;1;4" # green on black
		    ;;
		ilab*|iLab*)
		    prompt_color="$bkg;31;1;4" # red on black
		    ;;
		fortune*)
		    prompt_color="$bkg;34;1;4" # blue on black
		    ;;
		quantum*|hpc-master*|bumblebee|buzzsaw*|bluestreak*)
		    prompt_color="$bkg;36;1;4" # cyan on black
	    esac
	    ;;

	root)
	    prompt_color="41;37;1;4" # root gets bold-white-on-red
	    ;;
    esac

    local escape1=""
    local main_prompt=""
    local prompt_content=""
    local escape2=""
    local term_title=""

    # This fragment: [\h \@ \!]$ is the actual prompt itself, giving
    # the hostname, the current time, and the history number

    case $BASH_VERSION in
	2.02*|2.04*|2.05.0*)
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
	    prompt_token=$default_prompt_token
	    ;;
	*)
	    case $USERNAME in
		rjpeters)
		    prompt_token=$default_prompt_token
		    ;;
		*)
		    prompt_content="\u@${prompt_content}"
		    prompt_token="#"
		    ;;
	    esac
	    ;;
    esac

    case $SSH_CLIENT in
	"")
	    main_prompt="[${prompt_content}]$prompt_token"
	    ;;
	*)
	    main_prompt="((${prompt_content}))$prompt_token"
	    export SSH_CLIENT
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
	    case $LOCATION in
		sideswipe)
	            # hmm, with gnome-terminal 2.14, we need to drop
	            # the bracketing \[ and \], because with those we
	            # get extra cursor flicker
		    term_title="\e]0;\W@\H\a"
		    ;;

		*)

	            # This fragment: \[\e]0;\w@\H\a\] is for setting
	            # the window title of the containing terminal
		    term_title="\[\e]0;\W@\H\a\]"
		    ;;
	    esac
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
    local archive_style="01;31"
    local archive_exts="tar tgz arj taz lzh zip z Z gz bz2 deb rpm jar"
    local ext
    for ext in $archive_exts; do
	LS_COLORS="${LS_COLORS}*.${ext}=${archive_style}:"
    done

    # file extensions for media files
    local media_style="01;35"
    local media_exts="jpg jpeg gif bmp pbm pgm ppm pnm tga xbm xpm tif tiff png mpg mpeg avi fli gl dl xcf xwd ogg mp3 wav"
    local ext
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
	ARCH="unknown"
    fi

    export ARCH
}

######################################################################
# No-op function to facilitate comments in the history files

function histnote ()
{
    :
}

######################################################################
# No-op function to facilitate comments in the history files

function offtherecord ()
{
    unset PROMPT_COMMAND
    unset HISTFILE
    setup_prompt '*'
}

######################################################################
# main initialization code

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin

umask 077

setup_arch
identify_location

export MANPATH=/usr/man:/usr/share/man
export MANPATH=${MANPATH}:/usr/local/share/man:/usr/local/man
export MANPATH=${MANPATH}:/usr/X11R6/man

# default width to which man pages are formatted
export MANWIDTH=74

export LD_LIBRARY_PATH=${HOME}/local/${ARCH}/lib

case $ARCH in
    hp9000s700)
        export PATH=${PATH}:/opt/aCC/bin:/opt/langtools/bin:/opt/imake/bin:/usr/ucb:/usr/ccs/bin
        export MANPATH=${MANPATH}:/opt/langtools/share/man:/opt/audio/share/man
        breaksw
	;;
esac

case `uname -s` in
    Darwin)
	# Initialize Fink if we're on ppc and it's available
	if test -r /sw/bin/init.sh; then
	    . /sw/bin/init.sh
	fi
	;;
esac

export PATH=${HOME}/local/bin:${HOME}/local/${ARCH}/bin:${PATH}

case $- in
    *i*)  # interactive shell

	if test x$ARCH = xunknown; then
	    echo "Warning: unknown architecture, setting ARCH to 'unknown'"
	fi

	if test x$LOCATION = xunknown; then
	    echo "Warning! Location unknown!"
	else
	    echo "Location is '$LOCATION'"
	fi

	if test x$COLORTERM = xgnome-terminal && test x$TERM = xxterm; then
	    # Ubunutu default bashrc looks for "xterm-color" when
	    # deciding whether to colorize the prompt; however
	    # xterm-color does not support fancy lsb init-functions
	    # because "/usr/bin/tput hpa 60" does not work with
	    # xterm-color
	    export TERM=xterm-color
	fi

	FIGNORE=""   # filename suffixes to be ignored by completion
	HISTSIZE=500 # number of commands to store in history
	HISTTIMEFORMAT="[%Y-%m-%d %H:%M:%S %Z %a] " # stftime format for storing in history
	#HISTFILE=$HOME/home/history/`date +%Y%m%d`-$LOCATION-`hostname`-$(basename `tty`)
	HISTFILE=""
	HISTFILERAW=$HOME/home/history/`date +%Y%m%d`-$LOCATION-`hostname`-$(basename `tty`).raw

	mkdir -p $HOME/home/history

	setup_prompt

	if test "x$SSH_CONNECTION" = "x"; then
	    GET_LAST_HISTORY_COMMAND="echo 0 `date \"+$HISTTIMEFORMAT\"` console login `tty`"
	else
	    GET_LAST_HISTORY_COMMAND="echo 0 `date \"+$HISTTIMEFORMAT\"` ssh connection $SSH_CONNECTION"
	fi

	PREV_PWD=$PWD
	PROMPT_COMMAND="echo \"pwd=\$PREV_PWD user=\$USER uid=\$UID //// \`\$GET_LAST_HISTORY_COMMAND\`\" >> $HISTFILERAW; PREV_PWD=\$PWD; GET_LAST_HISTORY_COMMAND='history 1'"

	setup_ls_colors

	if ls -F --color=tty ~/.bashrc > /dev/null 2>&1; then
	    alias ls='ls -F --color=tty'
	elif ls -FG ~/.bashrc > /dev/null 2>&1; then
	    alias ls='ls -FG'
	elif ls -F ~/.bashrc > /dev/null 2>&1; then
	    alias ls='ls -F'
	fi
	alias pwd='dirs -l'
	alias matlab='matlab -nojvm -nosplash'

	alias semacs='emacs --font "-*-LucidaTypewriter-Medium-R-Normal-Sans-12-*-*-*-*-*-*-*"'

	# keep this last
	if which -i which < /dev/null > /dev/null 2>&1; then
	    alias which='alias | which -i'
	fi

        # specify location of readline startup file
	export INPUTRC=${HOME}/.inputrc

	export COLUMNS
	;;
    *)    # non-interactive shell
	;;
esac

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
export MATLABPATH=./matlab
if test -d ${HOME}/science/matlab; then
    export MATLABPATH="${MATLABPATH}:${HOME}/science/matlab"
fi
if test -d ${HOME}/projects/matlab; then
    export MATLABPATH="${MATLABPATH}:${HOME}/projects/matlab"
fi

### home computer
# export HOMEIP=24.205.94.103 # expired 2008-Mar-20
# export HOMEIP=75.142.48.226
# export HOMEIP=76.167.221.245 # rr
# export HOMEIP=76.172.150.216 # rr
#export HOMEIP=76.94.45.138 # rr
export HOMEIP=76.91.31.24 # rr

### Source a system-local init file, if it exists, and reset important env vars beforehand
export CPPFLAGS=""
export LDFLAGS=""
if test -r ~/.bashrc_local; then
    . ~/.bashrc_local
fi

if test -r ~/home/base/bashrc_$LOCATION; then
    . ~/home/base/bashrc_$LOCATION
fi

### Clean up now-unneeded function definitions
unset -f identify_location
#unset -f setup_prompt
unset -f setup_ls_colors
unset -f setup_arch
