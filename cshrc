#!/bin/tcsh -*- shell-script -*-

# csh/tcsh init file

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

# umask: three octal digits apply to user/group/other permissions --
# the value of the umask is *subtracted* from the permissions settings
# that would otherwise apply to a file
#
# so if umask digit contains a '4', r is turned off
#    if umask digit contains a '2', w is turned off
#    if umask digit contains a '1', x is turned off

umask 077  # turns off all permissions for other+group by default

# Find out what kind of machine we are on.
if (-x /bin/arch) then
    setenv ARCH `/bin/arch`
else if (-x /usr/bin/arch) then
    setenv ARCH `/usr/bin/arch`
else if (`uname -m` == "Power Macintosh") then
    setenv ARCH "ppc"
else
    echo "Warning: unknown architecture, setting ARCH to 'unknown'"
    setenv ARCH "unknown"
endif

setenv MANPATH ${HOME}/local/$ARCH/man
setenv MANPATH ${MANPATH}:/usr/share/man:/usr/man
setenv MANPATH ${MANPATH}:/usr/local/share/man:/usr/local/man

setenv PATH .:${HOME}/local/bin:${HOME}/local/${ARCH}/bin:/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin

setenv LD_LIBRARY_PATH ${HOME}/local/${ARCH}/lib

switch ($ARCH)
    case hp9000s700:
        setenv PATH ${PATH}:/opt/aCC/bin:/opt/langtools/bin:/opt/imake/bin:/usr/ucb:/usr/ccs/bin
        setenv MANPATH ${MANPATH}:/opt/langtools/share/man:/opt/audio/share/man
        breaksw
    case ppc:
	# Initialize Fink if we're on ppc and it's available
	if (-r /sw/bin/init.csh) then
	    source /sw/bin/init.csh
	endif

	# Darwin for some reason has a limited stacksize by default
	unlimit stacksize datasize
	breaksw
endsw

# for interactive shells:
if ($?prompt) then

    set filec  # enable filename completion
    set matchbeep nomatch # only beep for missing, but not for ambiguous, matches
    set fignore=("~")  # filename suffixes to be ignored by completion
    set time=10
    set history=500
    set autolist # automatically list possible filename completions

    set prompt="%B[%m %T \!]%%%b "

    switch ($HOST)
	case goethe*:
	    alias ls 'ls -F'
	    breaksw
	default:
	    alias ls 'ls -F --color=tty'
	    breaksw
    endsw

    alias pwd 'dirs -l'

    # to keep case-sensitive sorting for 'ls', for example
    setenv LC_COLLATE C
endif

setenv GRSH_LIB_DIR ${HOME}/projects/grsh/share

setenv MAILTO rjpeters@klab.caltech.edu

setenv PVM_ROOT /usr/share/pvm3

setenv CVS_RSH ssh

setenv MATLABPATH ${HOME}/local/matlab

### Source a system-local init file, if it exists
if (-r ~/.cshrc_local) then
    source ~/.cshrc_local
endif
