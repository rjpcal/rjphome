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
setenv ARCH `/bin/arch`

setenv MANPATH ${HOME}/local/$ARCH/man:/usr/share/man:/usr/man

setenv PATH .:${HOME}/local/bin:${HOME}/local/${ARCH}/bin:/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin

setenv LD_LIBRARY_PATH ${HOME}/local/${ARCH}/lib

switch ($ARCH)
    case hp9000s700:
        setenv PATH ${PATH}:/opt/aCC/bin:/opt/langtools/bin:/opt/imake/bin:/usr/ucb:/usr/ccs/bin
        setenv MANPATH ${MANPATH}:/opt/langtools/share/man:/opt/audio/share/man
        breaksw
    case i686:
        breaksw
    case irix6:
        breaksw
    case sun4:
        breaksw
endsw

set filec  # enables filename completion in csh
set matchbeep nomatch # only beep for missing, but not for ambiguous, matches
set fignore=("~")  # filename suffixes to be ignored by completion
set time=10
set history=500
set autolist # automatically list possible filename completions

# for interactive shells:
if ($?prompt) then
    set prompt="%B[%m %T \!]%%%b "
    if (-f ~/.aliases) then
          source ~/.aliases
    endif

    switch ($HOST)
    case socrates*:
    case goethe*:
    case soma*:
    case curie*:
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
