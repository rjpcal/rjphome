# Startup file for csh/tcsh login shells.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

echo "TERM: ($TERM)"

# This does't exist universally
if (-x /usr/games/bin/fortune) /usr/games/bin/fortune -a
if (-x /usr/games/fortune) /usr/games/fortune

# Terminal stuff.  Assumes that network means vt100, since things
# like rlogin pass terminal info.
switch ($TERM)
    case 'dialup':
    case 'network':
    case 'unknown':
	set noglob
	eval `tset -I -Q -s vt100`
	unset noglob
	breaksw
    default:
	set noglob
	eval `tset -I -Q -s $TERM`
	unset noglob
	breaksw
endsw
