#!/bin/tcsh -*- shell-script -*-

# Startup file for csh/tcsh login shells.

# Rob Peters <rjpeters@klab.caltech.edu>

# $Id$

# This does't exist universally
if (-x /usr/games/bin/fortune) /usr/games/bin/fortune -a
if (-x /usr/games/fortune) /usr/games/fortune

# Commented out the terminal stuff -- seems to break things on the ilab
# cluster, and seems to not really be necessary anyway.

## Terminal stuff.  Assumes that network means vt100, since things
## like rlogin pass terminal info.
#echo "TERM: ($TERM)"
#switch ($TERM)
#    case 'dialup':
#    case 'network':
#    case 'unknown':
#	set noglob
#	eval `tset -I -Q -s vt100`
#	unset noglob
#	breaksw
#    default:
#	set noglob
#	eval `tset -I -Q -s $TERM`
#	unset noglob
#	breaksw
#endsw
