{
    $Id$
    Copyright (c) 2002 by Marco van de Voort.

    Implementation of the baseunix unit for *BSD^H^H^Hlinux. In practice only
    includes other files, or specifies libc bindings.

    The conditional uselibc can be used to switch from libc to syscall
    usage for basic primitives, but it is best to use unit POSIX if
    possible. Note that the system unit must also be compiled using uselibc.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

{ $I ostypes.inc}

{$ifdef uselibc}
 {$I oscdeclh.inc}
{$else}
 {$I sysdeclh.inc}
{$endif}

{$I bunxfunc.inc}
{$I genfuncs.inc}

{
 $Log$
 Revision 1.3  2003-06-01 16:28:56  marco
  * small rearrangements

 Revision 1.2  2003/01/02 22:39:19  marco
  * posmacro.inc was still include while it was already moved to bunxmacr.inc
    fixed, will remove posmacro.inc shortly

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.1  2002/11/12 14:37:59  marco
  * Parts of new unix rtl

}
