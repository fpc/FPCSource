{
    $Id$
    Copyright (c) 2002 by Marco van de Voort.

    Implementation of the POSIX unit for *BSD. In practice only includes
    other files, or specifies libc bindings.

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

Uses Sysctl;

{$I ostypes.inc}
{$I bunxmacr.inc}

{$ifdef uselibc}
   {$Linklib c}

{   var
     Errno : cint; external name 'errno';}
   
   {$i oscdeclh.inc}

{$else}
   {$i sysdeclh.inc}
{$endif}

{$I bunxfunc.inc}

{
 $Log$
 Revision 1.2  2003-06-01 16:35:27  marco
  * Several small fixes to harmonize the *BSD rtls and Linux.

 Revision 1.1  2003/01/05 19:01:28  marco
  * FreeBSD compiles now with baseunix mods.

 Revision 1.8  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.7  2002/10/27 11:58:30  marco
  * Modifications from Saturday.

 Revision 1.6  2002/10/26 18:27:51  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.5  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.4  2002/08/21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.3  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.

}
