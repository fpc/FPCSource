
{  useautoopenlib.inc }

{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
   In this includefile you can set some defines on how
   to compile amiga units.

   use_auto_openlib.
   If you use use_auto_openlib the unit will compile
   with autoopening of the unit at startup. You don't
   have to think about open and close the library, all
   is done in the unit. One problem is that the library
   will be loaded at startup so there can be a memory-
   problem. As default I have compiled all amiga system
   units as autoopened. Exec, intuition, amigados and
   utility is all opened on startup by sysamiga.


   use_init_openlib.
   If you compile with this define you get a procedure
   in every unit as an example for asl.library you
   have "procedure InitAslLibrary;". All you have to
   do is
   InitAslLibrary in the beginning of your program.
   You don't have to close the library the unit will
   handle that.

   dont_use_openlib.
   This is the standard amiga way. You have to open
   the library yourself and at the end close it.

   When you compile a unit you will get warnings or
   info on how the defines are set.

   First version of this include.
   11 Jan 2003.

   Added use_init_openlib and dont_use_openlib.
   21 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se

}

{
   Make sure that there is only one define set.
   Just put a space before the define to undef
}

{$define use_auto_openlib}
{ $define use_init_openlib}
{ $define dont_use_openlib}

{$ifdef use_auto_openlib}
   {$undef use_init_openlib}
   {$undef use_init_openlib}
{$endif use_auto_openlib}

{$ifdef use_init_openlib}
   {$undef use_auto_openlib}
   {$undef dont_use_openlib}
{$endif use_init_openlib}

{$ifdef dont_use_openlib}
   {$undef use_auto_openlib}
   {$undef use_init_openlib}
{$endif dont_use_openlib}




