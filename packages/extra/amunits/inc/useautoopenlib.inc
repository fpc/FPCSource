
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
   This includefile sets the define use_auto_openlib.
   
   It is set as standard and it means that all units 
   you use in your program will open the library itself.
   You don't have to worry about that.

   If you don't like the idea of autoopening of libraries
   or if you want to save on memoryuse cause some library
   will get opened and never used then just put a space 
   before $define use_auto_openlib.
   
   Be aware of that you have to open the library yourself.
   
   You don't have to open exec, amigados, utility and
   intuition they are opened by default.

   11 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se

}

{$ifndef use_auto_openlib}
   {$define use_auto_openlib}
{$endif}
