
{  useamigasmartlink.inc }

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
   This includefile sets the define use_amiga_smartlink.

   This way I can be sure that all units that reads
   this includefile is compiled with smartlink.

   If you don't want smartlink just put a space before
   $define use_amiga_smartlink.

   11 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se

}

{$ifndef use_amiga_smartlink}
   {$define use_amiga_smartlink}
{$endif}



