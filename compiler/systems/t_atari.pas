{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Amiga target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit t_atari;

{$i fpcdefs.inc}

interface


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_atari;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterTarget(system_m68k_atari_info);
end.
{
  $Log$
  Revision 1.1  2002-09-06 15:03:51  carl
    * moved files to systems directory

  Revision 1.12  2002/07/26 21:15:45  florian
    * rewrote the system handling

  Revision 1.11  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.10  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.7  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.6  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

}
