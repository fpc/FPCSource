{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the PowerPC optimizer object

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


Unit aoptcpu;

Interface

uses cpubase, aoptobj, aoptcpub;

Type
  TAOptCpu = Object(TAoptObj)
    { uses the same constructor as TAopObj }
  End;

Implementation

End.
{
 $Log$
 Revision 1.3  2002-05-16 19:46:52  carl
 + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
 + try to fix temp allocation (still in ifdef)
 + generic constructor calls
 + start of tassembler / tmodulebase class cleanup

 Revision 1.1  2001/08/26 13:31:04  florian
   * some cg reorganisation
   * some PPC updates

 Revision 1.2  2001/08/26 13:29:33  florian
   * some cg reorganisation
   * some PPC updates

 Revision 1.1  2000/07/13 06:30:12  michael
   + Initial import

 Revision 1.2  2000/01/07 01:14:57  peter
   * updated copyright to 2000

 Revision 1.1  1999/12/24 22:49:23  jonas
   + dummy to allow compiling

}
