{
    $Id$
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
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
 Revision 1.4  2002-05-18 13:34:26  peter
   * readded missing revisions

 Revision 1.3  2002/05/16 19:46:52  carl
 + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
 + try to fix temp allocation (still in ifdef)
 + generic constructor calls
 + start of tassembler / tmodulebase class cleanup

}
