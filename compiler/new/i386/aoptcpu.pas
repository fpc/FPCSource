{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the i386 optimizer object

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
 Revision 1.1  2000-07-13 06:30:10  michael
 + Initial import

 Revision 1.6  2000/01/07 01:14:56  peter
   * updated copyright to 2000

 Revision 1.5  1999/08/18 14:32:24  jonas
   + compilable!
   + dataflow analyzer finished
   + start of CSE units
   + aoptbase which contains a base object for all optimizer objects
   * some constants and type definitions moved around to avoid circular
     dependencies
   * moved some methods from base objects to specialized objects because
     they're not used anywhere else

 Revision 1.4  1999/08/11 14:23:39  jonas
   * some fixes to RegReadByInstr

 Revision 1.3  1999/08/10 12:40:20  jonas
   + implemented RegReadByInstr

 Revision 1.1  1999/08/08 13:24:50  jonas
   + added copyright header/GNU license info
   * made the assembler optimizer almost completely OOP
   * some code style clean up and extra comments
   * moved from the new/aopt to the /new and /new/i386 dirs

}