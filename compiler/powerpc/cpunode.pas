{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the PowerPC code generator

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
unit cpunode;

{$i defines.inc}

  interface

  implementation

    uses
//       nppcadd,
//       nppccal,
//       nppccon,
//       nppcflw,
       nppcmat,
//       nppcmem,
//       nppcset,
//       nppcinl,
//       nppcopt,
       { this not really a node }
//       nppcobj,
       { generic nodes }
       ncgbas,ncgld,ncgflw,ncgcnv,ncgmem,ncgcon
       ;

end.
{
  $Log$
  Revision 1.3  2002-05-14 19:35:01  peter
    * removed old logs and updated copyright year

  Revision 1.2  2002/05/13 19:52:46  peter
    * a ppcppc can be build again

  Revision 1.1  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
