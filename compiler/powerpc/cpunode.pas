{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

  interface

  implementation

    uses
       { generic nodes }
       ncgbas,ncgld,ncgflw,ncgcnv,ncgmem,ncgcon,ncgcal,ncgset,ncginl,ncgopt,
       { to be able to only parts of the generic code,
         the processor specific nodes must be included
         after the generic one (FK)
       }
       nppcadd,
       nppccal,
//       nppccon,
//       nppcflw,
//       nppcmem,
       nppcset,
       nppcinl,
//       nppcopt,
       { this not really a node }
//       nppcobj,
       nppcmat,
       nppccnv
       ;

end.
{
  $Log$
  Revision 1.16  2003-04-24 11:24:00  florian
    * fixed several issues with nested procedures

  Revision 1.15  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.14  2002/08/11 11:39:12  jonas
    + powerpc-specific genlinearlist

  Revision 1.13  2002/08/10 17:15:00  jonas
    + abs, sqr, sqrt implementations

  Revision 1.12  2002/08/10 14:52:52  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.11  2002/07/29 09:22:20  jonas
    + nppcmem

  Revision 1.10  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.9  2002/07/26 12:31:56  jonas
    + intial implementation of add nodes, only integer/enumeration/pointer/...
      handling is finished

  Revision 1.8  2002/07/21 16:58:59  jonas
    + include ncgset unit

  Revision 1.7  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.6  2002/07/11 07:42:31  jonas
    * fixed nppccnv and enabled it
    - removed PPC specific second_int_to_int and use the generic one instead

  Revision 1.5  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.4  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.2  2002/05/13 19:52:46  peter
    * a ppcppc can be build again

  Revision 1.1  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
