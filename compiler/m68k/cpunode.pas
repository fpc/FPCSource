{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Includes the 680x0/Coldfire code generator

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
       ncgbas,ncgld,ncgflw,ncgcnv,ncgmem,ncgcon,ncgcal,ncgset,ncginl,ncgmat
       { to be able to only parts of the generic code,
         the processor specific nodes must be included
         after the generic one (FK)
       }
//       nm68kadd,
//       nppccal,
//       nppccon,
//       nppcflw,
//       nppcmem,
//       nppcset,
//       nppcinl,
//       nppcopt,
       { this not really a node }
//       nppcobj,
//       nppcmat,
         ,n68kcnv
       ;

end.
{
  $Log$
  Revision 1.2  2002-08-14 19:16:34  carl
    + m68k type conversion nodes
    + started some mathematical nodes
    * out of bound references should now be handled correctly

  Revision 1.1  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.


}
