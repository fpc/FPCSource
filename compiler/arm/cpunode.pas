{
    $Id$
    Copyright (c) 2000-2003 by Florian Klaempfl

    This unit includes the ARM code generator into the compiler

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
       ncgbas,ncgld,ncgflw,ncgcnv,ncgmem,ncgcon,ncgcal,ncgset,ncginl,ncgopt,ncgmat,
       { to be able to only parts of the generic code,
         the processor specific nodes must be included
         after the generic one (FK)
       }
       narmadd,
//!!!       narminl,
       narmcnv
       ;

end.
{
  $Log$
  Revision 1.5  2003-08-25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.4  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.3  2003/08/21 23:24:08  florian
    * continued to work on the arm skeleton

  Revision 1.2  2003/08/21 03:14:00  florian
    * arm compiler can be compiled; far from being working

  Revision 1.1  2003/07/21 16:35:30  florian
    * very basic stuff for the arm
}
