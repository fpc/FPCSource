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
       narmcal,
       narmmat,
       narminl,
       narmcnv
       ;

end.
{
  $Log$
  Revision 1.10  2005-01-30 14:43:40  florian
    * fixed compilation of arm compiler

  Revision 1.9  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.8  2004/03/21 22:40:15  florian
    + added interface support for the arm
    * added  FPC_REQUIRES_PROPER_ALIGNMENT define for targets which require proper alignment

}
