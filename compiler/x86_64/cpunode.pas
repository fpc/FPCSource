{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the x86-64 code generator

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
{ This is a helper unit to include the necessary code generator units
  for the x86-64 processor.
}
unit cpunode;

{$i fpcdefs.inc}

  interface

  implementation

    uses
       ncgbas,
       ncgflw,
       ncgcnv,
       ncgmem,
       ncgcon,
       ncgld,
       ncgcal,
       // n386add,n386con,n386flw,n386mat,n386mem,
       // n386set,n386inl,n386opt,
       { this not really a node }
       // n386obj
       { the cpu specific node units must be used after the generic ones to
         get the correct class pointer }
       nx64cnv
       ;

end.
{
  $Log$
  Revision 1.3  2003-04-30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.2  2002/07/25 22:55:34  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}
