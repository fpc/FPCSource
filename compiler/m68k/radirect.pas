{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Reads inline assembler and writes the lines direct to the output
    This is not supported for the m68k

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
unit radirect;

{$i fpcdefs.inc}

interface

    uses
      node;

     function assemble : tnode;

  implementation

  uses
    verbose;


    function assemble : tnode;

     begin
       internalerror(20020813);
     end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}


end.
{
  $Log$
  Revision 1.2  2002-09-07 15:25:13  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.


}
