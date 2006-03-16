{
    Copyright (c) 1998-2002 by The Free Pascal Team

    This unit does the parsing process for the inline assembler

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
{
  This unit does the parsing process for the inline assembler.
}
Unit Rasm;

{$i fpcdefs.inc}

Interface

uses
  node;

   {
     This routine is called to parse the instructions in assembler
     blocks. It returns a complete list of directive and instructions
   }
   function assemble: tnode;


Implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,
       { aasm }
       cpubase,aasmbase,aasmtai,aasmdata,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,
       { pass 1 }
       nbas,
       { parser }
       scanner
       // ,rautils
       ;

    function assemble : tnode;
     begin
     end;

Begin
end.
