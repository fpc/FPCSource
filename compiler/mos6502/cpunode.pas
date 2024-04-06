{
    Copyright (c) 2000-2017 by Florian Klaempfl

    This unit includes the MOS Technology 6502 code generator into the compiler

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
       ncgbas,ncgld,ncgflw,ncgcnv,ncgmem,ncgcon,ncgcal,ncgset,ncginl,ncgopt,ncgmat,ncgadd
       { to be able to only parts of the generic code,
         the processor specific nodes must be included
         after the generic one (FK)
       }
       ,nmos6502add
       ,nmos6502cal
       ,nmos6502mat
       ,nmos6502mem
       ,nmos6502inl
//       ,nmos6502cnv
//       ,nmos6502util,
       { these are not really nodes }
       ,tgcpu
       { symtable }
       ,symcpu,
       aasmdef
       ;


end.
