{
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    procinfo,
    psub;

  type
    taarch64procinfo=class(tcgprocinfo)
      constructor create(aparent: tprocinfo); override;
      procedure set_first_temp_offset; override;
    end;

implementation

  uses
    tgobj,
    cpubase;

  constructor taarch64procinfo.create(aparent: tprocinfo);
    begin
      inherited;
      { use the stack pointer as framepointer, because
         1) we exactly know the offsets of the temps from the stack pointer
            after pass 1 (based on the require parameter stack size for called
            routines), while we don't know it for the frame pointer (it depends
            on the number of saved registers)
         2) temp offsets from the stack pointer are positive while those from
            the frame pointer are negative, and we can directly encode much
            bigger positive offsets in the instructions
      }
      framepointer:=NR_STACK_POINTER_REG;
    end;

  procedure taarch64procinfo.set_first_temp_offset;
    begin
     { leave room for allocated parameters }
     tg.setfirsttemp(align(maxpushedparasize,16));
    end;


begin
  cprocinfo:=taarch64procinfo;
end.
