{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the powerpc specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,aasmdata,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         function getcpuregisterint(list: TAsmList; reg: tregister): tregister; override;
         procedure ungetregisterint(list: TAsmList; reg: tregister); override;
       end;

  implementation

    uses
      cgobj;

    function trgcpu.getcpuregisterint(list: TAsmList; reg: tregister): tregister;

      begin
        if reg = R_0 then
          begin
            cg.a_reg_alloc(list,reg);
            result := reg;
          end
        else result := inherited getcpuregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: TAsmList; reg: tregister);

      begin
        if reg = R_0 then
          cg.a_reg_dealloc(list,reg)
        else
          inherited ungetregisterint(list,reg);
      end;

initialization
  rg := trgcpu.create;
end.
