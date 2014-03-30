{
    Copyright (c) 2002-2014 by Florian Klaempfl

    Generate i8086 assembler for load nodes

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
unit n8086ld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symsym,
      node,ncgld;

    type
      ti8086loadnode = class(tcgloadnode)
         procedure generate_absaddr_access(vs: tabsolutevarsym); override;
      end;


implementation

    uses
      globals,aasmdata,
      symcpu,
      nld,
      cgbase,cgobj;

{*****************************************************************************
                            TI8086LOADNODE
*****************************************************************************}

    procedure ti8086loadnode.generate_absaddr_access(vs: tabsolutevarsym);
      begin
        if tcpuabsolutevarsym(symtableentry).absseg then
          begin
            location.reference.segment:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
            cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_16,aint(tcpuabsolutevarsym(symtableentry).addrsegment),location.reference.segment);
          end;
        inherited;
      end;


begin
   cloadnode:=ti8086loadnode;
end.
