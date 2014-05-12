{
    Copyright (c) 1998-2014 by Florian Klaempfl

    Generate i386 assembler for load nodes

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
unit n386ld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symsym,
      node,ncgld,pass_1;

    type
      ti386loadnode = class(tcgloadnode)
         procedure generate_absaddr_access(vs: tabsolutevarsym); override;
      end;


implementation

    uses
      globals,
      symcpu,
      nld,
      cpubase;

{*****************************************************************************
                            TI386LOADNODE
*****************************************************************************}

    procedure ti386loadnode.generate_absaddr_access(vs: tabsolutevarsym);
      begin
        if tcpuabsolutevarsym(symtableentry).absseg then
          location.reference.segment:=NR_FS;
        inherited;
      end;


begin
   cloadnode:=ti386loadnode;
end.
