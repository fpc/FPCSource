{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the x86-64 specific class for the register
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
       cgbase,rgx86;

     type
       trgcpu = class(trgx86)
         procedure add_constraints(reg:Tregister); override;
       end;

  implementation

    uses
      cpubase;

    procedure trgcpu.add_constraints(reg:Tregister);
      var
        supreg : tsuperregister;
      begin
        supreg:=getsupreg(reg);
        { All registers conflict with rsp/rbp }
        add_edge(supreg,RS_RSP);
        add_edge(supreg,RS_RBP);
      end;

end.
