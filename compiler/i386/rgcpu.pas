{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the i386 specific class for the register
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
      cpubase,
      cpuinfo,
      aasmbase,aasmtai,aasmdata,
      cclasses,globtype,cgbase,rgobj,rgx86;

    type
       trgcpu = class(trgx86)
          procedure add_constraints(reg:Tregister);override;
       end;

implementation

    uses
       systems,
       verbose;

    const
       { This value is used in tsaved. If the array value is equal
         to this, then this means that this register is not used.}
       reg_not_saved = $7fffffff;

{************************************************************************
                                 trgcpu
*************************************************************************}

    procedure trgcpu.add_constraints(reg:Tregister);
      var
        supreg : tsuperregister;
      begin
        if getsubreg(reg) in [R_SUBL,R_SUBH] then
          begin
            { Some registers have no 8-bit subregister }
            supreg:=getsupreg(reg);
            add_edge(supreg,RS_ESI);
            add_edge(supreg,RS_EDI);
            add_edge(supreg,RS_EBP);
          end;
      end;


end.
