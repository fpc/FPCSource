{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate ppc assembler for nodes that handle loads and assignments

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
unit nppcld;

{$i fpcdefs.inc}

  interface

    uses
      node,ncgld;

    type
      tppcloadnode = class(tcgloadnode)
        procedure generate_picvaraccess;override;
      end;


  implementation

    uses
      verbose,
      systems,
      cpubase,
      cgutils,cgobj,
      aasmbase,aasmtai,
      symconst,symsym,
      procinfo,
      nld;


    procedure tppcloadnode.generate_picvaraccess;
      var
        l : tasmsymbol;
        ref : treference;
      begin
        case target_info.system of
          system_powerpc_darwin:
            begin
              if (tvarsym(symtableentry).owner.unitid<>0) or (vo_is_dll_var in tvarsym(symtableentry).varoptions) then
                begin
                  l:=objectlibrary.getasmsymbol('L'+tvarsym(symtableentry).mangledname+'$non_lazy_ptr');
                  if not(assigned(l)) then
                    begin
                      l:=objectlibrary.newasmsymbol('L'+tvarsym(symtableentry).mangledname+'$non_lazy_ptr',AB_COMMON,AT_DATA);
                      picdata.concat(tai_symbol.create(l,0));
                      picdata.concat(tai_const_symbol.create_indirect(objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA)));
                      picdata.concat(tai_const.create_32bit(0));
                    end;

                  reference_reset_symbol(ref,l,0);
                  ref.base:=current_procinfo.got;
                  ref.relsymbol:=current_procinfo.gotlabel;
                  reference_reset_base(location.reference,cg.getaddressregister(exprasmlist),0);
                  cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,ref,location.reference.base);
                end
              else
                internalerror(200403021);
            end
          else
            internalerror(200402291);
        end;
      end;


begin
   cloadnode:=tppcloadnode;
end.
{
  $Log$
  Revision 1.1  2004-03-02 17:32:12  florian
    * make cycle fixed
    + pic support for darwin
    + support of importing vars from shared libs on darwin implemented
}
