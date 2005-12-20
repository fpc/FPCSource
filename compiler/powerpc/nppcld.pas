{
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
        procedure pass_2; override;
      end;


  implementation

    uses
      verbose,
      systems,
      globtype,globals,defutil,
      cpubase, aasmbase, aasmtai,
      cgutils,cgbase,cgobj,cgcpu,
      symconst,symsym,
      procinfo,
      nld;


    procedure tppcloadnode.pass_2;
      var
        l : tasmsymbol;
        ref : treference;
        symname: string;
      begin
        symname := '';
        case target_info.system of
          system_powerpc_darwin:
            begin
              case symtableentry.typ of
                procsym:
                  begin
                    if (po_external in tprocsym(symtableentry).procdef[1].procoptions) then
                      symname := tprocsym(symtableentry).procdef[1].mangledname;
                  end;
                globalvarsym:
                  begin
                    if ([vo_is_dll_var,vo_is_external] * tglobalvarsym(symtableentry).varoptions <> []) then
                      symname := tglobalvarsym(symtableentry).mangledname;
                  end;
              end;
            end;
        end;
        if (symname = '') then
          inherited pass_2
        else
          begin
            location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
            reference_reset_base(location.reference,cg.getaddressregister(exprasmlist),0);
            location.reference.base := tcgppc(cg).g_darwin_indirect_sym_load(exprasmlist,symname);
          end;
      end;
      

begin
   cloadnode:=tppcloadnode;
end.
