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
        procedure pass_2; override;
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


    procedure tppcloadnode.pass_2;
      var
        l : tasmsymbol;
        ref : treference;
      begin
        case target_info.system of
          system_powerpc_darwin:
            begin
              if (symtableentry.typ = procsym) and
                 not assigned(left) and
                 ((tprocsym(symtableentry).owner.unitid<>0) or
                  (po_external in tprocsym(symtableentry).procdef[1].procoptions)) then
                begin
                  l:=objectlibrary.getasmsymbol('L'+tprocsym(symtableentry).procdef[1].mangledname+'$non_lazy_ptr');
                  if not(assigned(l)) then
                    begin
                      l:=objectlibrary.newasmsymbol('L'+tprocsym(symtableentry).procdef[1].mangledname+'$non_lazy_ptr',AB_COMMON,AT_DATA);
                      picdata.concat(tai_symbol.create(l,0));
                      picdata.concat(tai_const.create_indirect_sym(objectlibrary.newasmsymbol(tprocsym(symtableentry).procdef[1].mangledname,AB_EXTERNAL,AT_DATA)));
                      picdata.concat(tai_const.create_32bit(0));
                    end;
                  reference_reset_symbol(ref,l,0);
                  reference_reset_base(location.reference,cg.getaddressregister(exprasmlist),0);
                  cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,ref,location.reference.base);
                end
              else
                inherited pass_2;
            end;
          else
            inherited pass_2;
        end;
      end;

    procedure tppcloadnode.generate_picvaraccess;
      var
        l : tasmsymbol;
        ref : treference;
      begin
        case target_info.system of
          system_powerpc_darwin:
            begin
              if (tglobalvarsym(symtableentry).owner.unitid<>0) or
                 (vo_is_dll_var in tglobalvarsym(symtableentry).varoptions) then
                begin
                  l:=objectlibrary.getasmsymbol('L'+tglobalvarsym(symtableentry).mangledname+'$non_lazy_ptr');
                  if not(assigned(l)) then
                    begin
                      l:=objectlibrary.newasmsymbol('L'+tglobalvarsym(symtableentry).mangledname+'$non_lazy_ptr',AB_COMMON,AT_DATA);
                      picdata.concat(tai_symbol.create(l,0));
                      picdata.concat(tai_const.create_indirect_sym(objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA)));
                      picdata.concat(tai_const.create_32bit(0));
                    end;

                  reference_reset_symbol(ref,l,0);
{                  ref.base:=current_procinfo.got;
                  ref.relsymbol:=current_procinfo.gotlabel;}
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
  Revision 1.5  2004-11-11 19:31:33  peter
    * fixed compile of powerpc,sparc,arm

  Revision 1.4  2004/07/19 12:45:43  jonas
    * fixed loading external procedure addresses

  Revision 1.3  2004/06/17 16:55:46  peter
    * powerpc compiles again

  Revision 1.2  2004/03/05 22:17:11  jonas
    * fixed importing of variables from shared libraries, but disabled
      PIC support for now. You have to save/restore r31 when you us it! :)
      Also, it's not necessary to support the imported variables

  Revision 1.1  2004/03/02 17:32:12  florian
    * make cycle fixed
    + pic support for darwin
    + support of importing vars from shared libs on darwin implemented
}
