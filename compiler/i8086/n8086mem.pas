{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i8086 assembler for in memory related nodes

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
unit n8086mem;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cgbase,cpuinfo,cpubase,
      node,nmem,ncgmem,nx86mem;

    type
       ti8086derefnode = class(tx86derefnode)
         procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,globals,
      cutils,verbose,
      symbase,symconst,symdef,symtable,symtype,symsym,
      parabase,paramgr,
      aasmtai,aasmdata,
      nld,ncon,nadd,
      cgutils,cgobj,
      defutil,hlcgobj,
      pass_2,ncgutil;

{*****************************************************************************
                             TI8086DEREFNODE
*****************************************************************************}

    procedure ti8086derefnode.pass_generate_code;
      var
        paraloc1 : tcgpara;
        pd : tprocdef;
        sym : tsym;
        st : tsymtable;
        tmpref: treference;
      begin
        if tpointerdef(left.resultdef).x86pointertyp in [x86pt_far,x86pt_huge] then
          begin
            secondpass(left);
            { assume natural alignment, except for packed records }
            if not(resultdef.typ in [recorddef,objectdef]) or
               (tabstractrecordsymtable(tabstractrecorddef(resultdef).symtable).usefieldalignment<>1) then
              location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),resultdef.alignment)
            else
              location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),1);
            if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER,LOC_CREFERENCE,LOC_REFERENCE,LOC_CONSTANT]) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            case left.location.loc of
               LOC_CREGISTER,
               LOC_REGISTER:
                 begin
                   hlcg.maybe_change_load_node_reg(current_asmdata.CurrAsmList,left,true);
                   location.reference.base := left.location.register;
                   location.reference.segment := GetNextReg(left.location.register);
                 end;
               LOC_CREFERENCE,
               LOC_REFERENCE:
                 begin
                    location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                    cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,left.location.reference,location.reference.base);
                    location.reference.segment:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
                    tmpref:=left.location.reference;
                    inc(tmpref.offset,2);
                    cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,tmpref,location.reference.segment);
                 end;
               LOC_CONSTANT:
                 begin
                   location.reference.offset:=left.location.value and $FFFF;
                   location.reference.segment:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
                   cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_16,(left.location.value shr 16) and $FFFF,location.reference.segment);
                 end;
               else
                 internalerror(200507031);
            end;
            if (cs_use_heaptrc in current_settings.globalswitches) and
               (cs_checkpointer in current_settings.localswitches) and
               not(cs_compilesystem in current_settings.moduleswitches) and
   {$ifdef x86}
               (tpointerdef(left.resultdef).x86pointertyp = default_x86_data_pointer_type) and
   {$endif x86}
               not(nf_no_checkpointer in flags) and
               { can be NR_NO in case of LOC_CONSTANT }
               (location.reference.base<>NR_NO) then
             begin
               if not searchsym_in_named_module('HEAPTRC','CHECKPOINTER',sym,st) or
                  (sym.typ<>procsym) then
                 internalerror(2012010601);
               pd:=tprocdef(tprocsym(sym).ProcdefList[0]);
               paraloc1.init;
               paramanager.getintparaloc(pd,1,paraloc1);
               hlcg.a_load_reg_cgpara(current_asmdata.CurrAsmList,resultdef,location.reference.base,paraloc1);
               paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
               paraloc1.done;
               hlcg.allocallcpuregisters(current_asmdata.CurrAsmList);
               hlcg.a_call_name(current_asmdata.CurrAsmList,pd,'FPC_CHECKPOINTER',nil,false);
               hlcg.deallocallcpuregisters(current_asmdata.CurrAsmList);
             end;
          end
        else
          inherited pass_generate_code;
      end;


begin
  cderefnode:=ti8086derefnode;
end.
