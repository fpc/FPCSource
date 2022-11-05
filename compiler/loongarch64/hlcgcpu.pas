{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines high-level cg support LoongArch64

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
unit hlcgcpu;

{$i fpcdefs.inc}

interface

uses
  globals,
  aasmdata,
  symtype,symdef,
  cgbase,cgutils,hlcgobj,hlcg2ll, parabase;

type

  thlcgloongarch64 = class(thlcg2ll)
    protected
    public
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
  end;

implementation

  uses
    verbose,
    systems,fmodule,
    symconst, symsym,
    aasmbase,aasmtai,aasmcpu,
    cpubase,globtype,
    procinfo,cpupi,cgobj,cgcpu,
    defutil;


  procedure thlcgloongarch64.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    procedure loadvmttor12;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
      begin
        reference_reset_base(href,voidpointertype,NR_R4,0,ctempposinvalid,sizeof(pint),[]);
        href.refaddr:=addr_reg_12i;
        list.concat(taicpu.op_reg_ref(A_LD_D,NR_R12,href));
      end;
    procedure op_onr12methodaddr;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
        offset: longint;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(2022111915);

        offset:=tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber);
        if not is_simm12(offset) then
          begin
            list.concat(taicpu.op_reg_const(A_LI_D,NR_R15,offset));
            list.concat(taicpu.op_reg_reg_reg(A_ADD_D,NR_R12,NR_R12,NR_R15));
            offset:=0;
          end;
        reference_reset_base(href,voidpointertype,NR_R12,offset,ctempposinvalid, sizeof(pint),[]);
        href.refaddr:=addr_reg_12i;
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);

        reference_reset_base(href,voidpointertype,NR_R12,0,ctempposinvalid,0,[]);
        href.refaddr:=addr_reg;
        list.concat(taicpu.op_ref(A_JR,href));
      end;
    var
      make_global : boolean;
      tmpref , href: treference;
      l : TAsmLabel;
      hsym: tsym;
      paraloc: PCGParaLocation;
      tmpreg: TRegister;
    begin
      if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
        Internalerror(2022111909);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(2022111910);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(2022111911);

      make_global:=false;
      if (not current_module.is_unit) or
         create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,voidcodepointertype))
      else
        list.concat(Tai_symbol.Createname_hidden(labelname,AT_FUNCTION,0,voidcodepointertype));

      { the wrapper might need aktlocaldata for the additional data to
        load the constant }
      current_procinfo:=cprocinfo.create(nil);

      { set param1 interface to self  }
      procdef.init_paraloc_info(callerside);
      hsym:=tsym(procdef.parast.Find('self'));
      if not(assigned(hsym) and
        (hsym.typ=paravarsym)) then
        internalerror(2022111912);
      paraloc:=tparavarsym(hsym).paraloc[callerside].location;
      if assigned(paraloc^.next) then
        InternalError(2022111913);
      case paraloc^.loc of
        LOC_REGISTER:
          begin
            if is_simm12(ioffset) then
              cg.a_op_const_reg(list,OP_SUB, paraloc^.size,ioffset,paraloc^.register)
            else
              begin
                cg.a_load_const_reg(list, paraloc^.size, ioffset, NR_R13);
                cg.a_op_reg_reg(list, OP_SUB, paraloc^.size, NR_R13, paraloc^.register);
              end;
          end;
      else
        internalerror(2022111914);
      end;

      { case 4 }
      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          loadvmttor12;
          op_onr12methodaddr;
        end
      else
        begin
          tmpreg:=NR_R12;
          reference_reset_symbol(href,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION),0,0,[]);
          href.refaddr:=addr_pc_hi20;
          list.concat(taicpu.op_reg_ref(A_PCALAU12I,tmpreg,href));
          href.refaddr:=addr_pc_lo12;
          list.concat(taicpu.op_reg_reg_ref(A_ADDI_D,tmpreg,tmpreg,href));
          reference_reset_base(href,voidpointertype,tmpreg,0,ctempposinvalid,0,[]);
          href.refaddr:=addr_reg;
          list.concat(taicpu.op_ref(A_JR,href));
        end;
      list.concatlist(current_procinfo.aktlocaldata);

      current_procinfo.Free;
      current_procinfo:=nil;

      list.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure create_hlcodegen_cpu;
    begin
      hlcg:=thlcgloongarch64.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgloongarch64;
  create_hlcodegen:=@create_hlcodegen_cpu;
end.

