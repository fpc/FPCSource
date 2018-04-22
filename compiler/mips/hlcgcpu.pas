{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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
  globtype,
  aasmbase, aasmdata,
  cgbase, cgutils,
  symtype,symdef,
  parabase, hlcgobj, hlcg2ll;

  type
    thlcgmips = class(thlcg2ll)
      function a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara; override;
      procedure a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);override;
    protected
      procedure a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt); override;
    public
      procedure g_intf_wrapper(list: tasmlist; procdef: tprocdef; const labelname: string; ioffset: longint); override;
      procedure a_jmp_external_name(list: TAsmList; const externalname: TSymStr);override;
  end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,globals,
    fmodule,
    aasmtai,aasmcpu,
    cutils,
    symconst,symsym,defutil,
    cgobj,
    cpubase,
    cpuinfo,
    cgcpu;

  function thlcgmips.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;
    var
      ref: treference;
      sym: tasmsymbol;
    begin
      if weak then
        sym:=current_asmdata.WeakRefAsmSymbol(s,AT_FUNCTION)
      else
        sym:=current_asmdata.RefAsmSymbol(s,AT_FUNCTION);

      if (po_external in pd.procoptions) then
        begin
          if not (cs_create_pic in current_settings.moduleswitches) then
            begin
              reference_reset_symbol(ref,current_asmdata.RefAsmSymbol('_gp',AT_DATA),0,sizeof(aint),[]);
              list.concat(tai_comment.create(strpnew('Using PIC code for a_call_name')));
              cg.a_loadaddr_ref_reg(list,ref,NR_GP);
            end;
          TCGMIPS(cg).a_call_sym_pic(list,sym);
        end
      else
        cg.a_call_name(list,s,weak);
      { set the result location }
      result:=get_call_result_cgpara(pd,forceresdef);
    end;


  procedure thlcgmips.a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);
    var
      cgsubsetsize,
      cgtosize: tcgsize;
    begin
      cgsubsetsize:=def_cgsize(subsetsize);
      cgtosize:=def_cgsize(tosize);
      if (current_settings.cputype<>cpu_mips32r2) and (current_settings.cputype<>cpu_pic32mx) then
        inherited a_load_subsetreg_reg(list,subsetsize,tosize,sreg,destreg)
      else if (sreg.bitlen>32) then
        InternalError(2013070201)
      else if (sreg.bitlen<>32) then
        begin
          list.concat(taicpu.op_reg_reg_const_const(A_EXT,destreg,sreg.subsetreg,
            sreg.startbit,sreg.bitlen));
          { types with a negative lower bound are always a base type (8, 16, 32 bits) }
          if (cgsubsetsize in [OS_S8..OS_S128]) then
            if ((sreg.bitlen mod 8) = 0) then
              begin
                cg.a_load_reg_reg(list,tcgsize2unsigned[cgsubsetsize],cgsubsetsize,destreg,destreg);
                cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,destreg,destreg);
              end
            else
              begin
                cg.a_op_const_reg(list,OP_SHL,OS_INT,32-sreg.bitlen,destreg);
                cg.a_op_const_reg(list,OP_SAR,OS_INT,32-sreg.bitlen,destreg);
              end;
        end
      else
        cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,sreg.subsetreg,destreg);
    end;


  procedure thlcgmips.a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt);
    begin
      if (current_settings.cputype<>cpu_mips32r2)  and (current_settings.cputype<>cpu_pic32mx) then
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt)
      else if (sreg.bitlen>32) then
        InternalError(2013070202)
      else if (sreg.bitlen<>32) then
        begin
          case slopt of
            SL_SETZERO:
              fromreg:=NR_R0;
            SL_SETMAX:
              begin
                fromreg:=cg.getintregister(list,OS_INT);
                cg.a_load_const_reg(list,OS_INT,-1,fromreg);
              end;
          end;
          list.concat(taicpu.op_reg_reg_const_const(A_INS,sreg.subsetreg,fromreg,
            sreg.startbit,sreg.bitlen));
        end
      else if not (slopt in [SL_SETZERO,SL_SETMAX]) then
        cg.a_load_reg_reg(list,def_cgsize(fromsize),def_cgsize(subsetsize),fromreg,sreg.subsetreg)
      else
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt);
    end;


  procedure thlcgmips.a_jmp_external_name(list: TAsmList; const externalname: TSymStr);
    var
      href: treference;
    begin
      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(externalname,AT_DATA),0,sizeof(aint),[]);
      { Always do indirect jump using $t9, it won't harm in non-PIC mode }
      if (cs_create_pic in current_settings.moduleswitches) then
        begin
          list.concat(taicpu.op_none(A_P_SET_NOREORDER));
          list.concat(taicpu.op_reg(A_P_CPLOAD,NR_PIC_FUNC));
          href.base:=NR_GP;
          href.refaddr:=addr_pic_call16;
          list.concat(taicpu.op_reg_ref(A_LW,NR_PIC_FUNC,href));
          list.concat(taicpu.op_reg(A_JR,NR_PIC_FUNC));
          { Delay slot }
          list.Concat(taicpu.op_none(A_NOP));
          list.Concat(taicpu.op_none(A_P_SET_REORDER));
        end
      else
        begin
          href.refaddr:=addr_high;
          list.concat(taicpu.op_reg_ref(A_LUI,NR_PIC_FUNC,href));
          href.refaddr:=addr_low;
          list.concat(taicpu.op_reg_ref(A_ADDIU,NR_PIC_FUNC,href));
          list.concat(taicpu.op_reg(A_JR,NR_PIC_FUNC));
          { Delay slot }
          list.Concat(taicpu.op_none(A_NOP));
        end;
    end;


  procedure thlcgmips.g_intf_wrapper(list: tasmlist; procdef: tprocdef; const labelname: string; ioffset: longint);
  var
    make_global: boolean;
    hsym: tsym;
    href: treference;
    paraloc: Pcgparalocation;
    IsVirtual: boolean;
  begin
    if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
      Internalerror(200006137);
    if not assigned(procdef.struct) or
      (procdef.procoptions * [po_classmethod, po_staticmethod,
      po_methodpointer, po_interrupt, po_iocheck] <> []) then
      Internalerror(200006138);
    if procdef.owner.symtabletype <> objectsymtable then
      Internalerror(200109191);

    make_global := False;
    if (not current_module.is_unit) or create_smartlink or
      (procdef.owner.defowner.owner.symtabletype = globalsymtable) then
      make_global := True;

    if make_global then
      List.concat(Tai_symbol.Createname_global(labelname, AT_FUNCTION, 0, procdef))
    else
      List.concat(Tai_symbol.Createname(labelname, AT_FUNCTION, 0, procdef));

    IsVirtual:=(po_virtualmethod in procdef.procoptions) and
        not is_objectpascal_helper(procdef.struct);

    if (cs_create_pic in current_settings.moduleswitches) and
      (not IsVirtual) then
      begin
        list.concat(Taicpu.op_none(A_P_SET_NOREORDER));
        list.concat(Taicpu.op_reg(A_P_CPLOAD,NR_PIC_FUNC));
        list.concat(Taicpu.op_none(A_P_SET_REORDER));
      end;

    { set param1 interface to self  }
    procdef.init_paraloc_info(callerside);
    hsym:=tsym(procdef.parast.Find('self'));
    if not(assigned(hsym) and
      (hsym.typ=paravarsym)) then
      internalerror(2010103101);
    paraloc:=tparavarsym(hsym).paraloc[callerside].location;
    if assigned(paraloc^.next) then
      InternalError(2013020101);

    case paraloc^.loc of
      LOC_REGISTER:
        begin
          if ((ioffset>=simm16lo) and (ioffset<=simm16hi)) then
            cg.a_op_const_reg(list,OP_SUB, paraloc^.size,ioffset,paraloc^.register)
          else
            begin
              cg.a_load_const_reg(list, paraloc^.size, ioffset, NR_R1);
              cg.a_op_reg_reg(list, OP_SUB, paraloc^.size, NR_R1, paraloc^.register);
            end;
        end;
    else
      internalerror(2010103102);
    end;

    if IsVirtual then
    begin
      { load VMT pointer }
      reference_reset_base(href,voidpointertype,paraloc^.register,0,ctempposinvalid,sizeof(aint),[]);
      list.concat(taicpu.op_reg_ref(A_LW,NR_VMT,href));

      if (procdef.extnumber=$ffff) then
        Internalerror(200006139);

      { TODO: case of large VMT is not handled }
      { We have no reason not to use $t9 even in non-PIC mode. }
      reference_reset_base(href, voidpointertype, NR_VMT, tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber), ctempposinvalid, sizeof(aint), []);
      list.concat(taicpu.op_reg_ref(A_LW,NR_PIC_FUNC,href));
      list.concat(taicpu.op_reg(A_JR, NR_PIC_FUNC));
    end
    else if not (cs_create_pic in current_settings.moduleswitches) then
      list.concat(taicpu.op_sym(A_J,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)))
    else
      begin
        { GAS does not expand "J symbol" into PIC sequence }
        reference_reset_symbol(href,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION),0,sizeof(pint),[]);
        href.base:=NR_GP;
        href.refaddr:=addr_pic_call16;
        list.concat(taicpu.op_reg_ref(A_LW,NR_PIC_FUNC,href));
        list.concat(taicpu.op_reg(A_JR,NR_PIC_FUNC));
      end;
    { Delay slot }
    list.Concat(TAiCpu.Op_none(A_NOP));

    List.concat(Tai_symbol_end.Createname(labelname));
  end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgmips.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgmips;
end.
