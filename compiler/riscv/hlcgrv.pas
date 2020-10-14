{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines high-level code generator support shared by
    riscv32 and riscv64

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
unit hlcgrv;

{$i fpcdefs.inc}

interface

uses
  globals,
  aasmdata,
  symtype,symdef,
  cgbase,cgutils,hlcgobj,hlcg2ll, parabase;

type

  { thlcgriscv }

  thlcgriscv = class(thlcg2ll)
   protected
    procedure a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister); override;
   public
    procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    procedure g_external_wrapper(list: TAsmList; procdef: tprocdef; const wrappername, externalname: string; global: boolean); override;
  end;

implementation

  uses
    verbose,
    systems,fmodule,
    symconst, symsym,
    aasmbase,aasmtai,aasmcpu,
    cpubase,globtype,
    procinfo,cpupi,cgobj,cgrv,
    defutil;

{ thlcgriscv }

  procedure thlcgriscv.a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister);
    var
      fromsreg, tosreg: tsubsetregister;
      restbits: byte;
    begin
      { the code below is only valid for big endian }
      if target_info.endian=endian_little then
        begin
         inherited;
         exit
        end;
      restbits:=(sref.bitlen-(loadbitsize-sref.startbit));
      if is_signed(subsetsize) then
        begin
         { sign extend }
         a_op_const_reg(list,OP_SHL,osuinttype,AIntBits-loadbitsize+sref.startbit,valuereg);
         a_op_const_reg(list,OP_SAR,osuinttype,AIntBits-sref.bitlen,valuereg);
        end
      else
        begin
          a_op_const_reg(list,OP_SHL,osuinttype,restbits,valuereg);
          { mask other bits }
          if (sref.bitlen<>AIntBits) then
            a_op_const_reg(list,OP_AND,osuinttype,(aword(1) shl sref.bitlen)-1,valuereg);
        end;
      { use subsetreg routine, it may have been overridden with an optimized version }
      fromsreg.subsetreg:=extra_value_reg;
      fromsreg.subsetregsize:=OS_INT;
      { subsetregs always count bits from right to left }
      fromsreg.startbit:=loadbitsize-restbits;
      fromsreg.bitlen:=restbits;

      tosreg.subsetreg:=valuereg;
      tosreg.subsetregsize:=OS_INT;
      tosreg.startbit:=0;
      tosreg.bitlen:=restbits;

      a_load_subsetreg_subsetreg(list,subsetsize,subsetsize,fromsreg,tosreg);
    end;


  procedure thlcgriscv.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    procedure loadvmttor12;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
      begin
        reference_reset_base(href,voidpointertype,NR_X10,0,ctempposinvalid,sizeof(pint),[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_X5);
      end;


    procedure op_onr12methodaddr;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(2000061304);

        reference_reset_base(href,voidpointertype,NR_X5,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid, sizeof(pint),[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_X5);

        list.concat(taicpu.op_reg_reg(A_JALR,NR_X0,NR_X5));
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
        Internalerror(200006137);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(200006138);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(200109191);

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
        internalerror(2010103101);
      paraloc:=tparavarsym(hsym).paraloc[callerside].location;
      if assigned(paraloc^.next) then
        InternalError(2013020101);

      case paraloc^.loc of
        LOC_REGISTER:
          begin
            if is_imm12(ioffset) then
              cg.a_op_const_reg(list,OP_SUB, paraloc^.size,ioffset,paraloc^.register)
            else
              begin
                cg.a_load_const_reg(list, paraloc^.size, ioffset, NR_X6);
                cg.a_op_reg_reg(list, OP_SUB, paraloc^.size, NR_X6, paraloc^.register);
              end;
          end;
      else
        internalerror(2010103102);
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
          reference_reset_symbol(href,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION),0,0,[]);

          tmpreg:=NR_X5;

          current_asmdata.getjumplabel(l);

          cg.a_label(list, l);

          href.refaddr:=addr_pcrel_hi20;
          list.concat(taicpu.op_reg_ref(A_AUIPC,tmpreg,href));

          reference_reset_symbol(href,l,0,0,[]);
          href.refaddr:=addr_pcrel_lo12;
          list.concat(taicpu.op_reg_reg_ref(A_JALR,NR_X0,tmpreg,href));

          //list.concat(taicpu.op_reg_sym(A_JAL,NR_X0,current_asmdata.RefAsmSymbol(procdef.mangledname)));
        end;
      list.concatlist(current_procinfo.aktlocaldata);

      current_procinfo.Free;
      current_procinfo:=nil;

      list.concat(Tai_symbol_end.Createname(labelname));
    end;

  procedure thlcgriscv.g_external_wrapper(list: TAsmList; procdef: tprocdef; const wrappername, externalname: string; global: boolean);
    var
      sym: tasmsymbol;   
      ai: taicpu;
      href: treference;
      tmpreg: TRegister;
      l: TAsmLabel;
    begin
      maybe_new_object_file(list);
      new_section(list,sec_code,wrappername,target_info.alignment.procalign);
      if global then
        begin
          sym:=current_asmdata.DefineAsmSymbol(wrappername,AB_GLOBAL,AT_FUNCTION,procdef);
          list.concat(Tai_symbol.Create_global(sym,0));
        end
      else
        begin
          sym:=current_asmdata.DefineAsmSymbol(wrappername,AB_LOCAL,AT_FUNCTION,procdef);
          list.concat(Tai_symbol.Create(sym,0));
        end;

      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(externalname,AT_FUNCTION),0,0,[]);

      tmpreg:=NR_X5;

      current_asmdata.getjumplabel(l);
      a_label(list,l);

      href.refaddr:=addr_pcrel_hi20;
      list.concat(taicpu.op_reg_ref(A_AUIPC,tmpreg,href));
      reference_reset_symbol(href,l,0,0,[]);
      href.refaddr:=addr_pcrel_lo12;
      ai:=taicpu.op_reg_reg_ref(A_JALR,NR_X0,tmpreg,href);
      ai.is_jmp:=true;
      list.concat(ai);

      list.concat(Tai_symbol_end.Create(sym));
    end;


  procedure create_hlcodegen_cpu;
    begin
      hlcg:=thlcgriscv.create;
//      create_codegen;
    end;


begin
  chlcgobj:=thlcgriscv;
  create_hlcodegen:=@create_hlcodegen_cpu;
end.

