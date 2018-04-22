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
    aasmdata,
    symdef,
    hlcg2ll;

  type
    tbasehlcgarm = class(thlcg2ll)
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    end;

    tarmhlcgcpu = class(tbasehlcgarm)
    end;

    tthumbhlcgcpu = class(tbasehlcgarm)
      procedure a_jmp_external_name(list: TAsmList; const externalname: TSymStr); override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globals,verbose,
    procinfo,fmodule,
    symconst,
    aasmbase,aasmtai,aasmcpu, cpuinfo,
    hlcgobj,
    cgbase, cgutils, cpubase, cgobj, cgcpu;

  procedure tbasehlcgarm.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);

    procedure loadvmttor12;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
      begin
        reference_reset_base(href,voidpointertype,NR_R0,0,ctempposinvalid,sizeof(pint),[]);
        if GenerateThumbCode then
          begin
            if (href.offset in [0..124]) and ((href.offset mod 4)=0) then
              begin
                list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
                cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R0);
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
                list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
              end
            else
              begin
                list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0,RS_R1]));
                { create consts entry }
                reference_reset(tmpref,4,[]);
                current_asmdata.getjumplabel(l);
                current_procinfo.aktlocaldata.Concat(tai_align.Create(4));
                cg.a_label(current_procinfo.aktlocaldata,l);
                tmpref.symboldata:=current_procinfo.aktlocaldata.last;
                current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(href.offset));
                tmpref.symbol:=l;
                tmpref.base:=NR_PC;
                list.concat(taicpu.op_reg_ref(A_LDR,NR_R1,tmpref));
                href.offset:=0;
                href.index:=NR_R1;
                cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R0);
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
                list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0,RS_R1]));
              end;
          end
        else
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);
      end;


    procedure op_onr12methodaddr;
      var
        tmpref,
        href : treference;
        l : TAsmLabel;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(200006139);
        if GenerateThumbCode then
          begin
            reference_reset_base(href,voidpointertype,NR_R0,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,sizeof(pint),[]);
            if (href.offset in [0..124]) and ((href.offset mod 4)=0) then
              begin
                list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R0,NR_R12));
                cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R0);
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
                list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
              end
            else
              begin
                list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0,RS_R1]));
                { create consts entry }
                reference_reset(tmpref,4,[]);
                current_asmdata.getjumplabel(l);
                current_procinfo.aktlocaldata.Concat(tai_align.Create(4));
                cg.a_label(current_procinfo.aktlocaldata,l);
                tmpref.symboldata:=current_procinfo.aktlocaldata.last;
                current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(href.offset));
                tmpref.symbol:=l;
                tmpref.base:=NR_PC;
                list.concat(taicpu.op_reg_ref(A_LDR,NR_R1,tmpref));
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R0,NR_R12));
                href.offset:=0;
                href.base:=NR_R0;
                href.index:=NR_R1;
                cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R0);
                list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
                list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0,RS_R1]));
              end;
          end
        else
          begin
            reference_reset_base(href,voidpointertype,NR_R12,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,sizeof(pint),[]);
            cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R12);
          end;
        if not(CPUARM_HAS_BX in cpu_capabilities[current_settings.cputype]) then
          list.concat(taicpu.op_reg_reg(A_MOV,NR_PC,NR_R12))
        else
          list.concat(taicpu.op_reg(A_BX,NR_R12));
      end;

    var
      make_global : boolean;
      tmpref : treference;
      l : TAsmLabel;
    begin
      if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
        Internalerror(200006137);
      if not assigned(procdef.struct) or
         (procdef.procoptions*[po_classmethod, po_staticmethod,
           po_methodpointer, po_interrupt, po_iocheck]<>[]) then
        Internalerror(200006138);
      if procdef.owner.symtabletype<>ObjectSymtable then
        Internalerror(200109191);

        if GenerateThumbCode or GenerateThumb2Code then
          list.concat(tai_directive.Create(asd_thumb_func,''));

      make_global:=false;
      if (not current_module.is_unit) or
         create_smartlink or
         (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
        make_global:=true;

      if make_global then
        list.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,procdef))
      else
        list.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0,procdef));

      { the wrapper might need aktlocaldata for the additional data to
        load the constant }
      current_procinfo:=cprocinfo.create(nil);

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      { case 4 }
      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          loadvmttor12;
          op_onr12methodaddr;
        end
      { case 0 }
      else if GenerateThumbCode then
        begin
          { bl cannot be used here because it destroys lr }

          list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));

          { create consts entry }
          reference_reset(tmpref,4,[]);
          current_asmdata.getjumplabel(l);
          current_procinfo.aktlocaldata.Concat(tai_align.Create(4));
          cg.a_label(current_procinfo.aktlocaldata,l);
          tmpref.symboldata:=current_procinfo.aktlocaldata.last;
          current_procinfo.aktlocaldata.concat(tai_const.Create_sym(current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)));

          tmpref.symbol:=l;
          tmpref.base:=NR_PC;
          cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,tmpref,NR_R0);
          list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
          list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
          list.concat(taicpu.op_reg(A_BX,NR_R12));
        end
      else
        list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION)));
      list.concatlist(current_procinfo.aktlocaldata);

      current_procinfo.Free;
      current_procinfo:=nil;

      list.concat(Tai_symbol_end.Createname(labelname));
    end;


  { tthumbhlcgcpu }

  procedure tthumbhlcgcpu.a_jmp_external_name(list: TAsmList; const externalname: TSymStr);
    var
      tmpref : treference;
      l : tasmlabel;
    begin
      { there is no branch instruction on thumb which allows big distances and which leaves LR as it is
        and which allows to switch the instruction set }

      { create const entry }
      reference_reset(tmpref,4,[]);
      current_asmdata.getjumplabel(l);
      tmpref.symbol:=l;
      tmpref.base:=NR_PC;
      list.concat(taicpu.op_regset(A_PUSH,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
      list.concat(taicpu.op_reg_ref(A_LDR,NR_R0,tmpref));
      list.concat(taicpu.op_reg_reg(A_MOV,NR_R12,NR_R0));
      list.concat(taicpu.op_regset(A_POP,R_INTREGISTER,R_SUBWHOLE,[RS_R0]));
      list.concat(taicpu.op_reg(A_BX,NR_R12));

      { append const entry }
      list.Concat(tai_align.Create(4));
      list.Concat(tai_label.create(l));
      list.concat(tai_const.Create_sym(current_asmdata.RefAsmSymbol(externalname,AT_FUNCTION)));
    end;



  procedure create_hlcodegen;
    begin
      if GenerateThumbCode then
        hlcg:=tthumbhlcgcpu.create
      else
        hlcg:=tarmhlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=tbasehlcgarm;
end.
