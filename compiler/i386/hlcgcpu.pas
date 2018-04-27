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
    symtype,symdef,parabase,
    cgbase,cgutils,
    hlcgobj, hlcgx86;


  type
    thlcgcpu = class(thlcgx86)
     protected
      procedure gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint); override;
     public
      function a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara; override;
      procedure g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister); override;
      procedure g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation); override;
      procedure g_exception_reason_save(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const href: treference); override;
      procedure g_exception_reason_save_const(list: TAsmList; size: tdef; a: tcgint; const href: treference); override;
      procedure g_exception_reason_load(list: TAsmList; fromsize, tosize: tdef; const href: treference; reg: tregister); override;
      procedure g_exception_reason_discard(list: TAsmList; size: tdef; href: treference); override;

      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    globals, procinfo,
    verbose,
    fmodule,systems,
    aasmbase,aasmtai,
    paramgr,
    symconst,symsym,defutil,
    cpubase,aasmcpu,tgobj,cgobj,cgx86,cgcpu;

  { thlcgcpu }

  procedure thlcgcpu.gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint);
    var
      locsize : tcgsize;
      tmploc : tlocation;
      href   : treference;
      stacksize   : longint;
    begin
      if not(l.size in [OS_32,OS_S32,OS_64,OS_S64,OS_128,OS_S128]) then
        locsize:=l.size
      else
        locsize:=int_float_cgsize(tcgsize2size[l.size]);
      case l.loc of
        LOC_FPUREGISTER,
        LOC_CFPUREGISTER:
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    begin
                      cg.g_stackpointer_alloc(list,stacksize);
                      reference_reset_base(href,voidstackpointertype,NR_STACK_POINTER_REG,0,ctempposinvalid,voidstackpointertype.size,[]);
                    end
                  else
                    reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,ctempposinvalid,cgpara.alignment,[]);
                  cg.a_loadfpu_reg_ref(list,locsize,locsize,l.register,href);
                end;
              LOC_FPUREGISTER:
                begin
                  cg.a_loadfpu_reg_reg(list,locsize,cgpara.location^.size,l.register,cgpara.location^.register);
                end;
              { can happen if a record with only 1 "single field" is
                returned in a floating point register and then is directly
                passed to a regcall parameter }
              LOC_REGISTER:
                begin
                  tmploc:=l;
                  location_force_mem(list,tmploc,size);
                  case locsize of
                    OS_F32:
                      tmploc.size:=OS_32;
                    OS_F64:
                      tmploc.size:=OS_64;
                    else
                      internalerror(2010053116);
                  end;
                  cg.a_load_loc_cgpara(list,tmploc,cgpara);
                  location_freetemp(list,tmploc);
                end
              else
                internalerror(2010053003);
            end;
          end;
        LOC_MMREGISTER,
        LOC_CMMREGISTER:
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  { can't use TCGSize2Size[l.size], because the size of an
                    80 bit extended parameter can be either 10 or 12 bytes }
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    begin
                      cg.g_stackpointer_alloc(list,stacksize);
                      reference_reset_base(href,voidstackpointertype,NR_STACK_POINTER_REG,0,ctempposinvalid,voidstackpointertype.size,[]);
                    end
                  else
                    reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,ctempposinvalid,cgpara.alignment,[]);
                  cg.a_loadmm_reg_ref(list,locsize,locsize,l.register,href,mms_movescalar);
                end;
              LOC_FPUREGISTER:
                begin
                  tmploc:=l;
                  location_force_mem(list,tmploc,size);
                  cg.a_loadfpu_ref_cgpara(list,tmploc.size,tmploc.reference,cgpara);
                  location_freetemp(list,tmploc);
                end;
              else
                internalerror(2010053004);
            end;
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE :
          begin
            case cgpara.location^.loc of
              LOC_REFERENCE:
                begin
                  stacksize:=align(locintsize,cgpara.alignment);
                  if (not paramanager.use_fixed_stack) and
                     (cgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                    cg.a_load_ref_cgpara(list,locsize,l.reference,cgpara)
                  else
                    begin
                      reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,ctempposinvalid,cgpara.alignment,[]);
                      cg.g_concatcopy(list,l.reference,href,stacksize);
                    end;
                end;
              LOC_FPUREGISTER:
                begin
                  cg.a_loadfpu_ref_cgpara(list,locsize,l.reference,cgpara);
                end;
              else
                internalerror(2010053005);
            end;
          end;
        else
          internalerror(2002042430);
      end;
    end;


  function thlcgcpu.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;
  var
    need_got_load: boolean;
  begin
    { Load GOT address to EBX before calling an external function.
      It is needed because GOT stubs for external function calls
      generated by a linker expect EBX as a GOT register. }
    need_got_load:=not (target_info.system in systems_darwin) and
                   (cs_create_pic in current_settings.moduleswitches) and
                   (tf_pic_uses_got in target_info.flags) and
                   (po_external in pd.procoptions);
    if need_got_load then
      begin
        { Alloc EBX }
        getcpuregister(list, NR_PIC_OFFSET_REG);
        list.concat(taicpu.op_reg_reg(A_MOV,S_L,current_procinfo.got,NR_PIC_OFFSET_REG));
      end;
    Result:=inherited a_call_name(list, pd, s, paras, forceresdef, weak);
    { Free EBX }
    if need_got_load then
      ungetcpuregister(list, NR_PIC_OFFSET_REG);
  end;


  procedure thlcgcpu.g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg386(cg).g_copyvaluepara_openarray(list,ref,lenloc,arrdef.elesize,destreg);
    end;


  procedure thlcgcpu.g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg386(cg).g_releasevaluepara_openarray(list,l);
    end;


  procedure thlcgcpu.g_exception_reason_save(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const href: treference);
    begin
      if not paramanager.use_fixed_stack then
        list.concat(Taicpu.op_reg(A_PUSH,tcgsize2opsize[def_cgsize(tosize)],reg))
      else
        inherited
    end;


  procedure thlcgcpu.g_exception_reason_save_const(list: TAsmList; size: tdef; a: tcgint; const href: treference);
    begin
      if not paramanager.use_fixed_stack then
        list.concat(Taicpu.op_const(A_PUSH,tcgsize2opsize[def_cgsize(size)],a))
      else
        inherited;
    end;


  procedure thlcgcpu.g_exception_reason_load(list: TAsmList; fromsize, tosize: tdef; const href: treference; reg: tregister);
    begin
      if not paramanager.use_fixed_stack then
        list.concat(Taicpu.op_reg(A_POP,tcgsize2opsize[def_cgsize(tosize)],reg))
      else
        inherited;
    end;


  procedure thlcgcpu.g_exception_reason_discard(list: TAsmList; size: tdef; href: treference);
    begin
      if not paramanager.use_fixed_stack then
        begin
          getcpuregister(list,NR_FUNCTION_RESULT_REG);
          list.concat(Taicpu.op_reg(A_POP,tcgsize2opsize[def_cgsize(size)],NR_FUNCTION_RESULT_REG));
          ungetcpuregister(list,NR_FUNCTION_RESULT_REG);
        end;
    end;


  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    {
    possible calling conventions:
                  default stdcall cdecl pascal register
    default(0):      OK     OK    OK     OK       OK
    virtual(1):      OK     OK    OK     OK       OK(2 or 1)

    (0):
        set self parameter to correct value
        jmp mangledname

    (1): The wrapper code use %ecx to reach the virtual method address
         set self to correct value
         move self,%eax
         mov  0(%eax),%ecx ; load vmt
         jmp  vmtoffs(%ecx) ; method offs

    (2): Virtual use values pushed on stack to reach the method address
         so the following code be generated:
         set self to correct value
         push %ebx ; allocate space for function address
         push %eax
         mov  self,%eax
         mov  0(%eax),%eax ; load vmt
         mov  vmtoffs(%eax),eax ; method offs
         mov  %eax,4(%esp)
         pop  %eax
         ret  0; jmp the address

    }

    { returns whether ECX is used (either as a parameter or is nonvolatile and shouldn't be changed) }
    function is_ecx_used: boolean;
      var
        i: Integer;
        hp: tparavarsym;
        paraloc: PCGParaLocation;
      begin
        if not (RS_ECX in paramanager.get_volatile_registers_int(procdef.proccalloption)) then
          exit(true);
        for i:=0 to procdef.paras.count-1 do
         begin
           hp:=tparavarsym(procdef.paras[i]);
           procdef.init_paraloc_info(calleeside);
           paraloc:=hp.paraloc[calleeside].Location;
           while paraloc<>nil do
             begin
               if (paraloc^.Loc=LOC_REGISTER) and (getsupreg(paraloc^.register)=RS_ECX) then
                 exit(true);
               paraloc:=paraloc^.Next;
             end;
         end;
        Result:=false;
      end;

    procedure getselftoeax(offs: longint);
      var
        href : treference;
        selfoffsetfromsp : longint;
      begin
        { mov offset(%esp),%eax }
        if (procdef.proccalloption<>pocall_register) then
          begin
            { framepointer is pushed for nested procs }
            if procdef.parast.symtablelevel>normal_function_level then
              selfoffsetfromsp:=2*sizeof(aint)
            else
              selfoffsetfromsp:=sizeof(aint);
            reference_reset_base(href,voidstackpointertype,NR_ESP,selfoffsetfromsp+offs,ctempposinvalid,4,[]);
            cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_EAX);
          end;
      end;

    procedure loadvmtto(reg: tregister);
      var
        href : treference;
      begin
        { mov  0(%eax),%reg ; load vmt}
        reference_reset_base(href,voidpointertype,NR_EAX,0,ctempposinvalid,4,[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,reg);
      end;

    procedure op_onregmethodaddr(op: TAsmOp; reg: tregister);
      var
        href : treference;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(200006139);
        { call/jmp  vmtoffs(%reg) ; method offs }
        reference_reset_base(href,voidpointertype,reg,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,4,[]);
        list.concat(taicpu.op_ref(op,S_L,href));
      end;


    procedure loadmethodoffstoeax;
      var
        href : treference;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(200006139);
        { mov vmtoffs(%eax),%eax ; method offs }
        reference_reset_base(href,voidpointertype,NR_EAX,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),ctempposinvalid,4,[]);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_EAX);
      end;


    var
      lab : tasmsymbol;
      make_global : boolean;
      href : treference;
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
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0,procdef))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0,procdef));

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          if (procdef.proccalloption=pocall_register) and is_ecx_used then
            begin
              { case 2 }
              list.concat(taicpu.op_reg(A_PUSH,S_L,NR_EBX)); { allocate space for address}
              list.concat(taicpu.op_reg(A_PUSH,S_L,NR_EAX));
              getselftoeax(8);
              loadvmtto(NR_EAX);
              loadmethodoffstoeax;
              { mov %eax,4(%esp) }
              reference_reset_base(href,voidstackpointertype,NR_ESP,4,ctempposinvalid,4,[]);
              list.concat(taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
              { pop  %eax }
              list.concat(taicpu.op_reg(A_POP,S_L,NR_EAX));
              { ret  ; jump to the address }
              list.concat(taicpu.op_none(A_RET,S_L));
            end
          else
            begin
              { case 1 }
              getselftoeax(0);
              loadvmtto(NR_ECX);
              op_onregmethodaddr(A_JMP,NR_ECX);
            end;
        end
      { case 0 }
      else
        begin
          if (target_info.system <> system_i386_darwin) then
            begin
              lab:=current_asmdata.RefAsmSymbol(procdef.mangledname,AT_FUNCTION);
              list.concat(taicpu.op_sym(A_JMP,S_NO,lab))
            end
          else
            list.concat(taicpu.op_sym(A_JMP,S_NO,tcgx86(cg).get_darwin_call_stub(procdef.mangledname,false)))
        end;

      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;



begin
  chlcgobj:=thlcgcpu;
end.
