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
    globals,globtype,
    aasmdata,
    symtype,symdef,parabase,
    cgbase,cgutils,
    hlcgobj, hlcgx86;


  type

    { thlcgcpu }

    thlcgcpu = class(thlcgx86)
     private
      { checks whether the type needs special methodptr-like handling, when stored
        in a LOC_REGISTER location. This applies to the following types:
          - i8086 method pointers (incl. 6-byte mixed near + far),
          - 6-byte records (only in the medium and compact memory model are these
              loaded in a register)
          - nested proc ptrs
        When stored in a LOC_REGISTER tlocation, these types use both register
        and registerhi with the following sizes:

        register   - cgsize = int_cgsize(voidcodepointertype.size)
        registerhi - cgsize = int_cgsize(voidpointertype.size) or int_cgsize(parentfpvoidpointertype.size)
                              (check d.size to determine which one of the two)
        }
      function is_methodptr_like_type(d:tdef): boolean;

      { 4-byte records in registers need special handling as well. A record may
        be located in registerhi:register if it was converted from a procvar or
        in GetNextReg(register):register if it was converted from a longint.
        We can tell between the two by checking whether registerhi has been set. }
      function is_fourbyterecord(d:tdef): boolean;
     protected
      procedure gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint); override;
     public
      function getaddressregister(list:TAsmList;size:tdef):Tregister;override;

      procedure reference_reset_base(var ref: treference; regsize: tdef; reg: tregister; offset, alignment: longint); override;

      function a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;override;

      procedure a_load_loc_ref(list : TAsmList;fromsize, tosize: tdef; const loc: tlocation; const ref : treference);override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      procedure g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister); override;
      procedure g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation); override;

      procedure g_exception_reason_save(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const href: treference); override;
      procedure g_exception_reason_save_const(list: TAsmList; size: tdef; a: tcgint; const href: treference); override;
      procedure g_exception_reason_load(list: TAsmList; fromsize, tosize: tdef; const href: treference; reg: tregister); override;
      procedure g_exception_reason_discard(list: TAsmList; size: tdef; href: treference); override;

      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;

      procedure location_force_mem(list:TAsmList;var l:tlocation;size:tdef);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,
    paramgr,
    aasmbase,aasmtai,
    cpubase,cpuinfo,tgobj,cgobj,cgx86,cgcpu,
    defutil,
    symconst,symcpu,
    procinfo,fmodule,
    aasmcpu;

  { thlcgcpu }

  function thlcgcpu.is_methodptr_like_type(d: tdef): boolean;
    var
      is_sixbyterecord,is_methodptr,is_nestedprocptr: Boolean;
    begin
      is_sixbyterecord:=(d.typ=recorddef) and (d.size=6);
      is_methodptr:=(d.typ=procvardef)
        and (po_methodpointer in tprocvardef(d).procoptions)
        and not(po_addressonly in tprocvardef(d).procoptions);
      is_nestedprocptr:=(d.typ=procvardef)
        and is_nested_pd(tprocvardef(d))
        and not(po_addressonly in tprocvardef(d).procoptions);
      result:=is_sixbyterecord or is_methodptr or is_nestedprocptr;
    end;


  function thlcgcpu.is_fourbyterecord(d: tdef): boolean;
    begin
      result:=(d.typ=recorddef) and (d.size=4);
    end;


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
                      reference_reset_base(href,voidstackpointertype,NR_STACK_POINTER_REG,0,voidstackpointertype.size);
                    end
                  else
                    reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
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
                      reference_reset_base(href,voidstackpointertype,NR_STACK_POINTER_REG,0,voidstackpointertype.size);
                    end
                  else
                    reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
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
                      reference_reset_base(href,voidstackpointertype,cgpara.location^.reference.index,cgpara.location^.reference.offset,cgpara.alignment);
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


  function thlcgcpu.getaddressregister(list: TAsmList; size: tdef): Tregister;
    begin
      { implicit pointer types on i8086 follow the default data pointer size for
        the current memory model }
      if is_implicit_pointer_object_type(size) or is_implicit_array_pointer(size) then
        size:=voidpointertype;

      if is_farpointer(size) or is_hugepointer(size) then
        Result:=cg.getintregister(list,OS_32)
      else
        Result:=cg.getintregister(list,OS_16);
    end;


  procedure thlcgcpu.reference_reset_base(var ref: treference; regsize: tdef;
    reg: tregister; offset, alignment: longint);
    begin
      inherited reference_reset_base(ref, regsize, reg, offset, alignment);

      { implicit pointer types on i8086 follow the default data pointer size for
        the current memory model }
      if is_implicit_pointer_object_type(regsize) or is_implicit_array_pointer(regsize) then
        regsize:=voidpointertype;

      if regsize.typ=pointerdef then
        case tcpupointerdef(regsize).x86pointertyp of
          x86pt_near:
            ;
          x86pt_near_cs:
            ref.segment:=NR_CS;
          x86pt_near_ds:
            ref.segment:=NR_DS;
          x86pt_near_ss:
            ref.segment:=NR_SS;
          x86pt_near_es:
            ref.segment:=NR_ES;
          x86pt_near_fs:
            ref.segment:=NR_FS;
          x86pt_near_gs:
            ref.segment:=NR_GS;
          x86pt_far,
          x86pt_huge:
            if reg<>NR_NO then
              ref.segment:=GetNextReg(reg);
        end;
    end;


  function thlcgcpu.a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;
    begin
      if is_proc_far(pd) then
        begin
          { far calls to the same module (in $HUGECODE off mode) can be optimized
            to push cs + call near, because they are in the same segment }
          if not (cs_huge_code in current_settings.moduleswitches) and
             pd.owner.iscurrentunit and not (po_external in pd.procoptions) then
            begin
              list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_CS));
              tcg8086(cg).a_call_name_near(list,s,weak);
            end
          else
            tcg8086(cg).a_call_name_far(list,s,weak);
        end
      else
        tcg8086(cg).a_call_name_near(list,s,weak);
      result:=get_call_result_cgpara(pd,forceresdef);
    end;


  procedure thlcgcpu.a_load_loc_ref(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const ref: treference);
    var
      tmpref: treference;
    begin
      if is_methodptr_like_type(tosize) and (loc.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tmpref:=ref;
          a_load_reg_ref(list,voidcodepointertype,voidcodepointertype,loc.register,tmpref);
          inc(tmpref.offset,voidcodepointertype.size);
          { the second part could be either self or parentfp }
          if tosize.size=(voidcodepointertype.size+voidpointertype.size) then
            a_load_reg_ref(list,voidpointertype,voidpointertype,loc.registerhi,tmpref)
          else if tosize.size=(voidcodepointertype.size+parentfpvoidpointertype.size) then
            a_load_reg_ref(list,parentfpvoidpointertype,parentfpvoidpointertype,loc.registerhi,tmpref)
          else
            internalerror(2014052201);
        end
      else if is_fourbyterecord(tosize) and (loc.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tmpref:=ref;
          cg.a_load_reg_ref(list,OS_16,OS_16,loc.register,tmpref);
          inc(tmpref.offset,2);
          if loc.registerhi<>tregister(0) then
            cg.a_load_reg_ref(list,OS_16,OS_16,loc.registerhi,tmpref)
          else
            cg.a_load_reg_ref(list,OS_16,OS_16,GetNextReg(loc.register),tmpref);
        end
      else
        inherited a_load_loc_ref(list, fromsize, tosize, loc, ref);
    end;


  procedure thlcgcpu.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    var
      tmpref,segref: treference;
    begin
      { step 1: call the x86 low level code generator to handle the offset;
        we set the segment to NR_NO to disable the i8086 segment handling code
        in the low level cg (which can be removed, once all calls to
        a_loadaddr_ref_reg go through the high level code generator) }
      tmpref:=ref;
      tmpref.segment:=NR_NO;
      cg.a_loadaddr_ref_reg(list, tmpref, r);

      { step 2: if destination is a far pointer, we have to pass a segment as well }
      if is_farpointer(tosize) or is_hugepointer(tosize) or is_farprocvar(tosize) or
         ((tosize.typ=classrefdef) and (tosize.size=4)) then
        begin
          { if a segment register is specified in ref, we use that }
          if ref.segment<>NR_NO then
            begin
              if is_segment_reg(ref.segment) then
                list.concat(Taicpu.op_reg_reg(A_MOV,S_W,ref.segment,GetNextReg(r)))
              else
                cg.a_load_reg_reg(list,OS_16,OS_16,ref.segment,GetNextReg(r));
            end
          { references relative to a symbol use the segment of the symbol,
            which can be obtained by the SEG directive }
          else if assigned(ref.symbol) then
            begin
              reference_reset_symbol(segref,ref.symbol,0,0);
              segref.refaddr:=addr_seg;
              cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,segref,GetNextReg(r));
            end
          else if ref.base=NR_BP then
            list.concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_SS,GetNextReg(r)))
          else
            internalerror(2014032801);
        end;
    end;


  procedure thlcgcpu.g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg8086(cg).g_copyvaluepara_openarray(list,ref,lenloc,arrdef.elesize,destreg);
    end;


  procedure thlcgcpu.g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation);
    begin
      if paramanager.use_fixed_stack then
        begin
          inherited;
          exit;
        end;
      tcg8086(cg).g_releasevaluepara_openarray(list,l);
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
        tcg8086(cg).push_const(list,def_cgsize(size),a)
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
    virtual(1):      OK     OK    OK     OK       OK(2)

    (0):
        set self parameter to correct value
        jmp mangledname

    (1): The wrapper code use %eax to reach the virtual method address
         set self to correct value
         move self,%bx
         mov  0(%bx),%bx ; load vmt
         jmp  vmtoffs(%bx) ; method offs

    (2): Virtual use values pushed on stack to reach the method address
         so the following code be generated:
         set self to correct value
         push %bx ; allocate space for function address
         push %bx
         push %di
         mov  self,%bx
         mov  0(%bx),%bx ; load vmt
         mov  vmtoffs(%bx),bx ; method offs
         mov  %sp,%di
         mov  %bx,4(%di)
         pop  %di
         pop  %bx
         ret  0; jmp the address

    }

    procedure getselftobx(offs: longint);
      var
        href : treference;
        selfoffsetfromsp : longint;
      begin
        { "mov offset(%sp),%bx" }
        if (procdef.proccalloption<>pocall_register) then
          begin
            list.concat(taicpu.op_reg(A_PUSH,S_W,NR_DI));
            { framepointer is pushed for nested procs }
            if procdef.parast.symtablelevel>normal_function_level then
              selfoffsetfromsp:=2*sizeof(aint)
            else
              selfoffsetfromsp:=sizeof(aint);
            if current_settings.x86memorymodel in x86_far_code_models then
              inc(selfoffsetfromsp,2);
            list.concat(taicpu.op_reg_reg(A_mov,S_W,NR_SP,NR_DI));
            reference_reset_base(href,voidnearpointertype,NR_DI,selfoffsetfromsp+offs+2,2);
            if not segment_regs_equal(NR_SS,NR_DS) then
              href.segment:=NR_SS;
            if current_settings.x86memorymodel in x86_near_data_models then
              cg.a_load_ref_reg(list,OS_16,OS_16,href,NR_BX)
            else
              list.concat(taicpu.op_ref_reg(A_LES,S_W,href,NR_BX));
            list.concat(taicpu.op_reg(A_POP,S_W,NR_DI));
          end
        else
          cg.a_load_reg_reg(list,OS_ADDR,OS_ADDR,NR_BX,NR_BX);
      end;


    procedure loadvmttobx;
      var
        href : treference;
      begin
        { mov  0(%bx),%bx ; load vmt}
        if current_settings.x86memorymodel in x86_near_data_models then
          begin
            reference_reset_base(href,voidnearpointertype,NR_BX,0,2);
            cg.a_load_ref_reg(list,OS_16,OS_16,href,NR_BX);
          end
        else
          begin
            reference_reset_base(href,voidnearpointertype,NR_BX,0,2);
            href.segment:=NR_ES;
            list.concat(taicpu.op_ref_reg(A_LES,S_W,href,NR_BX));
          end;
      end;


    procedure loadmethodoffstobx;
      var
        href : treference;
        srcseg: TRegister;
      begin
        if (procdef.extnumber=$ffff) then
          Internalerror(200006139);
        if current_settings.x86memorymodel in x86_far_data_models then
          srcseg:=NR_ES
        else
          srcseg:=NR_NO;
        if current_settings.x86memorymodel in x86_far_code_models then
          begin
            { mov vmtseg(%bx),%si ; method seg }
            reference_reset_base(href,voidnearpointertype,NR_BX,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber)+2,2);
            href.segment:=srcseg;
            cg.a_load_ref_reg(list,OS_16,OS_16,href,NR_SI);
          end;
        { mov vmtoffs(%bx),%bx ; method offs }
        reference_reset_base(href,voidnearpointertype,NR_BX,tobjectdef(procdef.struct).vmtmethodoffset(procdef.extnumber),2);
        href.segment:=srcseg;
        cg.a_load_ref_reg(list,OS_16,OS_16,href,NR_BX);
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
        List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
      else
        List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

      { set param1 interface to self  }
      g_adjust_self_value(list,procdef,ioffset);

      if (po_virtualmethod in procdef.procoptions) and
          not is_objectpascal_helper(procdef.struct) then
        begin
          { case 1 & case 2 }
          list.concat(taicpu.op_reg(A_PUSH,S_W,NR_BX)); { allocate space for address}
          if current_settings.x86memorymodel in x86_far_code_models then
            list.concat(taicpu.op_reg(A_PUSH,S_W,NR_BX));
          list.concat(taicpu.op_reg(A_PUSH,S_W,NR_BX));
          list.concat(taicpu.op_reg(A_PUSH,S_W,NR_DI));
          if current_settings.x86memorymodel in x86_far_code_models then
            list.concat(taicpu.op_reg(A_PUSH,S_W,NR_SI));
          if current_settings.x86memorymodel in x86_far_code_models then
            getselftobx(10)
          else
            getselftobx(6);
          loadvmttobx;
          loadmethodoffstobx;
          { set target address
            "mov %bx,4(%sp)" }
          if current_settings.x86memorymodel in x86_far_code_models then
            reference_reset_base(href,voidnearpointertype,NR_DI,6,2)
          else
            reference_reset_base(href,voidnearpointertype,NR_DI,4,2);
          if not segment_regs_equal(NR_DS,NR_SS) then
            href.segment:=NR_SS;
          list.concat(taicpu.op_reg_reg(A_MOV,S_W,NR_SP,NR_DI));
          list.concat(taicpu.op_reg_ref(A_MOV,S_W,NR_BX,href));
          if current_settings.x86memorymodel in x86_far_code_models then
            begin
              inc(href.offset,2);
              list.concat(taicpu.op_reg_ref(A_MOV,S_W,NR_SI,href));
            end;

          { load ax? }
          if procdef.proccalloption=pocall_register then
            list.concat(taicpu.op_reg_reg(A_MOV,S_W,NR_BX,NR_AX));

          { restore register
            pop  %di,bx }
          if current_settings.x86memorymodel in x86_far_code_models then
            list.concat(taicpu.op_reg(A_POP,S_W,NR_SI));
          list.concat(taicpu.op_reg(A_POP,S_W,NR_DI));
          list.concat(taicpu.op_reg(A_POP,S_W,NR_BX));

          { ret  ; jump to the address }
          if current_settings.x86memorymodel in x86_far_code_models then
            list.concat(taicpu.op_none(A_RETF,S_W))
          else
            list.concat(taicpu.op_none(A_RET,S_W));
        end
      { case 0 }
      else
        begin
          lab:=current_asmdata.RefAsmSymbol(procdef.mangledname);

          if current_settings.x86memorymodel in x86_far_code_models then
            list.concat(taicpu.op_sym(A_JMP,S_FAR,lab))
          else
            list.concat(taicpu.op_sym(A_JMP,S_NO,lab));
        end;

      List.concat(Tai_symbol_end.Createname(labelname));
    end;


  procedure thlcgcpu.location_force_mem(list: TAsmList; var l: tlocation; size: tdef);
    var
      r,tmpref: treference;
    begin
      if is_methodptr_like_type(size) and (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tg.gethltemp(list,size,size.size,tt_normal,r);
          tmpref:=r;

          a_load_reg_ref(list,voidcodepointertype,voidcodepointertype,l.register,tmpref);
          inc(tmpref.offset,voidcodepointertype.size);
          { the second part could be either self or parentfp }
          if size.size=(voidcodepointertype.size+voidpointertype.size) then
            a_load_reg_ref(list,voidpointertype,voidpointertype,l.registerhi,tmpref)
          else if size.size=(voidcodepointertype.size+parentfpvoidpointertype.size) then
            a_load_reg_ref(list,parentfpvoidpointertype,parentfpvoidpointertype,l.registerhi,tmpref)
          else
            internalerror(2014052202);

          location_reset_ref(l,LOC_REFERENCE,l.size,0);
          l.reference:=r;
        end
      else if is_fourbyterecord(size) and (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
        begin
          tg.gethltemp(list,size,size.size,tt_normal,r);
          tmpref:=r;

          cg.a_load_reg_ref(list,OS_16,OS_16,l.register,tmpref);
          inc(tmpref.offset,2);
          if l.registerhi<>tregister(0) then
            cg.a_load_reg_ref(list,OS_16,OS_16,l.registerhi,tmpref)
          else
            cg.a_load_reg_ref(list,OS_16,OS_16,GetNextReg(l.register),tmpref);

          location_reset_ref(l,LOC_REFERENCE,l.size,0);
          l.reference:=r;
        end
      else
        inherited;
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;


begin
  chlcgobj:=thlcgcpu;
end.
