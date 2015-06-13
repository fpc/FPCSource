{
    Copyright (c) 2010, 2013 by Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the LLVM high level code generator

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
unit hlcgllvm;

{$i fpcdefs.inc}

interface

uses
  globtype,cclasses,
  aasmbase,aasmdata,
  symbase,symconst,symtype,symdef,symsym,
  cpubase, hlcgobj, cgbase, cgutils, parabase, tgobj;

  type

    { thlcgllvm }

    thlcgllvm = class(thlcgobj)
      constructor create;

      procedure temp_to_ref(p: ptemprecord; out ref: treference); override;

      procedure a_load_ref_cgpara(list: TAsmList; size: tdef; const r: treference; const cgpara: TCGPara); override;
     protected
       procedure a_load_ref_cgpara_init_src(list: TAsmList; const para: tcgpara; const initialref: treference; var refsize: tdef; out newref: treference);
     public
      procedure getcpuregister(list: TAsmList; r: Tregister); override;
      procedure ungetcpuregister(list: TAsmList; r: Tregister); override;
      procedure alloccpuregisters(list: TAsmList; rt: Tregistertype; const r: Tcpuregisterset); override;
      procedure deallocallcpuregisters(list: TAsmList); override;

     protected
      procedure a_call_common(list: TAsmList; pd: tabstractprocdef; const paras: array of pcgpara; const forceresdef: tdef; out res: tregister; out calldef: tdef; out hlretdef: tdef; out llvmretdef: tdef; out callparas: tfplist);
     public
      function a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;override;
      function a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister; const paras: array of pcgpara): tcgpara; override;

      procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : tcgint;register : tregister);override;
      procedure a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);override;
      procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
      procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
      procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
      procedure a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference); override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister); override;
      procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister); override;

      procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;
      procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); override;
      procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation); override;

      procedure a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

      procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

      procedure g_concatcopy(list : TAsmList;size: tdef; const source,dest : treference);override;

      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;

      procedure gen_proc_symbol(list: TAsmList); override;
      procedure gen_proc_symbol_end(list: TAsmList); override;
      procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean); override;
      procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean); override;
     protected
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;
     public
      procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;
      procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;var ovloc : tlocation); override;

      procedure g_ptrtypecast_reg(list: TAsmList; fromdef, todef: tpointerdef; reg: tregister); override;
      procedure g_ptrtypecast_ref(list: TAsmList; fromdef, todef: tpointerdef; var ref: treference); override;

      procedure a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle); override;
      procedure a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle); override;

      function get_call_result_cgpara(pd: tabstractprocdef; forceresdef: tdef): tcgpara; override;
     protected
      procedure gen_load_loc_function_result(list: TAsmList; vardef: tdef; const l: tlocation); override;
     public
      procedure gen_load_loc_cgpara(list: TAsmList; vardef: tdef; const l: tlocation; const cgpara: tcgpara); override;
      procedure gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean); override;
{$ifdef cpuflags}
      { llvm doesn't have flags, but cpuflags is defined in case the real cpu
        has flags and we have to override the abstract methods to prevent
        warnings }
      procedure a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel); override;
      procedure g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister); override;
      procedure g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref: TReference); override;
{$endif cpuflags}

      { unimplemented or unnecessary routines }
      procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tdef; src, dst: tregister); override;
      procedure g_stackpointer_alloc(list: TAsmList; size: longint); override;
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint); override;
      procedure g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint); override;
      procedure g_local_unwind(list: TAsmList; l: TAsmLabel); override;

      procedure varsym_set_localloc(list: TAsmList; vs: tabstractnormalvarsym); override;
      procedure paravarsym_set_initialloc_to_paraloc(vs: tparavarsym); override;

      procedure g_external_wrapper(list: TAsmList; procdef: tprocdef; const externalname: string); override;

      { def is the type of the data stored in memory pointed to by ref, not
        a pointer to this type }
      function make_simple_ref(list: TAsmList; const ref: treference; def: tdef): treference;
    protected
      procedure paraloctoloc(const paraloc: pcgparalocation; out hloc: tlocation);
      procedure set_call_function_result(const list: TAsmList; const pd: tabstractprocdef; const llvmretdef, hlretdef: tdef; const resval: tregister; var retpara: tcgpara);
    end;

  procedure create_hlcodegen;


implementation

  uses
    verbose,cutils,globals,fmodule,constexp,systems,
    defutil,llvmdef,llvmsym,
    aasmtai,aasmcpu,
    aasmllvm,llvmbase,tgllvm,
    symtable,
    paramgr,llvmpara,
    procinfo,cpuinfo,cgobj,cgllvm,cghlcpu;

  const
    topcg2llvmop: array[topcg] of tllvmop =
     { OP_NONE  OP_MOVE     OP_ADD  OP_AND  OP_DIV   OP_IDIV  OP_IMUL OP_MUL }
      (la_none, la_none, la_add, la_and, la_udiv, la_sdiv, la_mul, la_mul,
     { OP_NEG   OP_NOT   OP_OR  OP_SAR   OP_SHL  OP_SHR   OP_SUB  OP_XOR }
       la_none, la_none, la_or, la_ashr, la_shl, la_lshr, la_sub, la_xor,
     { OP_ROL   OP_ROR }
       la_none, la_none);


  constructor thlcgllvm.create;
    begin
      inherited
    end;


  procedure thlcgllvm.temp_to_ref(p: ptemprecord; out ref: treference);
    begin
      { on the LLVM target, every temp is independent and encoded via a
        separate temp register whose superregister number is stored in p^.pos }
      reference_reset_base(ref,voidstackpointertype,newreg(R_TEMPREGISTER,p^.pos,R_SUBWHOLE),0,p^.alignment);
    end;


  procedure thlcgllvm.a_load_ref_cgpara(list: TAsmList; size: tdef; const r: treference; const cgpara: TCGPara);
    var
      tmpref, initialref, ref: treference;
      orgsize: tdef;
      tmpreg: tregister;
      hloc: tlocation;
      location: pcgparalocation;
      orgsizeleft,
      sizeleft,
      totaloffset: asizeint;
      paralocidx: longint;
      userecord,
      reghasvalue: boolean;
    begin
      location:=cgpara.location;
      sizeleft:=cgpara.intsize;
      totaloffset:=0;
      orgsize:=size;
      a_load_ref_cgpara_init_src(list,cgpara,r,size,initialref);
      userecord:=
        (orgsize<>size) and
        assigned(cgpara.location^.next);
      paralocidx:=0;
      while assigned(location) do
        begin
          if userecord then
            begin
              { llvmparadef is a record in this case, with every field corresponding
                to a single paraloc }
              paraloctoloc(location,hloc);
              tmpreg:=getaddressregister(list,getpointerdef(location^.def));
              list.concat(taillvm.getelementptr_reg_size_ref_size_const(tmpreg,getpointerdef(size),initialref,s32inttype,paralocidx,true));
              reference_reset_base(tmpref,getpointerdef(location^.def),tmpreg,0,newalignment(initialref.alignment,totaloffset));
            end
          else
            tmpref:=initialref;
          paramanager.allocparaloc(list,location);
          case location^.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 { byval parameter -> load the address rather than the value }
                 if not location^.llvmvalueloc then
                   a_loadaddr_ref_reg(list,tpointerdef(location^.def).pointeddef,location^.def,tmpref,location^.register)
                 { if this parameter is split into multiple paralocs via
                   record fields, load the current paraloc. The type of the
                   paraloc and of the current record field will match by
                   construction (the record is build from the paraloc
                   types) }
                 else if userecord then
                   a_load_ref_reg(list,location^.def,location^.def,tmpref,location^.register)
                 { if the parameter is passed in a single paraloc, the
                   paraloc's type may be different from the declared type
                   -> use the original complete parameter size as source so
                   we can insert a type conversion if necessary }
                 else
                   a_load_ref_reg(list,size,location^.def,tmpref,location^.register)
             end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 if assigned(location^.next) then
                   internalerror(2010052906);
                 reference_reset_base(ref,getpointerdef(size),location^.reference.index,location^.reference.offset,newalignment(cgpara.alignment,cgpara.intsize-sizeleft));
                 if (def_cgsize(size)<>OS_NO) and
                    (size.size=sizeleft) and
                    (sizeleft<=sizeof(aint)) then
                   a_load_ref_ref(list,size,location^.def,tmpref,ref)
                 else
                   { use concatcopy, because the parameter can be larger than }
                   { what the OS_* constants can handle                       }
                   g_concatcopy(list,location^.def,tmpref,ref);
              end;
            LOC_MMREGISTER,LOC_CMMREGISTER:
              begin
                 case location^.size of
                   OS_F32,
                   OS_F64,
                   OS_F128:
                     a_loadmm_ref_reg(list,location^.def,location^.def,tmpref,location^.register,mms_movescalar);
                   OS_M8..OS_M128,
                   OS_MS8..OS_MS128:
                     a_loadmm_ref_reg(list,location^.def,location^.def,tmpref,location^.register,nil);
                   else
                     internalerror(2010053101);
                 end;
              end
            else
              internalerror(2010053111);
          end;
          inc(totaloffset,tcgsize2size[location^.size]);
          dec(sizeleft,tcgsize2size[location^.size]);
          location:=location^.next;
          inc(paralocidx);
        end;
    end;


  procedure thlcgllvm.a_load_ref_cgpara_init_src(list: TAsmList; const para: tcgpara; const initialref: treference; var refsize: tdef; out newref: treference);
    var
      newrefsize: tdef;
      reg: tregister;
    begin
      newrefsize:=llvmgetcgparadef(para,true);
      if refsize<>newrefsize then
        begin
          reg:=getaddressregister(list,getpointerdef(newrefsize));
          a_loadaddr_ref_reg(list,refsize,getpointerdef(newrefsize),initialref,reg);
          reference_reset_base(newref,getpointerdef(newrefsize),reg,0,initialref.alignment);
          refsize:=newrefsize;
        end
      else
        newref:=initialref;
    end;


  procedure thlcgllvm.getcpuregister(list: TAsmList; r: Tregister);
    begin
      { don't do anything }
    end;


  procedure thlcgllvm.ungetcpuregister(list: TAsmList; r: Tregister);
    begin
      { don't do anything }
    end;


  procedure thlcgllvm.alloccpuregisters(list: TAsmList; rt: Tregistertype; const r: Tcpuregisterset);
    begin
      { don't do anything }
    end;


  procedure thlcgllvm.deallocallcpuregisters(list: TAsmList);
    begin
      { don't do anything }
    end;


  function get_call_pd(pd: tabstractprocdef): tdef;
    begin
      if (pd.typ=procdef) or
         not pd.is_addressonly then
        result:=getprocaddressprocvar(pd)
      else
        result:=pd
    end;


  procedure thlcgllvm.a_call_common(list: TAsmList; pd: tabstractprocdef; const paras: array of pcgpara; const forceresdef: tdef; out res: tregister; out calldef: tdef; out hlretdef: tdef; out llvmretdef: tdef; out callparas: tfplist);

    procedure load_ref_anyreg(def: tdef; const ref: treference; reg: tregister; var callpara: pllvmcallpara);
      begin
        case getregtype(reg) of
          R_INTREGISTER,
          R_ADDRESSREGISTER:
            begin
              a_load_ref_reg(list,def,def,ref,reg);
              callpara^.loc:=LOC_REGISTER;
            end;
          R_FPUREGISTER:
            begin
              a_loadfpu_ref_reg(list,def,def,ref,reg);
              callpara^.loc:=LOC_FPUREGISTER;
            end;
          R_MMREGISTER:
            begin
              a_loadmm_ref_reg(list,def,def,ref,reg,mms_movescalar);
              callpara^.loc:=LOC_MMREGISTER;
            end;
          else
            internalerror(2014012213);
        end;
      end;

  var
    i: longint;
    href: treference;
    callpara: pllvmcallpara;
    paraloc: pcgparalocation;
  begin
    callparas:=tfplist.Create;
    for i:=0 to high(paras) do
      begin
        paraloc:=paras[i]^.location;
        while assigned(paraloc) and
              (paraloc^.loc<>LOC_VOID) do
          begin
            new(callpara);
            callpara^.def:=paraloc^.def;
            llvmextractvalueextinfo(paras[i]^.def, callpara^.def, callpara^.valueext);
            callpara^.loc:=paraloc^.loc;
            case callpara^.loc of
              LOC_REFERENCE:
                begin
                  if paraloc^.llvmvalueloc then
                    internalerror(2014012307)
                  else
                    begin
                      reference_reset_base(href, getpointerdef(callpara^.def), paraloc^.reference.index, paraloc^.reference.offset, paraloc^.def.alignment);
                      res:=getregisterfordef(list, paraloc^.def);
                      load_ref_anyreg(callpara^.def, href, res, callpara);
                    end;
                  callpara^.reg:=res
                end;
              LOC_REGISTER,
              LOC_FPUREGISTER,
              LOC_MMREGISTER:
                begin
                  { undo explicit value extension }
                  if callpara^.valueext<>lve_none then
                    begin
                      res:=getregisterfordef(list, callpara^.def);
                      a_load_reg_reg(list, paraloc^.def, callpara^.def, paraloc^.register, res);
                      paraloc^.register:=res;
                    end;
                    callpara^.reg:=paraloc^.register
                end;
              else
                internalerror(2014010605);
            end;
            callparas.add(callpara);
            paraloc:=paraloc^.next;
          end;
      end;
    { the Pascal level may expect a different returndef compared to the
      declared one }
    if not assigned(forceresdef) then
      hlretdef:=pd.returndef
    else
      hlretdef:=forceresdef;
    { llvm will always expect the original return def }
    if not paramanager.ret_in_param(hlretdef, pd) then
      llvmretdef:=llvmgetcgparadef(pd.funcretloc[callerside], true)
    else
      llvmretdef:=voidtype;
    if not is_void(llvmretdef) then
      res:=getregisterfordef(list, llvmretdef)
    else
      res:=NR_NO;

    { if this is a complex procvar, get the non-tmethod-like equivalent }
    if (pd.typ=procvardef) and
       not pd.is_addressonly then
      pd:=tprocvardef(getprocaddressprocvar(pd));
    { if the function returns a function pointer type or is varargs, we
      must specify the full function signature, otherwise we can only
      specify the return type }
    if (po_varargs in pd.procoptions) or
       ((pd.proccalloption in cdecl_pocalls) and
        (pd.paras.count>0) and
        is_array_of_const(tparavarsym(pd.paras[pd.paras.count-1]).vardef)) then
      calldef:=get_call_pd(pd)
    else
      calldef:=llvmretdef;
  end;


  function thlcgllvm.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; const paras: array of pcgpara; forceresdef: tdef; weak: boolean): tcgpara;
    var
      callparas: tfplist;
      asmsym: tasmsymbol;
      llvmretdef,
      hlretdef,
      calldef: tdef;
      res: tregister;
    begin
      if not pd.owner.iscurrentunit or
         (s<>pd.mangledname) or
         (po_external in pd.procoptions) then
        begin
          asmsym:=current_asmdata.RefAsmSymbol(tprocdef(pd).mangledname);
          if not asmsym.declared then
            current_asmdata.AsmLists[al_imports].Concat(taillvmdecl.create(asmsym,pd,nil,sec_code,pd.alignment));
        end;
      a_call_common(list,pd,paras,forceresdef,res,calldef,hlretdef,llvmretdef,callparas);
      list.concat(taillvm.call_size_name_paras(get_call_pd(pd),res,calldef,current_asmdata.RefAsmSymbol(pd.mangledname),callparas));
      result:=get_call_result_cgpara(pd,forceresdef);
      set_call_function_result(list,pd,llvmretdef,hlretdef,res,result);
    end;


  function thlcgllvm.a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister; const paras: array of pcgpara): tcgpara;
    var
      callparas: tfplist;
      llvmretdef,
      hlretdef,
      calldef: tdef;
      res: tregister;
    begin
      a_call_common(list,pd,paras,nil,res,calldef,hlretdef,llvmretdef,callparas);
      list.concat(taillvm.call_size_reg_paras(get_call_pd(pd),res,calldef,reg,callparas));
      result:=get_call_result_cgpara(pd,nil);
      set_call_function_result(list,pd,llvmretdef,hlretdef,res,result);
    end;


  procedure thlcgllvm.a_load_const_reg(list: TAsmList; tosize: tdef; a: tcgint; register: tregister);
    begin
      list.concat(taillvm.op_reg_size_const_size(llvmconvop(ptrsinttype,tosize),register,ptrsinttype,a,tosize))
    end;


  procedure thlcgllvm.a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);
    var
      sref: treference;
    begin
      { llvm instructions do not support pointer constants -> only directly
        encode for integers; a_load_const_reg() handles pointers properly }
      if is_ordinal(tosize) or
         is_64bit(tosize) then
        begin
          sref:=make_simple_ref(list,ref,tosize);
          list.concat(taillvm.op_size_const_size_ref(la_store,tosize,a,getpointerdef(tosize),sref))
        end
      else
        inherited;
    end;


  function def2intdef(fromsize, tosize: tdef): tdef;
    begin
      { we cannot zero-extend from/to anything but ordinal/enum
        types }
      if not(tosize.typ in [orddef,enumdef]) then
        internalerror(2014012305);
      { will give an internalerror if def_cgsize() returns OS_NO, which is
        what we want }
      result:=cgsize_orddef(def_cgsize(fromsize));
    end;


  procedure thlcgllvm.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    var
      tmpref,
      sref: treference;
      hreg,
      hreg2: tregister;
      tmpsize: tdef;
    begin
      sref:=make_simple_ref(list,ref,tosize);
      hreg:=register;
      (* typecast the pointer to the value instead of the value itself if
        they have the same size but are of different kinds, because we can't
        e.g. typecast a loaded <{i32, i32}> to an i64 *)
      if (llvmaggregatetype(fromsize) or
          llvmaggregatetype(tosize)) and
         (fromsize<>tosize) then
        begin
          if fromsize.size>tosize.size then
            begin
              { if source size is larger than the target size, we have to
                truncate it before storing. Unfortunately, we cannot truncate
                records (nor bitcast them to integers), so we first have to
                store them to memory and then bitcast the pointer to them
              }
              if fromsize.typ in [arraydef,recorddef] then
                begin
                  { store struct/array-in-register to memory }
                  tmpsize:=def2intdef(fromsize,tosize);
                  tg.gethltemp(list,fromsize,fromsize.size,tt_normal,tmpref);
                  a_load_reg_ref(list,fromsize,fromsize,register,tmpref);
                  { typecast pointer to memory into pointer to integer type }
                  hreg:=getaddressregister(list,getpointerdef(tmpsize));
                  a_loadaddr_ref_reg(list,fromsize,getpointerdef(tmpsize),tmpref,hreg);
                  reference_reset_base(sref,getpointerdef(tmpsize),hreg,0,tmpref.alignment);
                  { load the integer from the temp into the destination }
                  a_load_ref_ref(list,tmpsize,tosize,tmpref,sref);
                  tg.ungettemp(list,tmpref);
                end
              else
                begin
                  tmpsize:=def2intdef(tosize,fromsize);
                  hreg:=getintregister(list,tmpsize);
                  { truncate the integer }
                  a_load_reg_reg(list,fromsize,tmpsize,register,hreg);
                  { store it to memory (it will now be of the same size as the
                    struct, and hence another path will be followed in this
                    method) }
                  a_load_reg_ref(list,tmpsize,tosize,hreg,sref);
                end;
                exit;
            end
          else
            begin
              hreg2:=getaddressregister(list,getpointerdef(fromsize));
              a_loadaddr_ref_reg(list,tosize,getpointerdef(fromsize),sref,hreg2);
              reference_reset_base(sref,getpointerdef(fromsize),hreg2,0,sref.alignment);
              tosize:=fromsize;
            end;
        end
      else if fromsize<>tosize then
        begin
          hreg:=getregisterfordef(list,tosize);
          a_load_reg_reg(list,fromsize,tosize,register,hreg);
        end;
      list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,hreg,getpointerdef(tosize),sref));
    end;


  procedure thlcgllvm.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    var
      op: tllvmop;
      tmpreg: tregister;
      tmpintdef: tdef;
    begin
      op:=llvmconvop(fromsize,tosize);
      { converting from pointer to something else and vice versa is only
        possible via an intermediate pass to integer. Same for "something else"
        to pointer. }
      case op of
        la_ptrtoint_to_x,
        la_x_to_inttoptr:
          begin
            { convert via an integer with the same size as "x" }
            if op=la_ptrtoint_to_x then
              begin
                tmpintdef:=cgsize_orddef(def_cgsize(tosize));
                op:=la_bitcast
              end
            else
              begin
                tmpintdef:=cgsize_orddef(def_cgsize(fromsize));
                op:=la_inttoptr;
              end;
            tmpreg:=getintregister(list,tmpintdef);
            a_load_reg_reg(list,fromsize,tmpintdef,reg1,tmpreg);
            reg1:=tmpreg;
            fromsize:=tmpintdef;
          end;
      end;
      { reg2 = bitcast fromsize reg1 to tosize }
      list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    var
      tmpref,
      sref: treference;
      hreg: tregister;
      tmpsize: tdef;
    begin
      sref:=make_simple_ref(list,ref,fromsize);
      { "named register"? }
      if sref.refaddr=addr_full then
        begin
          { can't bitcast records/arrays }
          if (llvmaggregatetype(fromsize) or
              llvmaggregatetype(tosize)) and
             (fromsize<>tosize) then
            begin
              tg.gethltemp(list,fromsize,fromsize.size,tt_normal,tmpref);
              list.concat(taillvm.op_size_ref_size_ref(la_store,fromsize,sref,getpointerdef(fromsize),tmpref));
              a_load_ref_reg(list,fromsize,tosize,tmpref,register);
              tg.ungettemp(list,tmpref);
            end
          else
            list.concat(taillvm.op_reg_size_ref_size(la_bitcast,register,fromsize,sref,tosize))
        end
      else
        begin
          if ((fromsize.typ in [arraydef,recorddef]) or
              (tosize.typ in [arraydef,recorddef])) and
             (fromsize<>tosize) then
            begin
              if fromsize.size<tosize.size then
                begin
                  { if the target size is larger than the source size, we
                    have to perform the zero-extension using an integer type
                    (can't zero-extend a record/array) }
                  if fromsize.typ in [arraydef,recorddef] then
                    begin
                      { typecast the pointer to the struct into a pointer to an
                        integer of equal size }
                      tmpsize:=def2intdef(fromsize,tosize);
                      hreg:=getaddressregister(list,getpointerdef(tmpsize));
                      a_loadaddr_ref_reg(list,fromsize,getpointerdef(tmpsize),sref,hreg);
                      reference_reset_base(sref,getpointerdef(tmpsize),hreg,0,sref.alignment);
                      { load that integer }
                      a_load_ref_reg(list,tmpsize,tosize,sref,register);
                    end
                  else
                    begin
                      { load the integer into an integer memory location with
                        the same size as the struct (the integer should be
                        unsigned, we don't want sign extensions here) }
                      if is_signed(fromsize) then
                        internalerror(2014012309);
                      tmpsize:=def2intdef(tosize,fromsize);
                      tg.gethltemp(list,tmpsize,tmpsize.size,tt_normal,tmpref);
                      { typecast the struct-sized integer location into the
                        struct type }
                      a_load_ref_ref(list,fromsize,tmpsize,sref,tmpref);
                      { load the struct in the register }
                      a_load_ref_reg(list,tmpsize,tosize,tmpref,register);
                      tg.ungettemp(list,tmpref);
                    end;
                  exit;
                end
              else
                begin
                  (* typecast the pointer to the value instead of the value
                     itself if they have the same size but are of different
                     kinds, because we can't e.g. typecast a loaded <{i32, i32}>
                     to an i64 *)
                  hreg:=getaddressregister(list,getpointerdef(tosize));
                  a_loadaddr_ref_reg(list,fromsize,getpointerdef(tosize),sref,hreg);
                  reference_reset_base(sref,getpointerdef(tosize),hreg,0,sref.alignment);
                  fromsize:=tosize;
                end;
            end;
          hreg:=register;
          if fromsize<>tosize then
            hreg:=getregisterfordef(list,fromsize);
          list.concat(taillvm.op_reg_size_ref(la_load,hreg,getpointerdef(fromsize),sref));
          if hreg<>register then
            a_load_reg_reg(list,fromsize,tosize,hreg,register);
        end;
    end;


  procedure thlcgllvm.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
    var
      sdref: treference;
    begin
      if (fromsize=tosize) and
         (sref.refaddr=addr_full) then
        begin
          sdref:=make_simple_ref(list,dref,tosize);
          list.concat(taillvm.op_size_ref_size_ref(la_store,fromsize,sref,getpointerdef(tosize),sdref));
        end
      else
        inherited
    end;


  procedure thlcgllvm.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    var
      sref: treference;
    begin
      { can't take the address of a 'named register' }
      if ref.refaddr=addr_full then
        internalerror(2013102306);
      sref:=make_simple_ref(list,ref,fromsize);
      list.concat(taillvm.op_reg_size_ref_size(la_bitcast,r,getpointerdef(fromsize),sref,tosize));
    end;


  procedure thlcgllvm.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister);
    begin
      a_op_const_reg_reg(list,op,size,a,reg,reg);
    end;


  procedure thlcgllvm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister);
    var
      tmpreg: tregister;
    begin
      if (def2regtyp(size)=R_INTREGISTER) and
         (topcg2llvmop[op]<>la_none) then
        list.concat(taillvm.op_reg_size_reg_const(topcg2llvmop[op],dst,size,src,a))
      else
        begin
          { default implementation is not SSA-safe }
          tmpreg:=getregisterfordef(list,size);
          a_load_const_reg(list,size,a,tmpreg);
          a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
        end;
    end;


  procedure thlcgllvm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    var
      orgdst,
      tmpreg1,
      tmpreg2,
      tmpreg3: tregister;
      opsize: tdef;
    begin
      orgdst:=dst;
      opsize:=size;
      { always perform using integer registers, because math operations on
        pointers are not supported (except via getelementptr, possible future
        optimization) }
      if def2regtyp(size)=R_ADDRESSREGISTER then
        begin
          opsize:=ptruinttype;

          tmpreg1:=getintregister(list,ptruinttype);
          a_load_reg_reg(list,size,ptruinttype,src1,tmpreg1);
          src1:=tmpreg1;

          tmpreg1:=getintregister(list,ptruinttype);
          a_load_reg_reg(list,size,ptruinttype,src2,tmpreg1);
          src2:=tmpreg1;

          dst:=getintregister(list,ptruinttype);
        end;
     if topcg2llvmop[op]<>la_none then
       list.concat(taillvm.op_reg_size_reg_reg(topcg2llvmop[op],dst,opsize,src2,src1))
     else
       begin
         case op of
           OP_NEG:
             { %dst = sub size 0, %src1 }
             list.concat(taillvm.op_reg_size_const_reg(la_sub,dst,opsize,0,src1));
           OP_NOT:
             { %dst = xor size -1, %src1 }
             list.concat(taillvm.op_reg_size_const_reg(la_xor,dst,opsize,-1,src1));
           OP_ROL:
             begin
               tmpreg1:=getintregister(list,opsize);
               tmpreg2:=getintregister(list,opsize);
               tmpreg3:=getintregister(list,opsize);
               { tmpreg1 := tcgsize2size[size] - src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,opsize,opsize.size,src1));
               { tmpreg2 := src2 shr tmpreg1 }
               a_op_reg_reg_reg(list,OP_SHR,opsize,tmpreg1,src2,tmpreg2);
               { tmpreg3 := src2 shl src1 }
               a_op_reg_reg_reg(list,OP_SHL,opsize,src1,src2,tmpreg3);
               { dst := tmpreg2 or tmpreg3 }
               a_op_reg_reg_reg(list,OP_OR,opsize,tmpreg2,tmpreg3,dst);
             end;
           OP_ROR:
             begin
               tmpreg1:=getintregister(list,size);
               tmpreg2:=getintregister(list,size);
               tmpreg3:=getintregister(list,size);
               { tmpreg1 := tcgsize2size[size] - src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,opsize,opsize.size,src1));
               { tmpreg2 := src2 shl tmpreg1 }
               a_op_reg_reg_reg(list,OP_SHL,opsize,tmpreg1,src2,tmpreg2);
               { tmpreg3 := src2 shr src1 }
               a_op_reg_reg_reg(list,OP_SHR,opsize,src1,src2,tmpreg3);
               { dst := tmpreg2 or tmpreg3 }
               a_op_reg_reg_reg(list,OP_OR,opsize,tmpreg2,tmpreg3,dst);
             end;
           else
             internalerror(2010081310);
         end;
       end;
     if dst<>orgdst then
       a_load_reg_reg(list,opsize,size,dst,orgdst);
   end;


  procedure thlcgllvm.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
       a_op_reg_reg_reg(list,op,size,reg1,reg2,reg2);
    end;


  procedure thlcgllvm.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      { use xxx.with.overflow intrinsics }
      internalerror(2012111102);
    end;


  procedure thlcgllvm.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      { use xxx.with.overflow intrinsics }
      internalerror(2012111103);
    end;


  procedure thlcgllvm.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
    var
      tmpreg : tregister;
      invert: boolean;
      fallthroughlab, falselab, tmplab: tasmlabel;
    begin
      { since all comparisons return their results in a register, we'll often
        get comparisons against true/false -> optimise }
      if (size=pasbool8type) and
         (cmp_op in [OC_EQ,OC_NE]) then
        begin
          case cmp_op of
            OC_EQ:
              invert:=a=0;
            OC_NE:
              invert:=a=1;
            else
              { avoid uninitialised warning }
              internalerror(2015031504);
            end;
          current_asmdata.getjumplabel(falselab);
          fallthroughlab:=falselab;
          if invert then
            begin
              tmplab:=l;
              l:=falselab;
              falselab:=tmplab;
            end;
          list.concat(taillvm.op_size_reg_lab_lab(la_br,pasbool8type,reg,l,falselab));
          a_label(list,fallthroughlab);
          exit;
        end;
      tmpreg:=getregisterfordef(list,size);
      a_load_const_reg(list,size,a,tmpreg);
      a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
    end;


  procedure thlcgllvm.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    var
      resreg: tregister;
      falselab: tasmlabel;
    begin
      if getregtype(reg1)<>getregtype(reg2) then
        internalerror(2012111105);
      resreg:=getintregister(list,pasbool8type);
      current_asmdata.getjumplabel(falselab);
      { invert order of registers. In FPC, cmp_reg_reg(reg1,reg2) means that
        e.g. OC_GT is true if "subl %reg1,%reg2" in x86 AT&T is >0. In LLVM,
        OC_GT is true if op1>op2 }
      list.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,resreg,cmp_op,size,reg2,reg1));
      list.concat(taillvm.op_size_reg_lab_lab(la_br,pasbool8type,resreg,l,falselab));
      a_label(list,falselab);
    end;


  procedure thlcgllvm.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      { implement in tcg because required by the overridden a_label; doesn't use
        any high level stuff anyway }
      cg.a_jmp_always(list,l);
    end;


  procedure thlcgllvm.g_concatcopy(list: TAsmList; size: tdef; const source, dest: treference);
    begin
      a_load_ref_ref(list,size,size,source,dest);
    end;


  procedure thlcgllvm.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    var
       tmpreg: tregister;
       href: treference;
       fromcompcurr,
       tocompcurr: boolean;
     begin
       { named register -> use generic code }
       if ref.refaddr=addr_full then
         begin
           a_load_ref_reg(list,fromsize,tosize,ref,reg);
           exit
         end;
       { comp and currency are handled by the x87 in this case. They cannot
         be represented directly in llvm, and llvmdef translates them into i64
         (since that's their storage size and internally they also are int64).
         Solve this by changing the type to s80real once they are loaded into
         a register. }
       fromcompcurr:=tfloatdef(fromsize).floattype in [s64comp,s64currency];
       tocompcurr:=tfloatdef(tosize).floattype in [s64comp,s64currency];
       if tocompcurr then
         tosize:=s80floattype;
       href:=make_simple_ref(list,ref,fromsize);
       { don't generate different code for loading e.g. extended into cextended,
         but to take care of loading e.g. comp (=int64) into double }
       if (fromsize.size<>tosize.size) then
         tmpreg:=getfpuregister(list,fromsize)
       else
         tmpreg:=reg;
       { %tmpreg = load size* %ref }
       list.concat(taillvm.op_reg_size_ref(la_load,tmpreg,getpointerdef(fromsize),href));
       if tmpreg<>reg then
         if fromcompcurr then
           { treat as extended as long as it's in a register }
           list.concat(taillvm.op_reg_size_reg_size(la_sitofp,reg,fromsize,tmpreg,tosize))
         else
           a_loadfpu_reg_reg(list,fromsize,tosize,tmpreg,reg);
     end;


  procedure thlcgllvm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    var
       tmpreg: tregister;
       href: treference;
       fromcompcurr,
       tocompcurr: boolean;
     begin
       { see comment in a_loadfpu_ref_reg }
       fromcompcurr:=tfloatdef(fromsize).floattype in [s64comp,s64currency];
       tocompcurr:=tfloatdef(tosize).floattype in [s64comp,s64currency];
       if fromcompcurr then
         fromsize:=s80floattype;
       href:=make_simple_ref(list,ref,tosize);
       { don't generate different code for loading e.g. extended into cextended,
         but to take care of storing e.g. comp (=int64) into double  }
       if (fromsize.size<>tosize.size) then
         begin
           tmpreg:=getfpuregister(list,tosize);
           if tocompcurr then
             { store back an int64 rather than an extended }
             list.concat(taillvm.op_reg_size_reg_size(la_fptosi,tmpreg,fromsize,reg,tosize))
           else
             a_loadfpu_reg_reg(list,fromsize,tosize,reg,tmpreg);
         end
       else
         tmpreg:=reg;
       { store tosize tmpreg, tosize* href }
       list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,tmpreg,getpointerdef(tosize),href));
     end;


  procedure thlcgllvm.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    var
      op: tllvmop;
      intfromsize,
      inttosize: longint;
    begin
      { treat comp and currency as extended in registers (see comment at start
        of a_loadfpu_ref_reg) }
      if tfloatdef(fromsize).floattype in [s64comp,s64currency] then
        fromsize:=sc80floattype;
      if tfloatdef(tosize).floattype in [s64comp,s64currency] then
        tosize:=sc80floattype;
      { at the value level, s80real and sc80real are the same }
      if fromsize<>s80floattype then
        intfromsize:=fromsize.size
      else
        intfromsize:=sc80floattype.size;
      if tosize<>s80floattype then
        inttosize:=tosize.size
      else
        inttosize:=sc80floattype.size;

      if intfromsize<inttosize then
        op:=la_fpext
       else if intfromsize>inttosize then
        op:=la_fptrunc
      else
        op:=la_bitcast;
      { reg2 = bitcast fromllsize reg1 to tollsize }
      list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.gen_proc_symbol(list: TAsmList);
    var
      item: TCmdStrListItem;
      mangledname: TSymStr;
      asmsym: tasmsymbol;
    begin
      item:=TCmdStrListItem(current_procinfo.procdef.aliasnames.first);
      mangledname:=current_procinfo.procdef.mangledname;
      { predefine the real function name as local/global, so the aliases can
        refer to the symbol and get the binding correct }
      if (cs_profile in current_settings.moduleswitches) or
         (po_global in current_procinfo.procdef.procoptions) then
        asmsym:=current_asmdata.DefineAsmSymbol(mangledname,AB_GLOBAL,AT_FUNCTION)
      else
        asmsym:=current_asmdata.DefineAsmSymbol(mangledname,AB_LOCAL,AT_FUNCTION);
      while assigned(item) do
        begin
          if mangledname<>item.Str then
            list.concat(taillvmalias.create(asmsym,item.str,current_procinfo.procdef,llv_default,lll_default));
          item:=TCmdStrListItem(item.next);
        end;
      list.concat(taillvmdecl.create(asmsym,current_procinfo.procdef,nil,sec_code,current_procinfo.procdef.alignment));
    end;


  procedure thlcgllvm.gen_proc_symbol_end(list: TAsmList);
    begin
      list.concat(Tai_symbol_end.Createname(current_procinfo.procdef.mangledname));
      { todo: darwin main proc, or handle in other way? }
    end;


  procedure thlcgllvm.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      list.concatlist(ttgllvm(tg).alloclist)
      { rest: todo }
    end;


  procedure thlcgllvm.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    var
      retdef: tdef;
      retreg,
      hreg: tregister;
      retpara: tcgpara;
    begin
      { the function result type is the type of the first location, which can
        differ from the real result type (e.g. int64 for a record consisting of
        two longint fields on x86-64 -- we are responsible for lowering the
        result types like that) }
      retpara:=get_call_result_cgpara(current_procinfo.procdef,nil);
      retpara.check_simple_location;
      retdef:=retpara.location^.def;
      if is_void(retdef) or
         { don't check retdef here, it is e.g. a pshortstring in case it's
           shortstring that's returned in a parameter }
         paramanager.ret_in_param(current_procinfo.procdef.returndef,current_procinfo.procdef) then
        list.concat(taillvm.op_size(la_ret,voidtype))
      else
        begin
          case retpara.location^.loc of
            LOC_REGISTER,
            LOC_FPUREGISTER,
            LOC_MMREGISTER:
              begin
                { inline assembler routines contain a ret at the end and never
                  get here, but we do need a return at the end -> return an
                  undefined value in this case }
                if po_assembler in current_procinfo.procdef.procoptions then
                  gen_load_uninitialized_function_result(list,current_procinfo.procdef,retpara.def,retpara);
                { sign/zeroextension of function results is handled implicitly
                  via the signext/zeroext modifiers of the result, rather than
                  in the code generator -> remove any explicit extensions here }
                retreg:=retpara.location^.register;
                if (current_procinfo.procdef.returndef.typ in [orddef,enumdef]) and
                   (retdef.typ in [orddef,enumdef]) then
                  begin
                    if (current_procinfo.procdef.returndef.size<retpara.location^.def.size) then
                      begin
                        hreg:=getintregister(list,current_procinfo.procdef.returndef);
                        a_load_reg_reg(list,retdef,current_procinfo.procdef.returndef,retreg,hreg);
                        retreg:=hreg;
                        retdef:=current_procinfo.procdef.returndef;
                      end;
                   end;
                list.concat(taillvm.op_size_reg(la_ret,retdef,retreg))
              end
            else
              { todo: complex returns }
              internalerror(2012111106);
          end;
        end;
      retpara.resetiftemp;
    end;


  procedure thlcgllvm.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
    begin
      if not paramanager.ret_in_param(resdef,pd) then
        begin
          case resloc.location^.loc of
            LOC_REGISTER,
            LOC_FPUREGISTER,
            LOC_MMREGISTER:
              begin
                resloc.check_simple_location;
                list.concat(taillvm.op_reg_size_undef(la_bitcast,resloc.location^.register,resloc.location^.def));
              end;
            else
              internalerror(2015042301);
          end;
        end;
    end;


  procedure thlcgllvm.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
    begin
      { not possible, need ovloc }
      internalerror(2012111107);
    end;


  procedure thlcgllvm.g_overflowCheck_loc(List: TAsmList; const Loc: TLocation; def: TDef; var ovloc: tlocation);
    begin
      { todo }
      internalerror(2012111108);
    end;


  procedure thlcgllvm.g_ptrtypecast_reg(list: TAsmList; fromdef, todef: tpointerdef; reg: tregister);
    begin
      { will insert a bitcast if necessary }
      a_load_reg_reg(list,fromdef,todef,reg,reg);
    end;


  procedure thlcgllvm.g_ptrtypecast_ref(list: TAsmList; fromdef, todef: tpointerdef; var ref: treference);
    var
      hreg: tregister;
    begin
      hreg:=getaddressregister(list,todef);
      a_loadaddr_ref_reg(list,fromdef.pointeddef,todef,ref,hreg);
      reference_reset_base(ref,todef,hreg,0,ref.alignment);
    end;


  procedure thlcgllvm.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle);
    var
      href: treference;
    begin
      { named register -> use generic code }
      if ref.refaddr=addr_full then
        a_load_ref_reg(list,fromsize,tosize,ref,reg)
      else if shuffle=mms_movescalar then
        a_loadfpu_ref_reg(list,fromsize,tosize,ref,reg)
      else
        begin
          { todo }
          if fromsize<>tosize then
            internalerror(2013060220);
          href:=make_simple_ref(list,ref,fromsize);
          { %reg = load size* %ref }
          list.concat(taillvm.op_reg_size_ref(la_load,reg,getpointerdef(fromsize),href));
        end;
    end;


  procedure thlcgllvm.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle);
    var
      href: treference;
    begin
      if shuffle=mms_movescalar then
        a_loadfpu_reg_ref(list,fromsize,tosize,reg,ref)
      else
        begin
          { todo }
          if fromsize<>tosize then
            internalerror(2013060220);
          href:=make_simple_ref(list,ref,tosize);
          { store tosize reg, tosize* href }
          list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,reg,getpointerdef(tosize),href))
        end;
    end;


  procedure thlcgllvm.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle);
    begin
      if shuffle=mms_movescalar then
        a_loadfpu_reg_reg(list,fromsize,tosize,reg1,reg2)
      else
        { reg2 = bitcast fromllsize reg1 to tollsize }
        list.concat(taillvm.op_reg_size_reg_size(la_bitcast,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle);
    begin
      if (op=OP_XOR) and
         (src=dst) then
        a_load_const_reg(list,size,0,dst)
      else
        { todo }
        internalerror(2013060221);
    end;


  procedure thlcgllvm.a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2013060222);
    end;


  procedure thlcgllvm.a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2013060223);
    end;


  function thlcgllvm.get_call_result_cgpara(pd: tabstractprocdef; forceresdef: tdef): tcgpara;
    var
      paraloc: pcgparalocation;
    begin
      result:=inherited;
      { we'll change the paraloc, make sure we don't modify the original one }
      if not result.temporary then
        begin
          result:=result.getcopy;
          result.temporary:=true;
        end;
      { get the LLVM representation of the function result (e.g. a
        struct with two i64 fields for a record with 4 i32 fields) }
      result.def:=llvmgetcgparadef(result,true);
      if assigned(result.location^.next) then
        begin
          { unify the result into a sinlge location; unlike for parameters,
            we are not responsible for splitting up results into multiple
            locations }
          { set the first location to the type of the function result }
          result.location^.def:=result.def;
          result.location^.size:=result.size;
          { free all extra paralocs }
          while assigned(result.location^.next) do
            begin
              paraloc:=result.location^.next^.next;
              freemem(result.location^.next);
              result.location^.next:=paraloc;
            end;
        end;
      paraloc:=result.location;
      paraloc^.def:=result.def;
      case paraloc^.loc of
        LOC_VOID:
          ;
        LOC_REGISTER,
        LOC_FPUREGISTER,
        LOC_MMREGISTER:
          begin
            paraloc^.llvmloc.loc:=paraloc^.loc;
            paraloc^.llvmloc.reg:=paraloc^.register;
            paraloc^.llvmvalueloc:=true;
          end;
        LOC_REFERENCE:
          if not paramanager.ret_in_param(pd.returndef,pd) then
            { TODO, if this can happen at all }
            internalerror(2014011901);
        else
          internalerror(2014011902);
      end;
    end;


  procedure thlcgllvm.gen_load_loc_function_result(list: TAsmList; vardef: tdef; const l: tlocation);
    begin
      gen_load_loc_cgpara(list,vardef,l,get_call_result_cgpara(current_procinfo.procdef,nil));
    end;


  procedure thlcgllvm.gen_load_loc_cgpara(list: TAsmList; vardef: tdef; const l: tlocation; const cgpara: tcgpara);
    var
      memloc: tlocation;
    begin
      if not(cgpara.location^.llvmvalueloc) then
        begin
          memloc:=l;
          location_force_mem(list,memloc,vardef);
          a_loadaddr_ref_cgpara(list,vardef,memloc.reference,cgpara);
        end
      else
        inherited;
    end;


  procedure thlcgllvm.gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean);
    var
      ploc        : pcgparalocation;
      hloc        : tlocation;
      href, href2 : treference;
      hreg        : tregister;
      llvmparadef : tdef;
      index       : longint;
      offset      : pint;
      userecord   : boolean;
    begin
      { ignore e.g. empty records }
      if (para.location^.loc=LOC_VOID) then
        exit;
      { If the parameter location is reused we don't need to copy
        anything }
      if (destloc.loc=LOC_REFERENCE) and
         reusepara then
        exit;
      { get the equivalent llvm def used to pass the parameter (e.g. a record
        with two int64 fields for passing a record consisiting of 8 bytes on
        x86-64) }
      llvmparadef:=llvmgetcgparadef(para,true);
      userecord:=
        (llvmparadef<>para.def) and
        assigned(para.location^.next);
      if userecord then
        begin
          { llvmparadef is a record in this case, with every field corresponding
            to a single paraloc }
          if destloc.loc<>LOC_REFERENCE then
            tg.gethltemp(list,llvmparadef,llvmparadef.size,tt_normal,href)
          else
            begin
              hreg:=getaddressregister(list,getpointerdef(llvmparadef));
              a_loadaddr_ref_reg(list,vardef,getpointerdef(llvmparadef),destloc.reference,hreg);
              reference_reset_base(href,getpointerdef(llvmparadef),hreg,0,destloc.reference.alignment);
            end;
          index:=0;
          offset:=0;
          ploc:=para.location;
          repeat
            paraloctoloc(ploc,hloc);
            hreg:=getaddressregister(list,getpointerdef(ploc^.def));
            list.concat(taillvm.getelementptr_reg_size_ref_size_const(hreg,getpointerdef(llvmparadef),href,s32inttype,index,true));
            reference_reset_base(href2,getpointerdef(ploc^.def),hreg,0,newalignment(href.alignment,offset));
            a_load_loc_ref(list,ploc^.def,ploc^.def,hloc,href2);
            inc(offset,ploc^.def.size);
            inc(index);
            ploc:=ploc^.next;
          until not assigned(ploc);
          if destloc.loc<>LOC_REFERENCE then
            tg.ungettemp(list,href);
        end
      else
        begin
          para.check_simple_location;
          paraloctoloc(para.location,hloc);
          case destloc.loc of
            LOC_REFERENCE :
              begin
                case def2regtyp(llvmparadef) of
                  R_INTREGISTER,
                  R_ADDRESSREGISTER:
                    a_load_loc_ref(list,llvmparadef,vardef,hloc,destloc.reference);
                  R_FPUREGISTER:
                    a_loadfpu_loc_ref(list,llvmparadef,vardef,hloc,destloc.reference);
                  R_MMREGISTER:
                    a_loadmm_loc_ref(list,llvmparadef,vardef,hloc,destloc.reference,nil);
                  else
                    internalerror(2014080801);
                  end;
              end;
            LOC_REGISTER:
              begin
                a_load_loc_reg(list,llvmparadef,vardef,hloc,destloc.register);
              end;
            LOC_FPUREGISTER:
              begin
                a_loadfpu_loc_reg(list,llvmparadef,vardef,hloc,destloc.register);
              end;
            LOC_MMREGISTER:
              begin
                a_loadmm_loc_reg(list,llvmparadef,vardef,hloc,destloc.register,nil);
              end;
            { TODO other possible locations }
            else
              internalerror(2013102304);
          end;
        end;
    end;


  procedure thlcgllvm.a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel);
    begin
      internalerror(2013060224);
    end;


  procedure thlcgllvm.g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister);
    begin
      internalerror(2013060225);
    end;


  procedure thlcgllvm.g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref: TReference);
    begin
      internalerror(2013060226);
    end;


  procedure thlcgllvm.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tdef; src, dst: tregister);
    begin
      internalerror(2012090201);
    end;


  procedure thlcgllvm.g_stackpointer_alloc(list: TAsmList; size: longint);
    begin
      internalerror(2012090203);
    end;


  procedure thlcgllvm.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    begin
      internalerror(2012090204);
    end;


  procedure thlcgllvm.g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint);
    begin
      internalerror(2012090205);
    end;


  procedure thlcgllvm.g_local_unwind(list: TAsmList; l: TAsmLabel);
    begin
      internalerror(2012090206);
    end;


  function thlcgllvm.make_simple_ref(list: TAsmList; const ref: treference; def: tdef): treference;
    var
      ptrindex: tcgint;
      hreg1,
      hreg2: tregister;
      tmpref: treference;
      defsize: asizeint;
    begin
      { already simple? }
      if (not assigned(ref.symbol) or
          (ref.base=NR_NO)) and
         (ref.index=NR_NO) and
         (ref.offset=0) then
        begin
          result:=ref;
          exit;
        end;

      hreg2:=getaddressregister(list,getpointerdef(def));
      defsize:=def.size;
      { for voiddef/formaldef }
      if defsize=0 then
        defsize:=1;
      { symbol+offset or base+offset with offset a multiple of the size ->
        use getelementptr }
      if (ref.index=NR_NO) and
         (ref.offset mod defsize=0) then
        begin
          ptrindex:=ref.offset div defsize;
          if assigned(ref.symbol) then
            reference_reset_symbol(tmpref,ref.symbol,0,ref.alignment)
          else
            reference_reset_base(tmpref,getpointerdef(def),ref.base,0,ref.alignment);
          list.concat(taillvm.getelementptr_reg_size_ref_size_const(hreg2,getpointerdef(def),tmpref,ptruinttype,ptrindex,assigned(ref.symbol)));
          reference_reset_base(result,getpointerdef(def),hreg2,0,ref.alignment);
          exit;
        end;
      { for now, perform all calculations using plain pointer arithmetic. Later
        we can look into optimizations based on getelementptr for structured
        accesses (if only to prevent running out of virtual registers).

        Assumptions:
          * symbol/base register: always type "def*"
          * index/offset: always type "ptruinttype" (llvm bitcode has no sign information, so sign doesn't matter) }
      hreg1:=getintregister(list,ptruinttype);
      if assigned(ref.symbol) then
        begin
          if ref.base<>NR_NO then
            internalerror(2012111301);
          reference_reset_symbol(tmpref,ref.symbol,0,ref.alignment);
          list.concat(taillvm.getelementptr_reg_size_ref_size_const(hreg1,getpointerdef(def),tmpref,ptruinttype,0,true));
        end
      else if ref.base<>NR_NO then
        begin
          a_load_reg_reg(list,getpointerdef(def),ptruinttype,ref.base,hreg1);
        end
      else
        { todo: support for absolute addresses on embedded platforms }
        internalerror(2012111302);
      if ref.index<>NR_NO then
        begin
          { SSA... }
          hreg2:=getintregister(list,ptruinttype);
          a_op_reg_reg_reg(list,OP_ADD,ptruinttype,ref.index,hreg1,hreg2);
          hreg1:=hreg2;
        end;
      if ref.offset<>0 then
        begin
          hreg2:=getintregister(list,ptruinttype);
          a_op_const_reg_reg(list,OP_ADD,ptruinttype,ref.offset,hreg1,hreg2);
          hreg1:=hreg2;
        end;
      hreg2:=getaddressregister(list,getpointerdef(def));
      a_load_reg_reg(list,ptruinttype,getpointerdef(def),hreg1,hreg2);
      reference_reset_base(result,getpointerdef(def),hreg2,0,ref.alignment);
    end;


  procedure thlcgllvm.set_call_function_result(const list: TAsmList; const pd: tabstractprocdef; const llvmretdef, hlretdef: tdef; const resval: tregister; var retpara: tcgpara);
    var
      rettemp: treference;
    begin
      if not is_void(hlretdef) and
         not paramanager.ret_in_param(hlretdef, pd) then
        begin
          { should already be a copy, because it currently describes the llvm
            return location }
          if not retpara.temporary then
            internalerror(2014020101);
          if llvmaggregatetype(hlretdef) then
            begin
              { to ease the handling of aggregate types here, we just store
                everything to memory rather than potentially dealing with aggregates
                in "registers" }
              tg.gethltemp(list, hlretdef, hlretdef.size, tt_normal, rettemp);
              a_load_reg_ref(list, llvmretdef, hlretdef, resval, rettemp);
              { the return parameter now contains a value whose type matches the one
                that the high level code generator expects instead of the llvm shim
              }
              retpara.def:=hlretdef;
              retpara.location^.def:=hlretdef;
              { for llvm-specific code:  }
              retpara.location^.llvmvalueloc:=false;
              retpara.location^.llvmloc.loc:=LOC_REGISTER;
              retpara.location^.llvmloc.reg:=rettemp.base;
              { for the rest (normally not used, but cleaner to set it correclty) }
              retpara.location^.loc:=LOC_REFERENCE;
              retpara.location^.reference.index:=rettemp.base;
              retpara.location^.reference.offset:=0;
            end
          else
            begin
              retpara.Location^.llvmloc.loc:=retpara.location^.loc;
              retpara.location^.llvmloc.reg:=resval;
              retpara.Location^.llvmvalueloc:=true;
            end;
        end
      else
        retpara.location^.llvmloc.loc:=LOC_VOID;
    end;


  procedure thlcgllvm.paraloctoloc(const paraloc: pcgparalocation; out hloc: tlocation);
    begin
      case paraloc^.llvmloc.loc of
        LOC_REFERENCE:
          begin
            location_reset_ref(hloc,LOC_REFERENCE,def_cgsize(paraloc^.def),paraloc^.def.alignment);
            hloc.reference.symbol:=paraloc^.llvmloc.sym;
            if paraloc^.llvmvalueloc then
              hloc.reference.refaddr:=addr_full;
          end;
        LOC_REGISTER:
          begin
            if paraloc^.llvmvalueloc then
              begin
                location_reset(hloc,LOC_REGISTER,def_cgsize(paraloc^.def));
                hloc.register:=paraloc^.llvmloc.reg;
              end
            else
              begin
                if getregtype(paraloc^.llvmloc.reg)<>R_TEMPREGISTER then
                  internalerror(2014011903);
                location_reset_ref(hloc,LOC_REFERENCE,def_cgsize(paraloc^.def),paraloc^.def.alignment);
                hloc.reference.base:=paraloc^.llvmloc.reg;
              end;
          end;
        LOC_FPUREGISTER,
        LOC_MMREGISTER:
          begin
            if paraloc^.llvmvalueloc then
              begin
                location_reset(hloc,paraloc^.llvmloc.loc,def_cgsize(paraloc^.def));
                hloc.register:=paraloc^.llvmloc.reg;
              end
            else
              internalerror(2014012401);
          end
        else
          internalerror(2014010706);
      end;
    end;


  procedure thlcgllvm.varsym_set_localloc(list: TAsmList; vs: tabstractnormalvarsym);
    begin
      if cs_asm_source in current_settings.globalswitches then
        begin
          case vs.initialloc.loc of
            LOC_REFERENCE :
              begin
                if assigned(vs.initialloc.reference.symbol) then
                  list.concat(Tai_comment.Create(strpnew('Var '+vs.realname+' located at '+
                     vs.initialloc.reference.symbol.name)))
                else
                  list.concat(Tai_comment.Create(strpnew('Var '+vs.realname+' located at %tmp.'+
                     tostr(getsupreg(vs.initialloc.reference.base)))));
              end;
          end;
        end;
      vs.localloc:=vs.initialloc;
      FillChar(vs.currentregloc,sizeof(vs.currentregloc),0);
    end;


  procedure thlcgllvm.paravarsym_set_initialloc_to_paraloc(vs: tparavarsym);
    var
      parasym : tasmsymbol;
    begin
      if vs.paraloc[calleeside].location^.llvmloc.loc<>LOC_REFERENCE then
        internalerror(2014010708);
      parasym:=vs.paraloc[calleeside].location^.llvmloc.sym;
      reference_reset_symbol(vs.initialloc.reference,parasym,0,vs.paraloc[calleeside].alignment);
      if vs.paraloc[calleeside].location^.llvmvalueloc then
        vs.initialloc.reference.refaddr:=addr_full;
    end;


  procedure thlcgllvm.g_external_wrapper(list: TAsmList; procdef: tprocdef; const externalname: string);
    var
      asmsym: TAsmSymbol;
    begin
      asmsym:=current_asmdata.RefAsmSymbol(externalname,AT_FUNCTION);
      list.concat(taillvmalias.create(asmsym,procdef.mangledname,procdef,llv_default,lll_default));
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgllvm.create;
      cgllvm.create_codegen
    end;

begin
  chlcgobj:=thlcgllvm;
end.
