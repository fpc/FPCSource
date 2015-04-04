{
    Copyright (c) 2010, 2013 by Jonas Maebe

    Contains the assembler object for the LLVM target

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
unit aasmllvm;

{$i fpcdefs.inc}

interface

    uses
      globtype,verbose,cclasses,
      aasmbase,aasmtai,aasmdata,aasmsym,
      cpubase,cgbase,cgutils,
      symtype,symdef,symsym,
      llvmbase;

    type
      { taillvm }
      taillvm = class(tai_cpu_abstract_sym)
       private
        procedure maybe_declare(def: tdef; const ref: treference);
       public
        llvmopcode: tllvmop;

        constructor create_llvm(op: tllvmop);

        { e.g. ret void }
        constructor op_size(op : tllvmop; size: tdef);

        { e.g. dst = alloca size }
        constructor op_reg_size(op:tllvmop;dst:tregister;size:tdef);
        { e.g. dst = alloca size }
        constructor op_ref_size(op:tllvmop;const dst:treference;size:tdef);

        { e.g. dst = add size src1, src2 }
        constructor op_reg_size_reg_reg(op:tllvmop;dst:tregister;size:tdef;src1,src2:tregister);
        { e.g. dst = shl size src1, 1 ( = src1 shl 1) }
        constructor op_reg_size_reg_const(op:tllvmop;dst:tregister;size:tdef;src1:tregister;src2:int64);
        { e.g. dst = sub size 0, src2 ( = 0 - src2) }
        constructor op_reg_size_const_reg(op:tllvmop;dst:tregister;size:tdef;src1:int64;src2:tregister);
        { e.g. dst = bitcast size1 src to tosize }
        constructor op_reg_size_reg_size(op:tllvmop;dst:tregister;fromsize:tdef;src:tregister;tosize:tdef);
        { e.g. dst = bitcast fromsize 255 to tosize }
        constructor op_reg_size_const_size(op:tllvmop;dst:tregister;fromsize:tdef;src:int64;tosize:tdef);
        { e.g. dst = bitcast fromsize double to tosize }
        constructor op_reg_size_fpconst_size(op:tllvmop;dst:tregister;fromsize:tdef;src:double;tosize:tdef);
{$ifdef cpuextended}
        { e.g. dst = bitcast fromsize extended to tosize }
        constructor op_reg_size_fpconst80_size(op:tllvmop;dst:tregister;fromsize:tdef;src:extended;tosize:tdef);
{$endif cpuextended}
        { e.g. dst = bitcast fromsize @globalvar to tosize }
        constructor op_reg_size_sym_size(op:tllvmop;dst:tregister;fromsize:tdef;src:TAsmSymbol;tosize:tdef);
        { e.g. dst = bitcast fromsize <abstracttaidata> to tosize }
        constructor op_reg_tai_size(op:tllvmop;dst:tregister;src:tai;tosize:tdef);

        { e.g. dst = bitcast fromsize src to tosize }
        constructor op_reg_size_ref_size(op:tllvmop;dst:tregister;fromsize:tdef;const src:treference;tosize:tdef);
        { e.g. store fromsize src, ptrsize toref}
        constructor op_size_reg_size_ref(op:tllvmop;fromsize:tdef;src:tregister;ptrsize:tdef;const toref:treference);
        { e.g. store fromsize srcref, ptrsize toref (with srcref.refaddr=full) }
        constructor op_size_ref_size_ref(op:tllvmop;fromsize:tdef;const src:treference;ptrsize:tdef;const toref:treference);
        { e.g. store fromsize const, ptrsize toref}
        constructor op_size_const_size_ref(op:tllvmop;fromsize:tdef;src:int64;ptrsize:tdef;const toref:treference);
        { e.g. dst = load fromsize fromref }
        constructor op_reg_size_ref(op:tllvmop;dst:tregister;fromsize:tdef;const fromref:treference);

        { e.g. dst = icmp cmpcond size reg1, reg2 }
        constructor op_reg_cond_size_reg_reg(op:tllvmop;dst:tregister;cmpcond:topcmp;size:tdef;reg1,reg2:tregister);
        { e.g. dst = icmp cmpcond size reg1, constant }
        constructor op_reg_cond_size_reg_const(op:tllvmop;dst:tregister;cmpcond:topcmp;size:tdef;reg1:tregister;cnst:int64);
        { e.g. dst = fcmp cmpcond size reg1, reg2 }
        constructor op_reg_fpcond_size_reg_reg(op:tllvmop;dst:tregister;cmpcond:tllvmfpcmp;size:tdef;reg1,reg2:tregister);
        { e.g. br label lab }
        constructor op_lab(op:tllvmop;lab:tasmlabel);
        { e.g. br i1 condreg, label iftrue, label iffalse }
        constructor op_size_reg_lab_lab(op:tllvmop;fromsize:tdef;condreg:tregister;labtrue,labfalse: tasmlabel);

        { e.g. la_ret retdef retval }
        constructor op_size_reg(op:tllvmop;def: tdef;reg: tregister);

        { e.g. dst = getelementptr ptrsize ref, i32 0 (if indirect), index1type index1 }
        constructor getelementptr_reg_size_ref_size_reg(dst:tregister;ptrsize:tdef;const ref:treference;indextype:tdef;index1:tregister;indirect:boolean);
        constructor getelementptr_reg_size_ref_size_const(dst:tregister;ptrsize:tdef;const ref:treference;indextype:tdef;index1:ptrint;indirect:boolean);
        constructor getelementptr_reg_tai_size_const(dst:tregister;const ai:tai;indextype:tdef;index1:ptrint;indirect:boolean);

        { e.g. dst = call retsize name (paras) }
        constructor call_size_name_paras(dst: tregister;retsize: tdef;name:tasmsymbol;paras: tfplist);
        { e.g. dst = call retsize reg (paras) }
        constructor call_size_reg_paras(dst: tregister;retsize: tdef;reg:tregister;paras: tfplist);

        procedure loadoper(opidx: longint; o: toper); override;
        procedure clearop(opidx: longint); override;
        procedure loadtai(opidx: longint; _ai: tai);
        procedure loaddef(opidx: longint; _def: tdef);
        procedure loadsingle(opidx: longint; _sval: single);
        procedure loaddouble(opidx: longint; _dval: double);
{$ifdef cpuextended}
        procedure loadextended(opidx: longint; _eval: extended);
{$endif cpuextended}
        procedure loadcond(opidx: longint; _cond: topcmp);
        procedure loadfpcond(opidx: longint; _fpcond: tllvmfpcmp);
        procedure loadparas(opidx: longint; _paras: tfplist);

        { register spilling code }
        function spilling_get_operation_type(opnr: longint): topertype;override;
        function spilling_get_reg_type(opnr: longint): tdef;
      end;


    tllvmvisibility = (llv_default, llv_hidden, llv_protected);

    tllvmlinkage = (
      { llvm 2.5 }
      lll_default { = externally visible/global },
      lll_private, lll_internal, lll_linkonce, lll_common,
      lll_weak, lll_appending, lll_extern_weak,
      lll_dllimport, lll_dllexport,
      { llvm 2.6+ }
      lll_linker_private, lll_private_weak, lll_private_weak_def_auto,
      lll_available_externally,lll_linkonce_odr, lll_weak_odr
      );

    taillvmalias = class(tailineinfo)
      vis: tllvmvisibility;
      linkage: tllvmlinkage;
      oldsym, newsym: TAsmSymbol;
      def: tdef;
      constructor create(_oldsym: tasmsymbol; const newname: TSymStr; _def: tdef; _vis: tllvmvisibility; _linkage: tllvmlinkage);
    end;

    { declarations/definitions of symbols (procedures, variables), both defined
      here and external }
    taillvmdecl = class(tai)
      { initialisation data, if any }
      initdata: tasmlist;
      namesym: tasmsymbol;
      def: tdef;
      sec: TAsmSectiontype;
      alignment: shortint;
      tls: boolean;
      constructor create(_namesym: tasmsymbol; _def: tdef; _initdata: tasmlist; _sec: tasmsectiontype; _alignment: shortint);
      constructor createtls(_namesym: tasmsymbol; _def: tdef; _alignment: shortint);
      destructor destroy; override;
    end;

    { parameter to an llvm call instruction }
    pllvmcallpara = ^tllvmcallpara;
    tllvmcallpara = record
      def: tdef;
      valueext: tllvmvalueextension;
      case loc: tcgloc of
        LOC_REFERENCE,
        LOC_REGISTER,
        LOC_FPUREGISTER,
        LOC_MMREGISTER: (reg: tregister);
    end;


implementation

uses
  cutils, strings,
  symconst,
  aasmcpu;

    { taillvmprocdecl }

    constructor taillvmdecl.create(_namesym: tasmsymbol; _def: tdef; _initdata: tasmlist; _sec: tasmsectiontype; _alignment: shortint);
      begin
        inherited create;
        typ:=ait_llvmdecl;
        namesym:=_namesym;
        def:=_def;
        initdata:=_initdata;
        sec:=_sec;
        alignment:=_alignment;
        _namesym.declared:=true;
      end;


    constructor taillvmdecl.createtls(_namesym: tasmsymbol; _def: tdef; _alignment: shortint);
      begin
        create(_namesym,_def,nil,sec_data,_alignment);
        tls:=true;
      end;


    destructor taillvmdecl.destroy;
      begin
        initdata.free;
        inherited destroy;
      end;

    { taillvmalias }

    constructor taillvmalias.create(_oldsym: tasmsymbol; const newname: TSymStr; _def: tdef; _vis: tllvmvisibility; _linkage: tllvmlinkage);
      begin
        inherited Create;
        typ:=ait_llvmalias;
        oldsym:=_oldsym;
        newsym:=current_asmdata.DefineAsmSymbol(newname,AB_GLOBAL,AT_FUNCTION);
        def:=_def;
        vis:=_vis;
        linkage:=_linkage;
      end;




{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    procedure taillvm.maybe_declare(def: tdef; const ref: treference);
      begin
        { add llvm declarations for imported symbols }
        if not assigned(ref.symbol) or
           (ref.symbol.declared) or
           not(ref.symbol.bind in [AB_EXTERNAL,AB_WEAK_EXTERNAL]) then
          exit;
        if ref.refaddr<>addr_full then
          begin
            if def.typ<>pointerdef then
              internalerror(2014020701);
            def:=tpointerdef(def).pointeddef;
          end;
        current_asmdata.AsmLists[al_imports].concat(taillvmdecl.create(ref.symbol,def,nil,sec_none,def.alignment));
      end;


    constructor taillvm.create_llvm(op: tllvmop);
      begin
        create(a_none);
        llvmopcode:=op;
        typ:=ait_llvmins;
      end;


    procedure taillvm.loadoper(opidx: longint; o: toper);
      var
        i: longint;
        callpara: pllvmcallpara;
      begin
        inherited;
        if o.typ=top_para then
          begin
            oper[opidx]^.paras:=tfplist.create;
            for i:=0 to o.paras.count-1 do
              begin
                new(callpara);
                callpara^:=pllvmcallpara(o.paras[i])^;
                oper[opidx]^.paras.add(callpara);
                if (callpara^.loc in [LOC_REGISTER,LOC_FPUREGISTER,LOC_MMREGISTER]) and
                   assigned(add_reg_instruction_hook) then
                  add_reg_instruction_hook(self,callpara^.reg);
              end;
          end;
      end;


    procedure taillvm.clearop(opidx: longint);
      var
        i: longint;
      begin
        case oper[opidx]^.typ of
          top_para:
            begin
              for i:=0 to oper[opidx]^.paras.count-1 do
                dispose(pllvmcallpara(oper[opidx]^.paras[i]));
              oper[opidx]^.paras.free;
            end;
          top_tai:
            oper[opidx]^.ai.free;
        end;
        inherited;
      end;


    procedure taillvm.loadtai(opidx: longint; _ai: tai);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           clearop(opidx);
           ai:=_ai;
           typ:=top_tai;
         end;
      end;


    procedure taillvm.loaddef(opidx:longint;_def: tdef);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_def then
             clearop(opidx);
           def:=_def;
           typ:=top_def;
         end;
      end;


    procedure taillvm.loadsingle(opidx: longint; _sval: single);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_single then
             clearop(opidx);
           sval:=_sval;
           typ:=top_single;
         end;
      end;


    procedure taillvm.loaddouble(opidx: longint; _dval: double);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_double then
             clearop(opidx);
           dval:=_dval;
           typ:=top_double;
         end;
      end;


{$ifdef cpuextended}
    procedure taillvm.loadextended(opidx: longint; _eval: extended);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_extended80 then
             clearop(opidx);
           eval:=_eval;
           typ:=top_extended80;
         end;
      end;
{$endif cpuextended}


    procedure taillvm.loadcond(opidx: longint; _cond: topcmp);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_cond then
             clearop(opidx);
           cond:=_cond;
           typ:=top_cond;
         end;
      end;

    procedure taillvm.loadfpcond(opidx: longint; _fpcond: tllvmfpcmp);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_fpcond then
             clearop(opidx);
           fpcond:=_fpcond;
           typ:=top_fpcond;
         end;
      end;


    procedure taillvm.loadparas(opidx: longint; _paras: tfplist);
      var
        callpara: pllvmcallpara;
        i: longint;
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            clearop(opidx);
            paras:=_paras;
            for i:=0 to _paras.count-1 do
              begin
                callpara:=pllvmcallpara(_paras[i]);
                if (callpara^.loc in [LOC_REGISTER,LOC_FPUREGISTER,LOC_MMREGISTER]) and
                   assigned(add_reg_instruction_hook) then
                  add_reg_instruction_hook(self,callpara^.reg);
              end;
            typ:=top_para;
          end;
      end;


    function taillvm.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case llvmopcode of
          la_ret, la_br, la_switch, la_indirectbr,
          la_invoke, la_resume,
          la_unreachable,
          la_store,
          la_fence,
          la_cmpxchg,
          la_atomicrmw:
            begin
              { instructions that never have a result }
              result:=operand_read;
            end;
          la_alloca,
          la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
          la_fptoui, la_fptosi, la_uitofp, la_sitofp,
          la_ptrtoint, la_inttoptr,
          la_bitcast,
          la_add, la_fadd, la_sub, la_fsub, la_mul, la_fmul,
          la_udiv,la_sdiv, la_fdiv, la_urem, la_srem, la_frem,
          la_shl, la_lshr, la_ashr, la_and, la_or, la_xor,
          la_extractelement, la_insertelement, la_shufflevector,
          la_extractvalue, la_insertvalue,
          la_getelementptr,
          la_load,
          la_icmp, la_fcmp,
          la_phi, la_select, la_call,
          la_va_arg, la_landingpad:
            begin
              if opnr=0 then
                result:=operand_write
              else
                result:=operand_read;
            end;
          else
            internalerror(2013103101)
        end;
      end;


    function taillvm.spilling_get_reg_type(opnr: longint): tdef;
      begin
        case llvmopcode of
          la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
          la_fptoui, la_fptosi, la_uitofp, la_sitofp,
          la_ptrtoint, la_inttoptr,
          la_bitcast:
            begin
              { toreg = bitcast fromsize fromreg to tosize }
              case opnr of
                0: result:=oper[3]^.def;
                2: result:=oper[1]^.def
                else
                  internalerror(2013103102);
              end;
            end;
          la_ret, la_switch, la_indirectbr,
          la_resume:
            begin
              { ret size reg }
              if opnr=1 then
                result:=oper[0]^.def
              else
                internalerror(2013110101);
            end;
          la_invoke, la_call:
            begin
              if opnr=0 then
                result:=oper[1]^.def
              else
                internalerror(2013110102);
            end;
          la_br,
          la_unreachable:
            internalerror(2013110103);
          la_store:
            begin
              case opnr of
                1: result:=oper[0]^.def;
                { type of the register in the reference }
                3: result:=oper[2]^.def;
                else
                  internalerror(2013110104);
              end;
            end;
          la_load,
          la_getelementptr:
            begin
              { dst = load ptrdef srcref }
              case opnr of
                0: result:=tpointerdef(oper[1]^.def).pointeddef;
                2: result:=oper[1]^.def;
                else
                  internalerror(2013110105);
              end;
            end;
          la_fence,
          la_cmpxchg,
          la_atomicrmw:
            begin
              internalerror(2013110610);
            end;
          la_add, la_fadd, la_sub, la_fsub, la_mul, la_fmul,
          la_udiv,la_sdiv, la_fdiv, la_urem, la_srem, la_frem,
          la_shl, la_lshr, la_ashr, la_and, la_or, la_xor:
            begin
              case opnr of
                0,2,3:
                  result:=oper[1]^.def;
                else
                  internalerror(2013110106);
              end;
            end;
          la_extractelement, la_insertelement, la_shufflevector,
          la_extractvalue:
            begin
              { todo }
              internalerror(2013110107);
            end;
          la_insertvalue:
            begin
              case opnr of
                0,2: result:=oper[1]^.def;
                else
                  internalerror(2013110108);
              end;
            end;
          la_icmp, la_fcmp:
            begin
              case opnr of
                0: result:=pasbool8type;
                3,4: result:=oper[2]^.def;
                else
                  internalerror(2013110801);
              end
            end;
          la_alloca:
            begin
              { shouldn't be spilled, the result of alloca should be read-only }
              internalerror(2013110109);
            end;
          la_select:
            begin
              case opnr of
                0,4,6: result:=oper[3]^.def;
                2: result:=oper[1]^.def;
                else
                  internalerror(2013110110);
              end;
            end;
          else
            internalerror(2013103101)
        end;
      end;


    constructor taillvm.op_size(op : tllvmop; size: tdef);
      begin
        create_llvm(op);
        ops:=1;
        loaddef(0,size);
      end;


    constructor taillvm.op_reg_size(op: tllvmop; dst: tregister; size: tdef);
      begin
        create_llvm(op);
        ops:=2;
        loadreg(0,dst);
        loaddef(1,size);
      end;


    constructor taillvm.op_ref_size(op: tllvmop; const dst: treference; size: tdef);
      begin
        create_llvm(op);
        ops:=2;
        loadref(0,dst);
        loaddef(1,size);
      end;


    { %dst = add i32 %src1, %src2 }
    constructor taillvm.op_reg_size_reg_reg(op: tllvmop; dst: tregister;size: tdef; src1, src2: tregister);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,size);
        loadreg(2,src1);
        loadreg(3,src2);
      end;

    { %dst = shl i32 %reg, 1 (= %reg shl 1) }
    constructor taillvm.op_reg_size_reg_const(op: tllvmop; dst: tregister; size: tdef; src1: tregister; src2: int64);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,size);
        loadreg(2,src1);
        loadconst(3,src2);
      end;


    { %dst = sub i32 1, %src (= 1 - %src) }
    constructor taillvm.op_reg_size_const_reg(op: tllvmop; dst: tregister; size: tdef; src1: int64; src2: tregister);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,size);
        loadconst(2,src1);
        loadreg(3,src2);
      end;


    { %dst = bitcast i32 %src to i8 }
    constructor taillvm.op_reg_size_reg_size(op: tllvmop; dst: tregister; fromsize: tdef; src: tregister; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        loadreg(2,src);
        loaddef(3,tosize);
      end;


    { %dst = bitcast i32 -1 to i8 }
    constructor taillvm.op_reg_size_const_size(op: tllvmop; dst: tregister; fromsize: tdef; src: int64; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        loadconst(2,src);
        loaddef(3,tosize);
      end;


    constructor taillvm.op_reg_size_fpconst_size(op: tllvmop; dst: tregister; fromsize: tdef; src: double; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        if fromsize.typ<>floatdef then
          internalerror(2014012214);
        case tfloatdef(fromsize).floattype of
          s32real:
            loadsingle(2,src);
          s64real:
            loaddouble(2,src);
          else
            internalerror(2014012215);
        end;
        loaddef(3,tosize);
      end;

{$ifdef cpuextended}
    constructor taillvm.op_reg_size_fpconst80_size(op: tllvmop; dst: tregister; fromsize: tdef; src: extended; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        loadextended(2,src);
        loaddef(3,tosize);
      end;
{$endif cpuextended}


    constructor taillvm.op_reg_size_sym_size(op: tllvmop; dst: tregister; fromsize: tdef; src: TAsmSymbol; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        loadsymbol(2,src,0);
        loaddef(3,tosize);
      end;


    constructor taillvm.op_reg_tai_size(op:tllvmop;dst:tregister;src:tai;tosize:tdef);
      begin
        create_llvm(op);
        ops:=3;
        loadreg(0,dst);
        loadtai(1,src);
        loaddef(2,tosize);
      end;


    constructor taillvm.op_reg_size_ref_size(op: tllvmop; dst: tregister; fromsize: tdef; const src: treference; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        maybe_declare(fromsize,src);
        loaddef(1,fromsize);
        loadref(2,src);
        loaddef(3,tosize);
      end;


    { store i32 3, i32* %ptr }
    constructor taillvm.op_size_reg_size_ref(op: tllvmop; fromsize: tdef; src: tregister; ptrsize: tdef; const toref: treference);
      begin
        create_llvm(op);
        ops:=4;
        loaddef(0,fromsize);
        loadreg(1,src);
        maybe_declare(ptrsize,toref);
        loaddef(2,ptrsize);
        loadref(3,toref);
      end;


    constructor taillvm.op_size_ref_size_ref(op: tllvmop; fromsize: tdef; const src: treference; ptrsize: tdef; const toref: treference);
      begin
        create_llvm(op);
        ops:=4;
        maybe_declare(fromsize,src);
        loaddef(0,fromsize);
        loadref(1,src);
        maybe_declare(ptrsize,toref);
        loaddef(2,ptrsize);
        loadref(3,toref);
      end;


    constructor taillvm.op_size_const_size_ref(op: tllvmop; fromsize: tdef; src: int64; ptrsize: tdef; const toref: treference);
      begin
        create_llvm(op);
        ops:=4;
        loaddef(0,fromsize);
        loadconst(1,src);
        maybe_declare(ptrsize,toref);
        loaddef(2,ptrsize);
        loadref(3,toref);
      end;


    constructor taillvm.op_reg_size_ref(op: tllvmop; dst: tregister; fromsize: tdef; const fromref: treference);
      begin
        create_llvm(op);
        ops:=3;
        loadreg(0,dst);
        maybe_declare(fromsize,fromref);
        loaddef(1,fromsize);
        loadref(2,fromref);
      end;


    constructor taillvm.op_reg_cond_size_reg_reg(op: tllvmop; dst: tregister; cmpcond: topcmp; size: tdef; reg1, reg2: tregister);
      begin
        create_llvm(op);
        ops:=5;
        loadreg(0,dst);
        loadcond(1,cmpcond);
        loaddef(2,size);
        loadreg(3,reg1);
        loadreg(4,reg2);
      end;

    constructor taillvm.op_reg_cond_size_reg_const(op: tllvmop; dst: tregister; cmpcond: topcmp; size: tdef; reg1: tregister; cnst: int64);
      begin
        create_llvm(op);
        ops:=5;
        loadreg(0,dst);
        loadcond(1,cmpcond);
        loaddef(2,size);
        loadreg(3,reg1);
        loadconst(4,cnst);
      end;

    constructor taillvm.op_reg_fpcond_size_reg_reg(op: tllvmop; dst: tregister; cmpcond: tllvmfpcmp; size: tdef; reg1, reg2: tregister);
      begin
        create_llvm(op);
        ops:=5;
        loadreg(0,dst);
        loadfpcond(1,cmpcond);
        loaddef(2,size);
        loadreg(3,reg1);
        loadreg(4,reg2);
      end;


    constructor taillvm.op_lab(op: tllvmop; lab: tasmlabel);
      begin
        create_llvm(op);
        ops:=1;
        loadsymbol(0,lab,0);
      end;


    constructor taillvm.op_size_reg_lab_lab(op: tllvmop; fromsize: tdef; condreg: tregister; labtrue, labfalse: tasmlabel);
      begin
        create_llvm(op);
        ops:=4;
        loaddef(0,fromsize);
        loadreg(1,condreg);
        loadsymbol(2,labtrue,0);
        loadsymbol(3,labfalse,0);
      end;


    constructor taillvm.op_size_reg(op: tllvmop; def: tdef; reg: tregister);
      begin
        create_llvm(op);
        ops:=2;
        loaddef(0,def);
        loadreg(1,reg);
      end;


    constructor taillvm.getelementptr_reg_size_ref_size_reg(dst: tregister; ptrsize: tdef; const ref: treference; indextype: tdef; index1: tregister; indirect: boolean);
      var
        index: longint;
      begin
        create_llvm(la_getelementptr);
        if indirect then
          ops:=7
        else
          ops:=5;
        loadreg(0,dst);
        maybe_declare(ptrsize,ref);
        loaddef(1,ptrsize);
        loadref(2,ref);
        if indirect then
          begin
            loaddef(3,s32inttype);
            loadconst(4,0);
            index:=5;
          end
        else
          index:=3;
        loaddef(index,indextype);
        loadreg(index+1,index1);
      end;


    constructor taillvm.getelementptr_reg_size_ref_size_const(dst: tregister; ptrsize: tdef; const ref: treference; indextype: tdef; index1: ptrint; indirect: boolean);
      var
        index: longint;
      begin
        create_llvm(la_getelementptr);
        if indirect then
          ops:=7
        else
          ops:=5;
        loadreg(0,dst);
        maybe_declare(ptrsize,ref);
        loaddef(1,ptrsize);
        loadref(2,ref);
        if indirect then
          begin
            loaddef(3,s32inttype);
            loadconst(4,0);
            index:=5;
          end
        else
          index:=3;
        loaddef(index,indextype);
        loadconst(index+1,index1);
      end;


    constructor taillvm.getelementptr_reg_tai_size_const(dst: tregister; const ai: tai; indextype: tdef; index1: ptrint; indirect: boolean);
      var
        index: longint;
      begin
        create_llvm(la_getelementptr);
        if indirect then
          ops:=6
        else
          ops:=4;
        loadreg(0,dst);
        loadtai(1,ai);
        if indirect then
          begin
            loaddef(2,s32inttype);
            loadconst(3,0);
            index:=4;
          end
        else
          index:=2;
        loaddef(index,indextype);
        loadconst(index+1,index1);
      end;


    constructor taillvm.call_size_name_paras(dst: tregister; retsize: tdef; name:tasmsymbol; paras: tfplist);
      begin
        create_llvm(la_call);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,retsize);
        loadsymbol(2,name,0);
        loadparas(3,paras);
      end;


    constructor taillvm.call_size_reg_paras(dst: tregister; retsize: tdef; reg: tregister; paras: tfplist);
      begin
        create_llvm(la_call);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,retsize);
        loadreg(2,reg);
        loadparas(3,paras);
      end;

end.
