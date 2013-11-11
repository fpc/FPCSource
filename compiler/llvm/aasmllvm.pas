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
      globtype,verbose,
      aasmbase,aasmtai,aasmdata,aasmsym,
      cpubase,cgbase,cgutils,
      symtype,symdef,symsym,
      llvmbase;

    type
      { taillvm }
      taillvm = class(tai_cpu_abstract_sym)
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

        { e.g. dst = bitcast fromsize src to tosize }
        constructor op_reg_size_ref_size(op:tllvmop;dst:tregister;fromsize:tdef;const src:treference;tosize:tdef);
        { e.g. store fromsize src, ptrsize toref}
        constructor op_size_reg_size_ref(op:tllvmop;fromsize:tdef;src:tregister;ptrsize:tdef;const toref:treference);
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

        { e.g. dst = getelementptr ptrsize ref, i32 0 (implicit), index1type index1 }
        constructor getelementptr_reg_size_ref_size_reg(dst:tregister;ptrsize:tdef;const ref:treference;indextype: tdef;index1:tregister);
        constructor getelementptr_reg_size_ref_size_const(dst:tregister;ptrsize:tdef;const ref:treference;indextype: tdef;index1:ptrint);

        procedure loaddef(opidx: longint; _def: tdef);
        procedure loaddouble(opidx: longint; _dval: double);
{$ifdef cpuextended}
        procedure loadextended(opidx: longint; _eval: extended);
{$endif cpuextended}
        procedure loadcond(opidx: longint; _cond: topcmp);
        procedure loadfpcond(opidx: longint; _fpcond: tllvmfpcmp);
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
      constructor Create(const oldname, newname: TSymStr; _def: tdef; _vis: tllvmvisibility; _linkage: tllvmlinkage);
    end;

    { start of a procedure }
    taillvmprocdef = class(tailineinfo)
      procdef: tprocdef;
      constructor create(_procdef: tprocdef);
    end;

    { global variable definition }
    taillvmvarsym = class(tailineinfo)
      varsym: tstaticvarsym;
      constructor create(_varsym: tstaticvarsym);
    end;


implementation

uses
  cutils, cclasses, strings, aasmcpu;

    { taillvmvarsym }

    constructor taillvmvarsym.create(_varsym: tstaticvarsym);
      begin
        inherited create;
        typ:=ait_llvmvarsym;
        varsym:=_varsym;
      end;

    { taillvmalias }

    constructor taillvmalias.Create(const oldname, newname: TSymStr; _def: tdef; _vis: tllvmvisibility; _linkage: tllvmlinkage);
      begin
        inherited Create;
        oldsym:=current_asmdata.RefAsmSymbol(oldname,AT_FUNCTION);
        newsym:=current_asmdata.DefineAsmSymbol(newname,AB_GLOBAL,AT_FUNCTION);
        def:=_def;
        vis:=_vis;
        linkage:=_linkage;
      end;

    { taillvmprocdef }
    constructor taillvmprocdef.create(_procdef: tprocdef);
      begin
        inherited create;
        typ:=ait_llvmprocdef;
        procdef:=_procdef;
      end;




{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taillvm.create_llvm(op: tllvmop);
      begin
        create(a_none);
        llvmopcode:=op;
        typ:=ait_llvmins;
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
        loaddouble(2,src);
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


    { store i32 3, i32* %ptr }
    constructor taillvm.op_reg_size_ref_size(op: tllvmop; dst: tregister; fromsize: tdef; const src: treference; tosize: tdef);
      begin
        create_llvm(op);
        ops:=4;
        loadreg(0,dst);
        loaddef(1,fromsize);
        loadref(2,src);
        loaddef(3,tosize);
      end;


    constructor taillvm.op_size_reg_size_ref(op: tllvmop; fromsize: tdef; src: tregister; ptrsize: tdef; const toref: treference);
      begin
        create_llvm(op);
        ops:=4;
        loaddef(0,fromsize);
        loadreg(1,src);
        loaddef(2,ptrsize);
        loadref(3,toref);
      end;


    constructor taillvm.op_size_const_size_ref(op: tllvmop; fromsize: tdef; src: int64; ptrsize: tdef; const toref: treference);
      begin
        create_llvm(op);
        ops:=4;
        loaddef(0,fromsize);
        loadconst(1,src);
        loaddef(2,ptrsize);
        loadref(3,toref);
      end;


    constructor taillvm.op_reg_size_ref(op: tllvmop; dst: tregister; fromsize: tdef; const fromref: treference);
      begin
        create_llvm(op);
        ops:=3;
        loadreg(0,dst);
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


    constructor taillvm.getelementptr_reg_size_ref_size_reg(dst: tregister; ptrsize: tdef; const ref: treference; indextype: tdef; index1: tregister);
      begin
        create_llvm(la_getelementptr);
        ops:=7;
        loadreg(0,dst);
        loaddef(1,ptrsize);
        loadref(2,ref);
        loaddef(3,s32inttype);
        loadconst(4,0);
        loaddef(5,indextype);
        loadreg(6,index1);
      end;


    constructor taillvm.getelementptr_reg_size_ref_size_const(dst: tregister; ptrsize: tdef; const ref: treference; indextype: tdef; index1: ptrint);
      begin
        create_llvm(la_getelementptr);
        ops:=7;
        loadreg(0,dst);
        loaddef(1,ptrsize);
        loadref(2,ref);
        loaddef(3,s32inttype);
        loadconst(4,0);
        loaddef(5,indextype);
        loadconst(6,index1);
      end;

end.
