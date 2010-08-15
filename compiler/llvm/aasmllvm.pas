{
    Copyright (c) 1999-2002 by Jonas Maebe

    Contains the assembler object for the PowerPC

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
      aasmbase,aasmtai,aasmdata,
      cpubase,cgbase,cgutils,
      symtype,symdef,
      llvmbase;

    type

      { taillvm }

      taillvm = class(tai_cpu_abstract)
        llvmopcode: tllvmop;
        size1, size2: tllvmopsize;
        cond: topcmp;

        constructor create_llvm(op: tllvmop);

        constructor op_none(op : tllvmop);

        { e.g. dst = add size src1, src2 }
        constructor op_reg_size_reg_reg(op:tllvmop;dst:tregister;size:tllvmopsize;src1,src2:tregister);
        { e.g. dst = sub size 0, src2 }
        constructor op_reg_size_const_reg(op:tllvmop;dst:tregister;size:tllvmopsize;src1:aint;src2:tregister);
        { e.g. dst = bitcast size1 src to tosize }
        constructor op_reg_size_reg_size(op:tllvmop;dst:tregister;fromsize:tllvmopsize;src:tregister;tosize:tllvmopsize);
        { e.g. dst = bitcast fromsize 255 to tosize }
        constructor op_reg_size_const_size(op:tllvmop;dst:tregister;fromsize:tllvmopsize;src:aint;tosize:tllvmopsize);

        { e.g. dst = bitcast fromsize src to tosize }
        constructor op_reg_size_ref_size(op:tllvmop;dst:tregister;fromsize:tllvmopsize;const src:treference;tosize:tllvmopsize);
        { e.g. store fromsize src, ptrsize toref}
        constructor op_size_reg_size_ref(op:tllvmop;fromsize:tllvmopsize;src:tregister;ptrsize:tllvmopsize;const toref:treference);
        { e.g. dst = load presize fromref }
        constructor op_reg_size_ref(op:tllvmop;dst:tregister;fromsize:tllvmopsize;const fromref:treference);

        { e.g. dst = icmp cmpcond size reg1, reg2 }
        constructor op_reg_cond_size_reg_reg(op:tllvmop;dst:tregister;cmpcond:topcmp;size:tllvmopsize;reg1,reg2:tregister);
        { e.g. br label lab }
        constructor op_lab(op:tllvmop;lab:tasmlabel);
        { e.g. br i1 condreg, label iftrue, label iffalse }
        constructor op_size_reg_lab_lab(op:tllvmop;fromsize:tllvmopsize;condreg:tregister;labtrue,labfalse: tasmlabel);

        { e.g. la_ret retdef retval }
        constructor op_def_reg(op:tllvmop;def: tdef;reg: tregister);

        { e.g. dst = getelementptr ptrsize ref, index1type index1 }
        constructor getelementptr_reg_size_ref_size_reg(dst:tregister;ptrsize:tllvmopsize;const ref:treference;indextype: tllvmopsize;index1:tregister);

        { older variants, will be removed }
        constructor op_ressym_string(op: tllvmop; restyp: tasmsymbol; const str: ansistring);

        procedure loadstring(opidx:longint;_str: pchar);
        procedure loaddef(opidx:longint;_def: tdef);
      end;
(*
    procedure InitLlvmAsm;
    procedure DoneLlvmAsm;
*)


implementation

uses
  cutils, cclasses, strings, aasmcpu;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taillvm.create_llvm(op: tllvmop);
      begin
        create(a_none);
        llvmopcode:=op;
        typ:=ait_llvmins;
      end;


    procedure taillvm.loadstring(opidx:longint;_str: pchar);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           clearop(opidx);
           str:=strnew(_str);
           typ:=top_string;
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


    constructor taillvm.op_ressym_string(op: tllvmop; restyp: tasmsymbol; const str: ansistring);
      begin
        create_llvm(op);
        ops:=2;
        loadsymbol(0,restyp,0);
        loadstring(1,pchar(str));
      end;


    constructor taillvm.op_none(op : tllvmop);
      begin
        create_llvm(op);
        ops:=0;
      end;


    constructor taillvm.op_reg_size_reg_reg(op: tllvmop; dst: tregister;size: tllvmopsize; src1, src2: tregister);
      begin
        create_llvm(op);
        ops:=2;
        size1:=size;
        loadreg(0,dst);
        loadreg(1,src1);
        loadreg(2,src2);
      end;


    constructor taillvm.op_reg_size_const_reg(op: tllvmop; dst: tregister; size: tllvmopsize; src1: aint; src2: tregister);
      begin
        create_llvm(op);
        ops:=2;
        size1:=size;
        loadreg(0,dst);
        loadconst(1,src1);
        loadreg(2,src2);
      end;


    constructor taillvm.op_reg_size_reg_size(op: tllvmop; dst: tregister; fromsize: tllvmopsize; src: tregister; tosize: tllvmopsize);
      begin
        create_llvm(op);
        ops:=2;
        size1:=fromsize;
        size2:=tosize;
        loadreg(0,dst);
        loadreg(1,src);
      end;


    constructor taillvm.op_reg_size_const_size(op: tllvmop; dst: tregister; fromsize: tllvmopsize; src: aint; tosize: tllvmopsize);
      begin
        create_llvm(op);
        ops:=2;
        size1:=fromsize;
        size2:=tosize;
        loadreg(0,dst);
        loadconst(1,src);
      end;


    constructor taillvm.op_reg_size_ref_size(op: tllvmop; dst: tregister; fromsize: tllvmopsize; const src: treference; tosize: tllvmopsize);
      begin
        create_llvm(op);
        ops:=2;
        size1:=fromsize;
        size2:=tosize;
        loadreg(0,dst);
        loadref(1,src);
      end;


    constructor taillvm.op_size_reg_size_ref(op: tllvmop; fromsize: tllvmopsize; src: tregister; ptrsize: tllvmopsize; const toref: treference);
      begin
        create_llvm(op);
        ops:=2;
        size1:=fromsize;
        size2:=ptrsize;
        loadreg(0,src);
        loadref(1,toref);
      end;


    constructor taillvm.op_reg_size_ref(op: tllvmop; dst: tregister; fromsize: tllvmopsize; const fromref: treference);
      begin
        create_llvm(op);
        ops:=2;
        size1:=fromsize;
        loadreg(0,dst);
        loadref(1,fromref);
      end;


    constructor taillvm.op_reg_cond_size_reg_reg(op: tllvmop; dst: tregister; cmpcond: topcmp; size: tllvmopsize; reg1, reg2: tregister);
      begin
        create_llvm(op);
        ops:=3;
        size1:=size;
        cond:=cmpcond;
        loadreg(0,dst);
        loadreg(1,reg1);
        loadreg(2,reg2);
      end;


    constructor taillvm.op_lab(op: tllvmop; lab: tasmlabel);
      begin
        create_llvm(op);
        ops:=1;
        loadsymbol(0,lab,0);
      end;


    constructor taillvm.op_size_reg_lab_lab(op: tllvmop; fromsize: tllvmopsize; condreg: tregister; labtrue, labfalse: tasmlabel);
      begin
        create_llvm(op);
        size1:=fromsize;
        ops:=3;
        loadreg(0,condreg);
        loadsymbol(1,labtrue,0);
        loadsymbol(2,labfalse,0);
      end;


    constructor taillvm.op_def_reg(op: tllvmop; def: tdef; reg: tregister);
      begin
        create_llvm(op);
        ops:=2;
        loaddef(0,def);
        loadreg(1,reg);
      end;


    constructor taillvm.getelementptr_reg_size_ref_size_reg(dst: tregister; ptrsize: tllvmopsize; const ref: treference; indextype: tllvmopsize; index1: tregister);
    begin
      create_llvm(la_getelementptr);
      ops:=3;
      size1:=ptrsize;
      size2:=indextype;
      loadreg(0,dst);
      loadref(1,ref);
      loadreg(2,index1);
    end;


end.
