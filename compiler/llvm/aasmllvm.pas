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
  symdef,
  llvmbase;

    type
      tai_llvmcpu = class(tai_cpu_abstract)
        // switch_end (= ']'), unreachable
        constructor create_llvm(op: tllvmop);
        constructor op_none(op : tllvmop);

        constructor op_ressym_string(op: tllvmop; sym: tasmsymbol; const str: ansistring);
        procedure loadstring(opidx:longint;_str: pchar);
        
        llvmopcode: tllvmop;
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

    constructor tai_llvmcpu.create_llvm(op: tllvmop);
      begin
        create(a_none);
        llvmopcode:=op;
        typ:=ait_llvmins;
      end;


    procedure tai_llvmcpu.loadstring(opidx:longint;_str: pchar);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_reg then
             clearop(opidx);
           str:=strnew(_str);
           typ:=top_string;
         end;
      end;


    constructor tai_llvmcpu.op_ressym_string(op: tllvmop; sym: tasmsymbol; const str: ansistring);
      begin
        create_llvm(op);
        ops:=2;
        loadsymbol(0,sym,0);
        loadstring(1,pchar(str));
      end;


    constructor tai_llvmcpu.op_none(op : tllvmop);
      begin
        create_llvm(op);
      end;

(*

    constructor taicpu.op_reg(op : tllvmop;_op1 : tregister);
      begin
         inherited create(op);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tllvmop;_op1 : aint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tllvmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tllvmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tllvmop; _op1: aint; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tllvmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tllvmop;_op1,_op2 : aint);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tllvmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_reg_reg_const(op : tllvmop;_op1,_op2 : tregister; _op3: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;

     constructor taicpu.op_reg_reg_sym_ofs(op : tllvmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;

     constructor taicpu.op_reg_reg_ref(op : tllvmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tllvmop;_op1 : aint;_op2, _op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_const_reg_const(op : tllvmop;_op1 : aint;_op2 : tregister;_op3 : aint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_const_const_const(op : tllvmop;_op1 : aint;_op2 : aint;_op3 : aint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadconst(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tllvmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_reg(op : tllvmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_const(op : tllvmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: aint);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(0,_op2);
         loadreg(0,_op3);
         loadconst(0,cardinal(_op4));
      end;


     constructor taicpu.op_reg_reg_reg_const_const(op : tllvmop;_op1,_op2,_op3 : tregister;_op4,_op5 : aint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

     constructor taicpu.op_reg_reg_const_const_const(op : tllvmop;_op1,_op2 : tregister;_op3,_op4,_op5 : aint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
         loadconst(3,_op4);
         loadconst(4,_op5);
      end;

    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;

     constructor taicpu.op_const_const_sym(op : tasmop;_op1,_op2 : aint; _op3: tasmsymbol);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadconst(1,_op2);
         loadsymbol(2,_op3,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:aint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result :=
          (((opcode=A_MR) and
            (regtype = R_INTREGISTER)) or
           ((opcode = A_FMR) and
            (regtype = R_FPUREGISTER))) and
          { these opcodes can only have registers as operands }
          (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: aint): topertype;
      begin
        result := operand_read;
        case opcode of
            A_STMW,A_LMW:
              internalerror(2005021805);
            A_STBU, A_STBUX, A_STHU, A_STHUX, A_STWU, A_STWUX, A_STFSU, A_STFSUX, A_STFDU, A_STFDUX, A_STB, A_STBX, A_STH, A_STHX, A_STW, A_STWX, A_STFS, A_STFSX, A_STFD, A_STFDX, A_STFIWX, A_STHBRX, A_STWBRX, A_STWCX_, A_CMP, A_CMPI, A_CMPL, A_CMPLI, A_DCBA, A_DCBI, A_DCBST, A_DCBT, A_DCBTST, A_DCBZ, A_ECOWX, A_FCMPO, A_FCMPU, A_MTMSR, A_TLBIE, A_TW, A_TWI, A_CMPWI, A_CMPW, A_CMPLWI, A_CMPLW, A_MT, A_MTLR, A_MTCTR:;
            A_RLWIMI:
              if opnr = 0 then
                result := operand_readwrite;
          else
            if opnr = 0 then
              result := operand_write;
          end;
      end;


    function taicpu.spilling_get_operation_type_ref(opnr: aint; reg: tregister): topertype;
      begin
        result := operand_read;
        case opcode of
          A_STBU, A_STBUX, A_STHU, A_STHUX, A_STWU, A_STWUX, A_STFSU, A_STFSUX, A_STFDU, A_STFDUX:
            if (oper[opnr]^.ref^.base = reg) then
              result := operand_readwrite;
        end;
      end;

    function spilling_create_load(const ref:treference;r:tregister): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER:
            result:=taicpu.op_reg_ref(A_LWZ,r,ref);
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_LFD,r,ref);
          else
            internalerror(2005123101);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER:
            result:=taicpu.op_reg_ref(A_STW,r,ref);
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_STFD,r,ref);
          else
            internalerror(2005123102);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


*)

end.
