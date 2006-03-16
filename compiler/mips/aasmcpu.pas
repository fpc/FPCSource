{
    Copyright (c) 2003 by Florian Klaempfl

    Contains the assembler object for MIPS

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,aasmtai,aasmdata,
  aasmbase,globtype,globals,verbose,
  cpubase,cpuinfo,cgbase,cgutils;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type
      taicpu = class(tai_cpu_abstract)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : longint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aint);

         constructor op_ref_regset(op:tasmop; _op1: treference; _op2: tcpuregisterset);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         { SFM/LFM }
         constructor op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);

         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         function spilling_get_operation_type(opnr: longint): topertype;override;
      end;
      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    function spilling_create_load(const ref:treference;r:tregister): tai;
    function spilling_create_store(r:tregister; const ref:treference): tai;

    procedure InitAsm;
    procedure DoneAsm;


implementation

  uses
    cutils,rgobj,itcpugas;


{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited create(op);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,aint(_op1));
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aint(_op2));
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aint(_op3));
      end;


    constructor taicpu.op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadref(2,_op3);
      end;


     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;


     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        { allow the register allocator to remove unnecessary moves }
        result:=(((opcode=A_MOVE) and (regtype = R_INTREGISTER)) or
                 ((opcode=A_MOVF) and (regtype = R_FPUREGISTER))
                ) and
                (condition=C_None) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function spilling_create_load(const ref:treference;r:tregister): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_LDR,r,ref);
          R_FPUREGISTER :
            { use lfm because we don't know the current internal format
              and avoid exceptions
            }
            result:=taicpu.op_reg_const_ref(A_LFM,r,1,ref);
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_STR,r,ref);
          R_FPUREGISTER :
            { use sfm because we don't know the current internal format
              and avoid exceptions
            }
            result:=taicpu.op_reg_const_ref(A_SFM,r,1,ref);
          else
            internalerror(200401041);
        end;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case opcode of
          A_ADC,A_ADD,A_AND,
          A_EOR,A_CLZ,
          A_LDR,A_LDRB,A_LDRD,A_LDRBT,A_LDRH,A_LDRSB,
          A_LDRSH,A_LDRT,
          A_MOV,A_MVN,A_MLA,A_MUL,
          A_ORR,A_RSB,A_RSC,A_SBC,A_SUB,
          A_SWP,A_SWPB,
          A_LDF,A_FLT,A_FIX,
          A_ADF,A_DVF,A_FDV,A_FML,
          A_RFS,A_RFC,A_RDF,
          A_RMF,A_RPW,A_RSF,A_SUF,A_ABS,A_ACS,A_ASN,A_ATN,A_COS,
          A_EXP,A_LOG,A_LGN,A_MVF,A_MNF,A_FRD,A_MUF,A_POL,A_RND,A_SIN,A_SQT,A_TAN,
          A_LFM:
            if opnr=0 then
              result:=operand_write
            else
              result:=operand_read;
          A_BIC,A_BKPT,A_B,A_BL,A_BLX,A_BX,
          A_CMN,A_CMP,A_TEQ,A_TST,
          A_CMF,A_CMFE,A_WFS,A_CNF:
            result:=operand_read;
          A_SMLAL,A_UMLAL:
            if opnr in [0,1] then
              result:=operand_readwrite
            else
              result:=operand_read;
           A_SMULL,A_UMULL:
            if opnr in [0,1] then
              result:=operand_write
            else
              result:=operand_read;
          A_STR,A_STRB,A_STRBT,A_STRD,
          A_STRH,A_STRT,A_STF,A_SFM:
            { important is what happens with the involved registers }
            if opnr=0 then
              result := operand_read
            else
              { check for pre/post indexed }
              result := operand_read;
          else
            internalerror(200403151);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

end.
