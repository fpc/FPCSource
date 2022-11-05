{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    Contains the assembler object for the LoongArch64

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
  globtype,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  cpubase,cgbase,cgutils;

    const
      { "move reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "move reg,reg" source operand number }
      O_MOV_DEST = 0;


    type
      { TODO: Branch condition? TRefernece and Symbol? }
      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         { NOP,TLBSRCH,TLBRD,TLBWR,TLBFILL,TLBCLR,TLBFLUSH,ERTN, }
         constructor op_none(op : tasmop);

         { DBAR,IBAR,SYSCALL,BREAK,DBCL,IDLE, }
         constructor op_const(op : tasmop;_op1 : aint);

         { B,BL,JR, }
         constructor op_ref(op : tasmop; const _op1 : treference);

         { EXT,CLO,CLZ,CTO,CTZ,REVB,REVH,BITREV,ASRTLE,ASRTGT,RDTIMEL,RDTIMEH,
           RDTIME,CPUCFG,FABS,FNEG,FSQRT,FRECIP,FRSQRT,FLOGB,FCLASS,FCVT,FFINT,
           FTINT,FTINTRM,FTINTRP,FTINTRZ,FTINTRNE,FRINT,FMOV,MOVGR2FR,MOVGR2FRH,
           MOVFR2GR,MOVFRH2GR,MOVGR2FCSR,MOVFCSR2GR,MOVFR2CF,MOVCF2FR,MOVGR2CF,
           MOVCF2GR,IOCSRRD,IOCSRWR,MOVE,}
         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);

         { BEQZ,BNEZ,BCEQZ,BCNEZ,LA,BLTZ,BGTZ,BGEZ,BLEZ,LD,ST,LDX,STX,LDPTR,
           STPTR,FLD,FST,FLDX,FSTX, }
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);

         { LU12I,LU32I,PCADDI,PCADDU12I,PCADDU18I,PCALAU12I,CSRRD,CSRWR,LDPTE,LI, }
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aint);

         { BSTRINS,BSTRPICK, }
         constructor op_reg_reg_const_const(op: tasmop; _op1, _op2: tregister; _op3, _op4: aint);

         { ADD,SUB,SLT,AND,OR,NOR,XOR,ANDN,ORN,MUL,MULH,MULW,DIV,MOD,SLL,SRL,SRA,
           RTOR,MASKEQZ,MASKNEZ,LDGT,LDLE,STGT,STLE,AMSWAP,AMSWAP_DB,AMADD,AMADD_DB,
           AMAND,AMAND_DB,AMOR,AMOR_DB,AMXOR,AMXOR_DB,AMMAX,AMMAX_DB,AMMIN,AMMIN_DB,
           CRC,CRCC,FADD,FSUB,FMUL,FDIV,FMAX,FMIN,FMAXA,FMINA,FCALEB,FCOPYSIGN,FCMP,
           FLDGT,FLDLE,FSTGT,FSTLE,}
         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);

         { ADDI,ADDU16I,LU52I,SLTI,ANDI,ORI,XORI,SLLI,SRLI,SRAI,ROTRI,JIRL,LL,SC,
           CSRXCHG,LDDIR,}
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);

         { BEQ,BNE,BLT,BGE,BLTU,BGEU,BGT,BLE,BGTU,BLEU, }
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);

         { PRELD,CACOP, }
         constructor op_const_reg_const(op : tasmop;_op1 : aint;_op2 : tregister;_op3 : aint);

         { PRELDX,INVTLB }
         constructor op_const_reg_reg(op : tasmop; _op1 : aint; _op2,_op3 : tregister);

         { FMADD,FMSUB,FNMADD,FNMSUB,FSEL,}
         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);

         { ALSL,BYTEPICK, }
         constructor op_reg_reg_reg_const(op : tasmop; _op1, _op2, _op3 : tregister; _op4 : aint);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
         function spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

implementation

uses cutils, cclasses;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : aint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_reg_const_const(op: tasmop; _op1, _op2: tregister; _op3, _op4: aint);
      begin
        inherited create(op);
        ops := 4;
        loadreg(0, _op1);
        loadreg(1, _op2);
        loadconst(2, _op3);
        loadconst(3, _op4);
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
         loadconst(2,_op3);
      end;


     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_const_reg_reg(op : tasmop;_op1 : aint;_op2, _op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


     constructor taicpu.op_const_reg_const(op : tasmop;_op1 : aint;_op2 : tregister;_op3 : aint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_reg_reg_const(op : tasmop; _op1, _op2, _op3 : tregister; _op4 : aint);
      begin
        inherited create(op);
        ops := 4;
        loadreg(0, _op1);
        loadreg(1, _op2);
        loadreg(2, _op3);
        loadconst(3, cardinal(_op4));
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        case regtype of
          R_INTREGISTER:
            result:=
               (opcode=A_MOVE) and (oper[0]^.reg=oper[1]^.reg);
          R_FPUREGISTER:
            result:=
               ((opcode=A_FMOV_S) or (opcode=A_FMOV_D)) and
               (oper[0]^.reg=oper[1]^.reg);
         else
           result:=false;
        end;
      end;

    { Mant of case is set opnr0 and read from opnr(others). }
    function normal_oper_type(opnr: longint): topertype;
      begin
        if opnr=0 then
          result:=operand_write
        else
          result:=operand_read;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case opcode of
          A_ST_B,A_ST_D,A_ST_W,A_ST_H,A_STX_B,A_STX_D,A_STX_W,A_STX_H,
          A_STPTR_W,A_STPTR_D,A_STGT_B,A_STGT_D,A_STGT_W,A_STGT_H,
          A_STLE_B,A_STLE_D,A_STLE_W,A_STLE_H,A_FST_S,A_FST_D,A_FSTX_S,
          A_FSTX_D,A_FSTGT_S,A_FSTGT_D,A_FSTLE_S,A_FSTLE_D,A_CSRWR,
          A_IOCSRWR_B,A_IOCSRWR_D,A_IOCSRWR_W,A_IOCSRWR_H:
            result:=operand_read;
          A_RDTIME_D,A_RDTIMEL_W,A_RDTIMEH_W:
            result:=operand_write;
          A_PRELD,A_PRELDX,A_DBAR,A_IBAR,A_SYSCALL,A_BREAK,A_ASRTLE_D,
          A_ASRTGT_D,A_CACOP,A_INVTLB,A_DBCL,A_IDLE,A_BEQ,A_BNE,A_BLT,
          A_BLTU,A_BGE,A_BGEU,A_BEQZ,A_BNEZ,A_B,A_BL,A_BCEQZ,A_BCNEZ,
          A_BLTZ,A_BGTZ,A_BGEZ,A_BLEZ,A_BGT,A_BLE,A_BGTU,A_BLEU,A_JR,
          A_NOP,A_BXX:
            result:=operand_read;
          A_AMSWAP_W,A_AMSWAP_D,A_AMSWAP_DB_W,A_AMSWAP_DB_D,A_AMADD_W,
          A_AMADD_D,A_AMADD_DB_W,A_AMADD_DB_D,A_AMAND_W,A_AMAND_D,
          A_AMAND_DB_W,A_AMAND_DB_D,A_AMOR_W,A_AMOR_D,A_AMOR_DB_W,
          A_AMOR_DB_D,A_AMXOR_W,A_AMXOR_D,A_AMXOR_DB_W,A_AMXOR_DB_D,
          A_AMMAX_W,A_AMMAX_DU,A_AMMAX_D,A_AMMAX_WU,A_AMMAX_DB_W,
          A_AMMAX_DB_DU,A_AMMAX_DB_D,A_AMMAX_DB_WU,A_AMMIN_W,A_AMMIN_DU,
          A_AMMIN_D,A_AMMIN_WU,A_AMMIN_DB_W,A_AMMIN_DB_DU,A_AMMIN_DB_D,
          A_AMMIN_DB_WU,A_SC_W,A_SC_D:
            result:=operand_readwrite;
        else
          result:=normal_oper_type(opnr);
        end;
      end;


    function taicpu.spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;
      begin
        result := operand_read;
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      var
        href : treference;
      begin
        href:=ref;
        href.refaddr:=addr_reg_12i;
        case getregtype(r) of
           R_INTREGISTER: result:=taicpu.op_reg_ref(A_LD_D,r,href);
           R_FPUREGISTER: result:=taicpu.op_reg_ref(A_FLD_D,r,href);
        else
           internalerror(2022111904);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      var
        href : treference;
      begin
        href:=ref;
        href.refaddr:=addr_reg_12i;
        case getregtype(r) of
           R_INTREGISTER: result:=taicpu.op_reg_ref(A_ST_D,r,href);
           R_FPUREGISTER: result:=taicpu.op_reg_ref(A_FST_D,r,href);
        else
           internalerror(2022111905);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


begin
  cai_align:=tai_align;
  cai_cpu:=taicpu;
end.
