{
    Copyright (c) 1999-2009 by Mazen Neifer and David Zhang

    Contains the assembler object for the MIPSEL

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
  cclasses,
  globtype, globals, verbose,
  aasmbase, aasmtai,
  cgbase, cgutils, cpubase, cpuinfo;

const
  { "mov reg,reg" source operand number }
  O_MOV_SOURCE = 0;
  { "mov reg,reg" source operand number }
  O_MOV_DEST   = 1;

type
  taicpu = class(tai_cpu_abstract)
    delayslot_annulled: boolean;   { conditinal opcode with ,a }
    constructor op_none(op: tasmop);

    constructor op_reg(op: tasmop; _op1: tregister);
    constructor op_const(op: tasmop; _op1: longint);
    constructor op_ref(op: tasmop; const _op1: treference);

    constructor op_reg_reg(op: tasmop; _op1, _op2: tregister);
    constructor op_reg_ref(op: tasmop; _op1: tregister; const _op2: treference);
    constructor op_reg_const(op: tasmop; _op1: tregister; _op2: longint);

    constructor op_reg_reg_reg(op: tasmop; _op1, _op2, _op3: tregister);

    constructor op_reg_reg_ref(op: tasmop; _op1, _op2: tregister; const _op3: treference);
    constructor op_reg_reg_const(op: tasmop; _op1, _op2: tregister; _op3: aint);

    { this is for Jmp instructions }
    constructor op_sym(op: tasmop; _op1: tasmsymbol);
    constructor op_reg_reg_sym(op: tasmop; _op1, _op2: tregister; _op3: tasmsymbol);
    constructor op_reg_sym(op: tasmop; _op1: tregister; _op2: tasmsymbol);
    constructor op_sym_ofs(op: tasmop; _op1: tasmsymbol; _op1ofs: longint);

    { register allocation }
    function is_same_reg_move(regtype: Tregistertype): boolean; override;

    { register spilling code }
    function spilling_get_operation_type(opnr: longint): topertype; override;
  end;

  tai_align = class(tai_align_abstract)
    { nothing to add }
  end;

  procedure InitAsm;
  procedure DoneAsm;

  function spilling_create_load(const ref: treference; r: tregister): taicpu;
  function spilling_create_store(r: tregister; const ref: treference): taicpu;

implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}



constructor taicpu.op_none(op: tasmop);
begin
  inherited Create(op);
end;


constructor taicpu.op_reg(op: tasmop; _op1: tregister);
begin
  inherited Create(op);
  ops := 1;
  loadreg(0, _op1);
end;


constructor taicpu.op_ref(op: tasmop; const _op1: treference);
begin
  inherited Create(op);
  ops := 1;
  loadref(0, _op1);
end;


constructor taicpu.op_const(op: tasmop; _op1: longint);
begin
  inherited Create(op);
  ops := 1;
  loadconst(0, _op1);
end;


constructor taicpu.op_reg_reg(op: tasmop; _op1, _op2: tregister);
begin
  inherited Create(op);
  ops := 2;
  loadreg(0, _op1);
  loadreg(1, _op2);
end;

constructor taicpu.op_reg_const(op: tasmop; _op1: tregister; _op2: longint);
begin
  inherited Create(op);
  ops := 2;
  loadreg(0, _op1);
  loadconst(1, _op2);
end;


constructor taicpu.op_reg_ref(op: tasmop; _op1: tregister; const _op2: treference);
begin
  inherited Create(op);
  ops := 2;
  loadreg(0, _op1);
  loadref(1, _op2);
end;


constructor taicpu.op_reg_reg_reg(op: tasmop; _op1, _op2, _op3: tregister);
begin
  inherited Create(op);
  ops := 3;
  loadreg(0, _op1);
  loadreg(1, _op2);
  loadreg(2, _op3);
end;


constructor taicpu.op_reg_reg_ref(op: tasmop; _op1, _op2: tregister; const _op3: treference);
 begin
   inherited create(op);
   ops := 3;
   loadreg(0, _op1);
   loadreg(1, _op2);
   loadref(2, _op3);
end;

constructor taicpu.op_reg_reg_const(op: tasmop; _op1, _op2: tregister; _op3: aint);
 begin
   inherited create(op);
   ops := 3;
   loadreg(0, _op1);
   loadreg(1, _op2);
   loadconst(2, _op3);
end;



constructor taicpu.op_sym(op: tasmop; _op1: tasmsymbol);
begin
  inherited Create(op);
  is_jmp := op in [A_J, A_BEQI, A_BNEI, A_BLTI, A_BLEI, A_BGTI, A_BGEI,
    A_BLTUI, A_BLEUI, A_BGTUI, A_BGEUI,
    A_BEQ, A_BNE, A_BLT, A_BLE, A_BGT, A_BGE,
    A_BLTU, A_BLEU, A_BGTU, A_BGEU
    ];

  ops := 1;
  loadsymbol(0, _op1, 0);
end;

constructor taicpu.op_reg_reg_sym(op: tasmop; _op1, _op2: tregister; _op3: tasmsymbol);
begin
   inherited create(op);
   is_jmp := op in [A_J,
     A_BEQI, A_BNEI, A_BLTI, A_BLEI, A_BGTI, A_BGEI, A_BLTUI, A_BLEUI,
     A_BGTUI, A_BGEUI,
     A_BEQ, A_BNE, A_BLT, A_BLE, A_BGT, A_BGE, A_BLTU, A_BLEU, A_BGTU, A_BGEU];
   ops := 3;
   loadreg(0, _op1);
   loadreg(1, _op2);
   loadsymbol(2, _op3, 0);
end;

constructor taicpu.op_reg_sym(op: tasmop; _op1: tregister; _op2: tasmsymbol);
begin
   inherited create(op);
   is_jmp := op in [A_J,
     A_BEQI, A_BNEI, A_BLTI, A_BLEI, A_BGTI, A_BGEI, A_BLTUI, A_BLEUI,
     A_BGTUI, A_BGEUI,
     A_BEQ, A_BNE, A_BLT, A_BLE, A_BGT, A_BGE, A_BLTU, A_BLEU, A_BGTU, A_BGEU, A_BGTZ];
   ops := 2;
   loadreg(0, _op1);
   loadsymbol(1, _op2, 0);
end;

constructor taicpu.op_sym_ofs(op: tasmop; _op1: tasmsymbol; _op1ofs: longint);
begin
  inherited Create(op);
  ops := 1;
  loadsymbol(0, _op1, _op1ofs);
end;


function taicpu.is_same_reg_move(regtype: Tregistertype): boolean;
begin
  Result := (
    ((opcode = A_MOVE) and (regtype = R_INTREGISTER)) or
    ((regtype = R_FPUREGISTER) and (opcode in [A_MOV_S, A_MOV_D]))
    ) and
    (oper[0]^.reg = oper[1]^.reg);
end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      type
        op_write_set_type =  set of TAsmOp;
      const
        op_write_set: op_write_set_type =
      [A_NEG,
      A_NEGU,
      A_LI,
      A_DLI,
      A_LA,
      A_MOVE,
      A_LB,
      A_LBU,
      A_LH,
      A_LHU,
      A_LW,
      A_LWU,
      A_LWL,
      A_LWR,
      A_LD,
      A_LDL,
      A_LDR,
      A_LL,
      A_LLD,
      A_ADDI,
      A_DADDI,
      A_ADDIU,
      A_DADDIU,
      A_SLTI,
      A_SLTIU,
      A_ANDI,
      A_ORI,
      A_XORI,
      A_LUI,
      A_DNEG,
      A_DNEGU,
      A_ADD,
      A_DADD,
      A_ADDU,
      A_DADDU,
      A_SUB,
      A_DSUB,
      A_SUBU,
      A_DSUBU,
      A_SLT,
      A_SLTU,
      A_AND,
      A_OR,
      A_XOR,
      A_NOR,
      A_MUL,
      A_MULO,
      A_MULOU,
      A_DMUL,
      A_DMULO,
      A_DMULOU,
      A_DIV,
      A_DIVU,
      A_DDIV,
      A_DDIVU,
      A_REM,
      A_REMU,
      A_DREM,
      A_DREMU,
      A_MULT,
      A_DMULT,
      A_MULTU,
      A_DMULTU,
      A_MFHI,
      A_MFLO,
      A_MULTG,
      A_DMULTG,
      A_MULTUG,
      A_DMULTUG,
      A_DIVG,
      A_DDIVG,
      A_DIVUG,
      A_DDIVUG,
      A_MODG,
      A_DMODG,
      A_MODUG,
      A_DMODUG,

      A_SLL,
      A_SRL,
      A_SRA,
      A_SLLV,
      A_SRLV,
      A_SRAV,
      A_DSLL,
      A_DSRL,
      A_DSRA,
      A_DSLLV,
      A_DSRLV,
      A_DSRAV,
      A_DSLL32,
      A_DSRL32,
      A_DSRA32,
      A_LWC1,
      A_LDC1,


      A_ADD_S,
      A_ADD_D,
      A_SUB_S,
      A_SUB_D,
      A_MUL_S,
      A_MUL_D,
      A_DIV_S,
      A_DIV_D,
      A_ABS_S,
      A_ABS_D,
      A_NEG_S,
      A_NEG_D,
      A_SQRT_S,
      A_SQRT_D,
      A_MOV_S,
      A_MOV_D,
      A_CVT_S_D,
      A_CVT_S_W,
      A_CVT_S_L,
      A_CVT_D_S,
      A_CVT_D_W,
      A_CVT_D_L,
      A_CVT_W_S,
      A_CVT_W_D,
      A_CVT_L_S,
      A_CVT_L_D,
      A_ROUND_W_S,
      A_ROUND_W_D,
      A_ROUND_L_S,
      A_ROUND_L_D,
      A_TRUNC_W_S,
      A_TRUNC_W_D,
      A_TRUNC_L_S,
      A_TRUNC_L_D,
      A_CEIL_W_S,
      A_CEIL_W_D,
      A_CEIL_L_S,
      A_CEIL_L_D,
      A_FLOOR_W_S,
      A_FLOOR_W_D,
      A_FLOOR_L_S,
      A_FLOOR_L_D,
      A_SEQ,
      A_SGE,
      A_SGEU,
      A_SGT,
      A_SGTU,
      A_SLE,
      A_SLEU,
      A_SNE];

      begin
        result := operand_read;
        if opcode in op_write_set then
          if opnr = 0 then
            result := operand_write;
      end;


    function spilling_create_load(const ref: treference; r: tregister): taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_LW,r,ref);
          R_FPUREGISTER :
            begin
              case getsubreg(r) of
                R_SUBFS :
                  result:=taicpu.op_reg_ref(A_LWC1,r,ref);
                R_SUBFD :
                  result:=taicpu.op_reg_ref(A_LDC1,r,ref);
                else
                  internalerror(200401042);
              end;
            end
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r: tregister; const ref: treference): taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_SW,r,ref);
          R_FPUREGISTER :
            begin
              case getsubreg(r) of
                R_SUBFS :
                  result:=taicpu.op_reg_ref(A_SWC1,r,ref);
                R_SUBFD :
                  result:=taicpu.op_reg_ref(A_SDC1,r,ref);
                else
                  internalerror(200401042);
              end;
            end
          else
            internalerror(200401041);
        end;
      end;


procedure InitAsm;
  begin
  end;


procedure DoneAsm;
  begin
  end;


begin
  cai_cpu   := taicpu;
  cai_align := tai_align;
end.
