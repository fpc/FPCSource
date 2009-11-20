{
    Copyright (c) 2000-2009 by Florian Klaempfl and David Zhang

    Code generation for add nodes on the FVM32

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
unit ncpuadd;

{$i fpcdefs.inc}

interface

uses
  node, ncgadd, cpubase, aasmbase, cgbase;

type
  tmipsaddnode = class(tcgaddnode)
  private
    function cmp64_lt(left_reg, right_reg: TRegister64): TRegister;
    function cmp64_le(left_reg, right_reg: TRegister64): TRegister;
    function cmp64_eq(left_reg, right_reg: TRegister64): TRegister;
    function cmp64_ne(left_reg, right_reg: TRegister64): TRegister;
    function cmp64_ltu(left_reg, right_reg: TRegister64): TRegister;
    function cmp64_leu(left_reg, right_reg: TRegister64): TRegister;

    function GetRes_register(unsigned: boolean; this_reg, left_reg, right_reg: TRegister): TRegister;
    function GetRes64_register(unsigned: boolean; {this_reg,} left_reg, right_reg: TRegister64): TRegister;
  protected
    procedure second_addfloat; override;
    procedure second_cmpfloat; override;
    procedure second_cmpboolean; override;
    procedure second_cmpsmallset; override;
    procedure second_cmp64bit; override;
    procedure second_cmpordinal; override;
  end;

implementation

uses
  systems,
  cutils, verbose,
  paramgr,
  aasmtai, aasmcpu, aasmdata,
  defutil,
  {cgbase,} cgcpu, cgutils,
  cpupara,
  ncon, nset, nadd,
  ncgutil, cgobj;

{*****************************************************************************
                               tmipsaddnode
*****************************************************************************}
function tmipsaddnode.GetRes_register(unsigned: boolean; this_reg, left_reg, right_reg: TRegister): TRegister;
var
  tmp_asm_op: tasmop;
begin
  case NodeType of
    equaln:
      tmp_asm_op := A_SEQ;
    unequaln:
      tmp_asm_op := A_SNE;
    else
      if not (unsigned) then
      begin
        if nf_swapped in flags then
          case NodeType of
            ltn:
              tmp_asm_op := A_SGT;
            lten:
              tmp_asm_op := A_SGE;
            gtn:
              tmp_asm_op := A_SLT;
            gten:
              tmp_asm_op := A_SLE;
          end
        else
          case NodeType of
            ltn:
              tmp_asm_op := A_SLT;
            lten:
              tmp_asm_op := A_SLE;
            gtn:
              tmp_asm_op := A_SGT;
            gten:
              tmp_asm_op := A_SGE;
          end;
      end
      else
      begin
        if nf_swapped in Flags then
          case NodeType of
            ltn:
              tmp_asm_op := A_SGTU;
            lten:
              tmp_asm_op := A_SGEU;
            gtn:
              tmp_asm_op := A_SLTU;
            gten:
              tmp_asm_op := A_SLEU;
          end
        else
          case NodeType of
            ltn:
              tmp_asm_op := A_SLTU;
            lten:
              tmp_asm_op := A_SLEU;
            gtn:
              tmp_asm_op := A_SGTU;
            gten:
              tmp_asm_op := A_SGEU;
          end;
      end;
  end;
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(tmp_asm_op, this_reg, left_reg, right_reg));
  GetRes_register := this_reg;
end;

function tmipsaddnode.cmp64_eq(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4: tasmlabel;
begin

  objectlibrary.getlabel(lfcmp64_L4);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, left_reg.reghi, right_reg.reghi, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_sym(A_BNE, left_reg.reglo, right_reg.reglo, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_eq := NR_TCR10;
end;

function tmipsaddnode.cmp64_ne(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4: tasmlabel;
begin

  objectlibrary.getlabel(lfcmp64_L4);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, left_reg.reghi, right_reg.reghi, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_sym(A_BNE, left_reg.reglo, right_reg.reglo, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_ne := NR_TCR10;
end;

function tmipsaddnode.cmp64_lt(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4, lfcmp64_L5: tasmlabel;
begin
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  objectlibrary.getlabel(lfcmp64_L4);
  objectlibrary.getlabel(lfcmp64_L5);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLT, NR_TCR11, left_reg.reghi, right_reg.reghi));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, left_reg.reghi, right_reg.reghi, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, left_reg.reglo, right_reg.reglo));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_B, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L5);
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_lt := NR_TCR10;
end;

function tmipsaddnode.cmp64_le(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4, lfcmp64_L5: tasmlabel;
begin
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  objectlibrary.getlabel(lfcmp64_L4);
  objectlibrary.getlabel(lfcmp64_L5);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLT, NR_TCR11, right_reg.reghi, left_reg.reghi));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, right_reg.reghi, left_reg.reghi, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, right_reg.reglo, left_reg.reglo));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L5);
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_le := NR_TCR10;
end;

function tmipsaddnode.cmp64_ltu(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4, lfcmp64_L5: tasmlabel;
begin
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  objectlibrary.getlabel(lfcmp64_L4);
  objectlibrary.getlabel(lfcmp64_L5);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, left_reg.reghi, right_reg.reghi));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, left_reg.reghi, right_reg.reghi, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, left_reg.reglo, right_reg.reglo));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_B, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L5);
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_ltu := NR_TCR10;
end;

function tmipsaddnode.cmp64_leu(left_reg, right_reg: TRegister64): TRegister;
var
  lfcmp64_L4, lfcmp64_L5: tasmlabel;
begin
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 0));

  objectlibrary.getlabel(lfcmp64_L4);
  objectlibrary.getlabel(lfcmp64_L5);

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, right_reg.reghi, left_reg.reghi));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, right_reg.reghi, left_reg.reghi, lfcmp64_L5));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, NR_TCR11, right_reg.reglo, left_reg.reglo));
  current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BNE, NR_TCR11, NR_R0, lfcmp64_L4));
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L5);
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, NR_TCR10, 1));

  cg.a_label(current_asmdata.CurrAsmList, lfcmp64_L4);
  cmp64_leu := NR_TCR10;
end;

function tmipsaddnode.GetRes64_register(unsigned: boolean; //this_reg: TRegister;
                                                            left_reg, right_reg: TRegister64): TRegister;
var
  tmpreg: TRegister;
  lfcmp64_L4, lfcmp_L5: tasmlabel;
begin
  case NodeType of
    equaln:
    begin
      GetRes64_register := cmp64_eq(left_reg, right_reg);
    end;
    unequaln:
      GetRes64_register := cmp64_ne(left_reg, right_reg);
    else
      if not (unsigned) then
      begin
        if nf_swapped in flags then
          case NodeType of
            ltn:
              GetRes64_register := cmp64_lt(right_reg, left_reg);
            lten:
              GetRes64_register := cmp64_le(right_reg, left_reg);
            gtn:
              GetRes64_register := cmp64_lt(left_reg, right_reg);
            gten:
              GetRes64_register := cmp64_le(left_reg, right_reg);
          end
        else
          case NodeType of
            ltn:
              GetRes64_register := cmp64_lt(left_reg, right_reg);
            lten:
              GetRes64_register := cmp64_le(left_reg, right_reg);
            gtn:
              GetRes64_register := cmp64_lt(right_reg, left_reg);
            gten:
              GetRes64_register := cmp64_le(right_reg, left_reg);
          end;
      end
      else
      begin
        if nf_swapped in Flags then
          case NodeType of
            ltn:
              GetRes64_register := cmp64_ltu(right_reg, left_reg);
            lten:
              GetRes64_register := cmp64_leu(right_reg, left_reg);
            gtn:
              GetRes64_register := cmp64_ltu(left_reg, right_reg);
            gten:
              GetRes64_register := cmp64_leu(left_reg, right_reg);
          end
        else
          case NodeType of
            ltn:
              GetRes64_register := cmp64_ltu(left_reg, right_reg);
            lten:
              GetRes64_register := cmp64_leu(left_reg, right_reg);
            gtn:
              GetRes64_register := cmp64_ltu(right_reg, left_reg);
            gten:
              GetRes64_register := cmp64_leu(right_reg, left_reg);
          end;
      end;
  end;
end;

procedure tmipsaddnode.second_addfloat;
var
  op: TAsmOp;
begin
  pass_left_right;
  if (nf_swapped in flags) then
    swapleftright;

        { force fpureg as location, left right doesn't matter
          as both will be in a fpureg }
  location_force_fpureg(current_asmdata.CurrAsmList, left.location, True);
  location_force_fpureg(current_asmdata.CurrAsmList, right.location, (left.location.loc <> LOC_CFPUREGISTER));

  location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
  if left.location.loc <> LOC_CFPUREGISTER then
    location.Register := left.location.Register
  else
    location.Register := right.location.Register;

  case nodetype of
    addn:
    begin
      if location.size = OS_F64 then
        op := A_ADD_D
      else
        op := A_ADD_S;
    end;
    muln:
    begin
      if location.size = OS_F64 then
        op := A_MUL_D
      else
        op := A_MUL_S;
    end;
    subn:
    begin
      if location.size = OS_F64 then
        op := A_SUB_D
      else
        op := A_SUB_S;
    end;
    slashn:
    begin
      if location.size = OS_F64 then
        op := A_DIV_D
      else
        op := A_DIV_S;
    end;
    else
      internalerror(200306014);
  end;
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
    location.Register, left.location.Register, right.location.Register));

end;


procedure tmipsaddnode.second_cmpfloat;
var
  op: tasmop;
  lfcmptrue, lfcmpfalse: tasmlabel;
begin
  pass_left_right;
  if nf_swapped in flags then
    swapleftright;

  { force fpureg as location, left right doesn't matter
    as both will be in a fpureg }
  location_force_fpureg(current_asmdata.CurrAsmList, left.location, True);
  location_force_fpureg(current_asmdata.CurrAsmList, right.location, True);

  location_reset(location, LOC_REGISTER, OS_INT);
  location.Register := NR_TCR0;

  case NodeType of
    equaln:
    begin
      if left.location.size = OS_F64 then
        op := A_C_EQ_D
      else
        op := A_C_EQ_S;
      objectlibrary.getlabel(lfcmpfalse);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.Register, right.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1F, lfcmpfalse)); //lfcmpfalse
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      cg.a_label(current_asmdata.CurrAsmList, lfcmpfalse);

    end;
    unequaln:
    begin
      if left.location.size = OS_F64 then
        op := A_C_EQ_D
      else
        op := A_C_EQ_S;
      objectlibrary.getlabel(lfcmpfalse);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.Register, right.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1F, lfcmpfalse));
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      cg.a_label(current_asmdata.CurrAsmList, lfcmpfalse);
    end;
    ltn:
    begin
      if left.location.size = OS_F64 then
        op := A_C_LT_D
      else
        op := A_C_LT_S;
      objectlibrary.getlabel(lfcmptrue);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.Register, right.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1T, lfcmptrue));
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      cg.a_label(current_asmdata.CurrAsmList, lfcmptrue);
    end;
    lten:
    begin
      if left.location.size = OS_F64 then
        op := A_C_LE_D
      else
        op := A_C_LE_S;
      objectlibrary.getlabel(lfcmptrue);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.Register, right.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1T, lfcmptrue));
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      cg.a_label(current_asmdata.CurrAsmList, lfcmptrue);
    end;
    gtn:
    begin
      if left.location.size = OS_F64 then
        op := A_C_LT_D
      else
        op := A_C_LT_S;
      objectlibrary.getlabel(lfcmptrue);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, right.location.Register, left.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1T, lfcmptrue));
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      cg.a_label(current_asmdata.CurrAsmList, lfcmptrue);
    end;
    gten:
    begin
      if left.location.size = OS_F64 then
        op := A_C_LE_D
      else
        op := A_C_LE_S;
      objectlibrary.getlabel(lfcmptrue);
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ORI, location.Register{NR_TCR0}, NR_R0, 1));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, right.location.Register, left.location.Register));
      current_asmdata.CurrAsmList.concat(Taicpu.op_sym(A_BC1T, lfcmptrue));
      current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR, location.Register {NR_TCR0}, NR_R0, NR_R0));
      cg.a_label(current_asmdata.CurrAsmList, lfcmptrue);
    end;
  end; {case}
end;


procedure tmipsaddnode.second_cmpboolean;
var
  tmp_right_reg: TRegister;
begin
  pass_left_right;
  force_reg_left_right(True, True);
  tmp_right_reg := NR_NO;
  if right.location.loc = LOC_CONSTANT then
  begin
    tmp_right_reg := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, tmp_right_reg, right.location.Value));
  end
  else
  begin
    tmp_right_reg := right.location.Register;
  end;

  location_reset(location, LOC_REGISTER, OS_INT);
  location.Register := GetRes_register(True, NR_TCR0, left.location.Register, tmp_right_reg);

end;


procedure tmipsaddnode.second_cmpsmallset;
var
  tmp_right_reg: TRegister;
begin
  pass_left_right;
  force_reg_left_right(True, True);

  tmp_right_reg := NR_NO;

  if right.location.loc = LOC_CONSTANT then
  begin
    tmp_right_reg := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, tmp_right_reg, right.location.Value));
  end
  else
  begin
    tmp_right_reg := right.location.Register;
  end;


  location_reset(location, LOC_REGISTER, OS_INT);
  location.Register := GetRes_register(True, NR_TCR0, left.location.Register, tmp_right_reg);
end;


procedure tmipsaddnode.second_cmp64bit;
var
         unsigned   : boolean;
  tmp_left_reg: TRegister;

begin
          pass_left_right;
          force_reg_left_right(false,false);

          unsigned:=not(is_signed(left.resultdef)) or
                    not(is_signed(right.resultdef));

  location_reset(location, LOC_REGISTER, OS_INT);
  location.Register := GetRes64_register(unsigned, {NR_TCR0, }left.location.register64, right.location.register64); // NR_TCR0;
end;


procedure tmipsaddnode.second_cmpordinal;
var
  unsigned: boolean;
  tmp_right_reg: TRegister;
begin
  pass_left_right;
  force_reg_left_right(True, True);
  unsigned := not (is_signed(left.resultdef)) or not (is_signed(right.resultdef));

  tmp_right_reg := NR_NO;
  if right.location.loc = LOC_CONSTANT then
  begin
    tmp_right_reg := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LI, tmp_right_reg, right.location.Value));
  end
  else
  begin
    tmp_right_reg := right.location.Register;
  end;
  location_reset(location, LOC_REGISTER, OS_INT);
  location.Register := getres_register(unsigned, NR_TCR0, left.location.Register, tmp_right_reg);
end;

begin
  caddnode := tmipsaddnode;
end.
