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

  { tmipsaddnode }

  tmipsaddnode = class(tcgaddnode)
  private
    procedure cmp64_lt(left_reg, right_reg: TRegister64;unsigned:boolean);
    procedure cmp64_le(left_reg, right_reg: TRegister64;unsigned:boolean);
    procedure second_generic_cmp32(unsigned: boolean);
  protected
    procedure second_addfloat; override;
    procedure second_cmpfloat; override;
    procedure second_cmpboolean; override;
    procedure second_cmpsmallset; override;
    procedure second_cmp64bit; override;
    procedure second_cmpordinal; override;
    procedure second_addordinal; override;
  public
    function pass_1: tnode; override;
    function use_generic_mul32to64: boolean; override;
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
  procinfo,
  symconst,symdef,
  ncon, nset, nadd,
  ncgutil, cgobj;

{*****************************************************************************
                               tmipsaddnode
*****************************************************************************}
const
  swapped_nodetype: array[ltn..unequaln] of tnodetype =
    //lt  lte  gt  gte
    (gtn, gten,ltn,lten, equaln, unequaln);

  nodetype2opcmp: array[boolean,ltn..unequaln] of TOpCmp = (
    (OC_LT, OC_LTE, OC_GT, OC_GTE, OC_EQ, OC_NE),
    (OC_B,  OC_BE,  OC_A,  OC_AE,  OC_EQ, OC_NE)
  );

procedure tmipsaddnode.second_generic_cmp32(unsigned: boolean);
var
  ntype: tnodetype;
begin
  pass_left_right;
  force_reg_left_right(True, True);
  location_reset(location,LOC_FLAGS,OS_NO);

  ntype:=nodetype;
  if nf_swapped in flags then
    ntype:=swapped_nodetype[nodetype];

  location.resflags.cond:=nodetype2opcmp[unsigned,ntype];
  location.resflags.reg1:=left.location.register;
  location.resflags.use_const:=(right.location.loc=LOC_CONSTANT);
  if location.resflags.use_const then
    location.resflags.value:=right.location.value
  else
    location.resflags.reg2:=right.location.register;
end;


const
  cmpops: array[boolean] of TOpCmp = (OC_LT,OC_B);

procedure tmipsaddnode.cmp64_lt(left_reg, right_reg: TRegister64;unsigned: boolean);
begin
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],right_reg.reghi,left_reg.reghi,current_procinfo.CurrTrueLabel);
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,current_procinfo.CurrFalseLabel);
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,right_reg.reglo,left_reg.reglo,current_procinfo.CurrTrueLabel);
  cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
end;


procedure tmipsaddnode.cmp64_le(left_reg, right_reg: TRegister64;unsigned: boolean);
begin
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,cmpops[unsigned],left_reg.reghi,right_reg.reghi,current_procinfo.CurrFalseLabel);
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,current_procinfo.CurrTrueLabel);
  cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_B,left_reg.reglo,right_reg.reglo,current_procinfo.CurrFalseLabel);
  cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
end;


procedure tmipsaddnode.second_cmp64bit;
var
  unsigned: boolean;
  left_reg,right_reg: TRegister64;
begin
  location_reset(location, LOC_JUMP, OS_NO);
  pass_left_right;
  force_reg_left_right(true,true);

  unsigned:=not(is_signed(left.resultdef)) or
            not(is_signed(right.resultdef));

  left_reg:=left.location.register64;
  if (right.location.loc=LOC_CONSTANT) then
    begin
      if lo(right.location.value64)=0 then
        right_reg.reglo:=NR_R0
      else
        begin
          right_reg.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,lo(right.location.value64),right_reg.reglo);
        end;
      if hi(right.location.value64)=0 then
        right_reg.reghi:=NR_R0
      else
        begin
          right_reg.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,hi(right.location.value64),right_reg.reghi);
        end;
    end
  else
    right_reg:=right.location.register64;

  case NodeType of
    equaln:
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,current_procinfo.CurrFalseLabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,current_procinfo.CurrFalseLabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
      end;
    unequaln:
      begin
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reghi,right_reg.reghi,current_procinfo.CurrTrueLabel);
        cg.a_cmp_reg_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_NE,left_reg.reglo,right_reg.reglo,current_procinfo.CurrTrueLabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
      end;
  else
    if nf_swapped in flags then
      case NodeType of
        ltn:
          cmp64_lt(right_reg, left_reg,unsigned);
        lten:
          cmp64_le(right_reg, left_reg,unsigned);
        gtn:
          cmp64_lt(left_reg, right_reg,unsigned);
        gten:
          cmp64_le(left_reg, right_reg,unsigned);
      end
    else
      case NodeType of
        ltn:
          cmp64_lt(left_reg, right_reg,unsigned);
        lten:
          cmp64_le(left_reg, right_reg,unsigned);
        gtn:
          cmp64_lt(right_reg, left_reg,unsigned);
        gten:
          cmp64_le(right_reg, left_reg,unsigned);
      end;
  end;
end;


function tmipsaddnode.pass_1 : tnode;
  begin
    result:=inherited pass_1;

    if not(assigned(result)) then
      begin
        if (nodetype in [ltn,lten,gtn,gten,equaln,unequaln]) then
          begin
            if (left.resultdef.typ=floatdef) or (right.resultdef.typ=floatdef) then
              expectloc:=LOC_JUMP;
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
  location_force_fpureg(current_asmdata.CurrAsmList, right.location, True);

  location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
  location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);

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


const
  ops_cmpfloat: array[boolean,ltn..unequaln] of TAsmOp = (
  // ltn       lten      gtn       gten      equaln    unequaln
    (A_C_LT_S, A_C_LE_S, A_C_LT_S, A_C_LE_S, A_C_EQ_S, A_C_EQ_S),
    (A_C_LT_D, A_C_LE_D, A_C_LT_D, A_C_LE_D, A_C_EQ_D, A_C_EQ_D)
  );

procedure tmipsaddnode.second_cmpfloat;
var
  op: tasmop;
  lreg,rreg: tregister;
  ai: Taicpu;
begin
  pass_left_right;
  if nf_swapped in flags then
    swapleftright;

  location_force_fpureg(current_asmdata.CurrAsmList, left.location, True);
  location_force_fpureg(current_asmdata.CurrAsmList, right.location, True);
  location_reset(location, LOC_JUMP, OS_NO);

  op:=ops_cmpfloat[left.location.size=OS_F64,nodetype];

  if (nodetype in [gtn,gten]) then
    begin
      lreg:=right.location.register;
      rreg:=left.location.register;
    end
  else
    begin
      lreg:=left.location.register;
      rreg:=right.location.register;
    end;

  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,lreg,rreg));
  ai:=taicpu.op_sym(A_BC,current_procinfo.CurrTrueLabel);
  if (nodetype=unequaln) then
    ai.SetCondition(C_COP1FALSE)
  else
    ai.SetCondition(C_COP1TRUE);
  current_asmdata.CurrAsmList.concat(ai);
  current_asmdata.CurrAsmList.concat(TAiCpu.Op_none(A_NOP));
  cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
end;


procedure tmipsaddnode.second_cmpboolean;
begin
  second_generic_cmp32(true);
end;


procedure tmipsaddnode.second_cmpsmallset;
begin
  second_generic_cmp32(true);
end;


procedure tmipsaddnode.second_cmpordinal;
var
  unsigned: boolean;
begin
  unsigned := not (is_signed(left.resultdef)) or not (is_signed(right.resultdef));
  second_generic_cmp32(unsigned);
end;


const
  multops: array[boolean] of TAsmOp = (A_MULT, A_MULTU);

procedure tmipsaddnode.second_addordinal;
var
  unsigned: boolean;
begin
  unsigned:=not(is_signed(left.resultdef)) or
            not(is_signed(right.resultdef));
  if (nodetype=muln) and is_64bit(resultdef) then
    begin
      pass_left_right;
      force_reg_left_right(true,false);
      location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
      location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
      location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(multops[unsigned],left.location.register,right.location.register));
      current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_MFLO,location.register64.reglo));
      current_asmdata.CurrAsmList.Concat(taicpu.op_reg(A_MFHI,location.register64.reghi));
    end
  else
    inherited second_addordinal;
end;


function tmipsaddnode.use_generic_mul32to64: boolean;
begin
  result:=false;
end;

begin
  caddnode := tmipsaddnode;
end.
