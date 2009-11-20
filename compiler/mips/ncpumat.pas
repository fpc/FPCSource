{
David Zhang 2007/01/15
    $Id: ncpumat.pas,v 1.23 2005/02/14 17:13:10 peter Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate MIPSel assembler for math nodes

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
unit ncpumat;

{$i fpcdefs.inc}

interface

uses
  node, nmat, ncgmat;

type
  tMIPSELmoddivnode = class(tmoddivnode)
    procedure pass_generate_code;override;
  end;

  tMIPSELshlshrnode = class(tshlshrnode)
    procedure pass_generate_code;override;
    { everything will be handled in pass_2 }
    function first_shlshr64bitint: tnode; override;
  end;

  tMIPSELnotnode = class(tcgnotnode)
    procedure second_boolean; override;
  end;

implementation

uses
  globtype, systems,
  cutils, verbose, globals,
  symconst,
  aasmbase, aasmcpu, aasmtai, aasmdata,
  defutil,
  procinfo,
  cgbase, cgobj, pass_2,
  ncon,
  cpubase,
  ncgutil, cgcpu, cgutils;

{*****************************************************************************
                             TMipselMODDIVNODE
*****************************************************************************}

procedure tMIPSELmoddivnode.pass_generate_code;
var
  power: longint;
  tmpreg, numerator, divider, resultreg: tregister;
begin
  secondpass(left);
  secondpass(right);
  location_copy(location, left.location);

  { put numerator in register }
  location_force_reg(current_asmdata.CurrAsmList, left.location, def_cgsize(left.resultdef), True);
  location_copy(location, left.location);
  numerator := location.Register;

  if (nodetype = modn) then
    resultreg := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT)
  else
  begin
    if (location.loc = LOC_CREGISTER) then
    begin
      location.loc      := LOC_REGISTER;
      location.Register := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
    end;
    resultreg := location.Register;
  end;

  if (nodetype = divn) and
    (right.nodetype = ordconstn) and
    ispowerof2(tordconstnode(right).Value.svalue, power) then
  begin
    tmpreg := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, 31, numerator, tmpreg);
    { if signed, tmpreg=right value-1, otherwise 0 }
    cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, tordconstnode(right).Value.svalue - 1, tmpreg);
    { add to the left value }
    cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_ADD, OS_INT, tmpreg, numerator);
    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, aword(power), numerator, resultreg);
  end
  else
  begin
    { load divider in a register if necessary }
    location_force_reg(current_asmdata.CurrAsmList, right.location,
      def_cgsize(right.resultdef), True);
    divider := right.location.Register;


    if (nodetype = modn) then
    begin
      if is_signed(right.resultdef) then
      begin
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_REM, resultreg, numerator, divider));
      end
      else
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_REMU, resultreg, numerator, divider));
    end
    else
    begin
      if is_signed({left.resultdef}right.resultdef) then
      begin
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_DIV, resultreg, numerator, divider));
      end
      else
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_DIVU, resultreg, numerator, divider));
    end;
  end;
  { set result location }
  location.loc      := LOC_REGISTER;
  location.Register := resultreg;
end;


{*****************************************************************************
                             TMIPSelSHLRSHRNODE
*****************************************************************************}

function TMIPSELShlShrNode.first_shlshr64bitint: TNode;
begin
  { 64bit without constants need a helper }
  if is_64bit(left.resultdef) and
    (right.nodetype <> ordconstn) then
  begin
    Result := inherited first_shlshr64bitint;
    exit;
  end;

  Result := nil;
end;


procedure tMIPSELshlshrnode.pass_generate_code;
var
  hregister, resultreg, hregister1, hreg64hi, hreg64lo: tregister;
  op: topcg;
  shiftval: aword;
begin
  { 64bit without constants need a helper, and is
          already replaced in pass1 }
  if is_64bit(left.resultdef) and
    (right.nodetype <> ordconstn) then
    internalerror(200405301);

  secondpass(left);
  secondpass(right);
  if is_64bit(left.resultdef) then
  begin
    location_reset(location, LOC_REGISTER, OS_64);

    { load left operator in a register }
    location_force_reg(current_asmdata.CurrAsmList, left.location, OS_64, False);


    hreg64hi := left.location.register64.reghi;
    hreg64lo := left.location.register64.reglo;

    shiftval := tordconstnode(right).Value.svalue and 63;
    if shiftval > 31 then
    begin
      if nodetype = shln then
      begin
        cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_32, 0, hreg64hi);
        if (shiftval and 31) <> 0 then
          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, shiftval and 31, hreg64lo, hreg64lo);
      end
      else
      begin
        cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_32, 0, hreg64lo);
        if (shiftval and 31) <> 0 then
          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, shiftval and 31, hreg64hi, hreg64hi);
      end;
      location.register64.reglo := hreg64hi;
      location.register64.reghi := hreg64lo;
    end
    else
    begin
      hregister := cg.getintregister(current_asmdata.CurrAsmList, OS_32);
      if nodetype = shln then
      begin
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, 32 - shiftval, hreg64lo, hregister);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, shiftval, hreg64hi, hreg64hi);
        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_32, hregister, hreg64hi, hreg64hi);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, shiftval, hreg64lo, hreg64lo);
      end
      else
      begin
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_32, 32 - shiftval, hreg64hi, hregister);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, shiftval, hreg64lo, hreg64lo);
        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_32, hregister, hreg64lo, hreg64lo);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_32, shiftval, hreg64hi, hreg64hi);
      end;
      location.register64.reghi := hreg64hi;
      location.register64.reglo := hreg64lo;
    end;
  end
  else
  begin
    { load left operators in a register }
    location_force_reg(current_asmdata.CurrAsmList, left.location, def_cgsize(left.resultdef), True);
    location_copy(location, left.location);
    resultreg  := location.Register;
    hregister1 := location.Register;
    if (location.loc = LOC_CREGISTER) then
    begin
      location.loc := LOC_REGISTER;
      resultreg    := cg.GetIntRegister(current_asmdata.CurrAsmList, OS_INT);
      location.Register := resultreg;
    end;
    { determine operator }
    if nodetype = shln then
      op := OP_SHL
    else
      op := OP_SHR;
    { shifting by a constant directly coded: }
    if (right.nodetype = ordconstn) then
    begin
      if tordconstnode(right).Value.svalue and 31 <> 0 then
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, op, OS_32, tordconstnode(right).Value.svalue and 31, hregister1, resultreg);
    end
    else
    begin
      { load shift count in a register if necessary }
      location_force_reg(current_asmdata.CurrAsmList, right.location, def_cgsize(right.resultdef), True);
      cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, op, OS_32, right.location.Register, hregister1, resultreg);
    end;
  end;
end;


{*****************************************************************************
                               TMIPSelNOTNODE
*****************************************************************************}

procedure tMIPSELnotnode.second_boolean;
var
  hl: tasmlabel;
begin
  { if the location is LOC_JUMP, we do the secondpass after the
          labels are allocated
        }
  if left.expectloc = LOC_JUMP then
  begin
    hl := current_procinfo.CurrTrueLabel;
    current_procinfo.CurrTrueLabel := current_procinfo.CurrFalseLabel;
    current_procinfo.CurrFalseLabel := hl;
    secondpass(left);
    maketojumpbool(current_asmdata.CurrAsmList, left, lr_load_regvars);
    hl := current_procinfo.CurrTrueLabel;
    current_procinfo.CurrTrueLabel := current_procinfo.CurrFalseLabel;
    current_procinfo.CurrFalseLabel := hl;
    location.loc := LOC_JUMP;
  end
  else
  begin
    secondpass(left);
    case left.location.loc of
      LOC_FLAGS:
      begin
        internalerror(2007011501);
      end;
      LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_CREFERENCE:
      begin
        location_force_reg(current_asmdata.CurrAsmList, left.location, def_cgsize(left.resultdef), True);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SEQ, NR_TCR0, left.location.Register, NR_R0));
        location_reset(location, LOC_REGISTER, OS_INT);
        location.Register := NR_TCR0;
      end;
      else
        internalerror(2003042401);
    end;
  end;
end;


begin
  cmoddivnode := tMIPSELmoddivnode;
  cshlshrnode := tMIPSELshlshrnode;
  cnotnode    := tMIPSELnotnode;
end.
