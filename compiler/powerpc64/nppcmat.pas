{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for math nodes

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
unit nppcmat;

{$I fpcdefs.inc}

interface

uses
  node, nmat;

type
  tppcmoddivnode = class(tmoddivnode)
    function pass_1: tnode; override;
    procedure pass_2; override;
  end;

  tppcshlshrnode = class(tshlshrnode)
    procedure pass_2; override;
  end;

  tppcunaryminusnode = class(tunaryminusnode)
    procedure pass_2; override;
  end;

  tppcnotnode = class(tnotnode)
    procedure pass_2; override;
  end;

implementation

uses
  sysutils,
  globtype, systems,
  cutils, verbose, globals,
  symconst, symdef,
  aasmbase, aasmcpu, aasmtai,
  defutil,
  cgbase, cgutils, cgobj, pass_1, pass_2,
  ncon, procinfo,
  cpubase, cpuinfo,
  ncgutil, cgcpu, rgobj;

{*****************************************************************************
                             TPPCMODDIVNODE
*****************************************************************************}

function tppcmoddivnode.pass_1: tnode;
begin
  result := inherited pass_1;
  if not assigned(result) then
    include(current_procinfo.flags, pi_do_call);
end;

procedure tppcmoddivnode.pass_2;
const         { signed   overflow }
  divops: array[boolean, boolean] of tasmop =
    ((A_DIVDU, A_DIVDU_),(A_DIVD, A_DIVDO_));
  divcgops : array[boolean] of TOpCG = (OP_DIV, OP_IDIV);
  zerocond: tasmcond = (dirhint: DH_Plus; simple: true; cond:C_NE; cr: RS_CR7);
  tcgsize2native : array[OS_8..OS_S128] of tcgsize = (
    OS_64, OS_64, OS_64, OS_64, OS_NO, 
    OS_S64, OS_S64, OS_S64, OS_S64, OS_NO
    );
var
  power  : longint;
  op  : tasmop;
  numerator, divider,
  resultreg  : tregister;
  size       : TCgSize;
  hl : tasmlabel;
  done: boolean;
         
  procedure genOrdConstNodeMod;
  var
    modreg, maskreg, tempreg : tregister;
    isNegPower : boolean;
  begin
    if (tordconstnode(right).value = 0) then begin
      internalerror(2005061702);
    end else if (abs(tordconstnode(right).value) = 1) then begin
      { x mod +/-1 is always zero }
      cg.a_load_const_reg(exprasmlist, OS_INT, 0, resultreg);
    end else if (ispowerof2(tordconstnode(right).value, power)) then begin
      if (is_signed(right.resulttype.def)) then begin
        tempreg := cg.getintregister(exprasmlist, OS_INT);
        maskreg := cg.getintregister(exprasmlist, OS_INT);
        modreg := cg.getintregister(exprasmlist, OS_INT);

        cg.a_load_const_reg(exprasmlist, OS_INT, abs(tordconstnode(right).value)-1, modreg);
        cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_INT, 63, numerator, maskreg);
        cg.a_op_reg_reg_reg(exprasmlist, OP_AND, OS_INT, numerator, modreg, tempreg);

        exprasmlist.concat(taicpu.op_reg_reg_reg(A_ANDC, maskreg, maskreg, modreg));
        exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC, modreg, tempreg, 0));
        exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUBFE, modreg, modreg, modreg));
        cg.a_op_reg_reg_reg(exprasmlist, OP_AND, OS_INT, modreg, maskreg, maskreg);
        cg.a_op_reg_reg_reg(exprasmlist, OP_OR, OS_INT, maskreg, tempreg, resultreg);
      end else begin
        cg.a_op_const_reg_reg(exprasmlist, OP_AND, OS_INT, tordconstnode(right).value-1, numerator, 
          resultreg);
      end;
    end else begin
      cg.a_op_const_reg_reg(exprasmlist, divCgOps[is_signed(right.resulttype.def)], OS_INT, 
        tordconstnode(right).value, numerator, resultreg);
      cg.a_op_const_reg_reg(exprasmlist, OP_MUL, OS_INT, tordconstnode(right).value, resultreg, 
        resultreg);
      cg.a_op_reg_reg_reg(exprasmlist, OP_SUB, OS_INT, resultreg, numerator, resultreg);
    end;
  end;

         
begin
  secondpass(left);
  secondpass(right);
  location_copy(location,left.location);

  { put numerator in register }
  size:=def_cgsize(left.resulttype.def);
  location_force_reg(exprasmlist,left.location,
    size,true);
  location_copy(location,left.location);
  numerator := location.register;
  resultreg := location.register;
  if (location.loc = LOC_CREGISTER) then begin
    location.loc := LOC_REGISTER;
    location.register := cg.getintregister(exprasmlist,size);
    resultreg := location.register;
  end else if (nodetype = modn) or (right.nodetype = ordconstn) then begin
    { for a modulus op, and for const nodes we need the result register
     to be an extra register }
    resultreg := cg.getintregister(exprasmlist,size);
  end;
  done := false;
  if (cs_opt_level1 in aktoptimizerswitches) and (right.nodetype = ordconstn) then begin
    if (nodetype = divn) then
      cg.a_op_const_reg_reg(exprasmlist, divCgOps[is_signed(right.resulttype.def)], 
        size, tordconstnode(right).value, numerator, resultreg)
    else 
      genOrdConstNodeMod;
    done := true;
  end;

  if (not done) then begin
    { load divider in a register if necessary }
    location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
    if (right.nodetype <> ordconstn) then
      exprasmlist.concat(taicpu.op_reg_reg_const(A_CMPDI, NR_CR7,
        right.location.register, 0))
    else begin
      if (tordconstnode(right).value = 0) then 
        internalerror(2005100301);
    end;
    divider := right.location.register;

    { select the correct opcode according to the sign of the result, whether we need
     overflow checking }
    op := divops[is_signed(right.resulttype.def), cs_check_overflow in aktlocalswitches];
    exprasmlist.concat(taicpu.op_reg_reg_reg(op, resultreg, numerator,
      divider));

    if (nodetype = modn) then begin
      { multiply with the divisor again, taking care of the correct size }
      exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULLD,resultreg,
          divider,resultreg));
      exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB,location.register,
        numerator,resultreg));
      resultreg := location.register;
    end;
  end;
  { set result location }
  location.loc:=LOC_REGISTER;
  location.register:=resultreg;
  if right.nodetype <> ordconstn then begin
    objectlibrary.getjumplabel(hl);
    exprasmlist.concat(taicpu.op_cond_sym(A_BC,zerocond,hl));
    cg.a_call_name(exprasmlist,'FPC_DIVBYZERO');
    cg.a_label(exprasmlist,hl);
  end;
  { unsigned division/module can only overflow in case of division by zero
   (but checking this overflow flag is more convoluted than performing a  
   simple comparison with 0)                                             }
  if is_signed(right.resulttype.def) then
    cg.g_overflowcheck(exprasmlist,location,resulttype.def);
end;

{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}

procedure tppcshlshrnode.pass_2;

var
  resultreg, hregister1, hregister2 : tregister;
  
  op: topcg;
  asmop1, asmop2: tasmop;
  shiftval: aint;

begin
  secondpass(left);
  secondpass(right);

  { load left operators in a register }
  location_force_reg(exprasmlist, left.location,
    def_cgsize(left.resulttype.def), true);
  location_copy(location, left.location);
  resultreg := location.register;
  hregister1 := location.register;
  if (location.loc = LOC_CREGISTER) then begin
    location.loc := LOC_REGISTER;
    resultreg := cg.getintregister(exprasmlist, OS_INT);
    location.register := resultreg;
  end;

  { determine operator }
  if nodetype = shln then
    op := OP_SHL
  else
    op := OP_SHR;

  { shifting by a constant directly coded: }
  if (right.nodetype = ordconstn) then begin
    // result types with size < 32 bits have their shift values masked
    // differently... :/
    shiftval := tordconstnode(right).value and (tcgsize2size[def_cgsize(resulttype.def)] * 8 -1);
    cg.a_op_const_reg_reg(exprasmlist, op, def_cgsize(resulttype.def),
      shiftval, hregister1, resultreg)
  end else begin
    { load shift count in a register if necessary }
    location_force_reg(exprasmlist, right.location,
      def_cgsize(right.resulttype.def), true);
    hregister2 := right.location.register;
    cg.a_op_reg_reg_reg(exprasmlist, op, def_cgsize(resulttype.def), hregister2,
      hregister1, resultreg);
  end;
end;

{*****************************************************************************
                          TPPCUNARYMINUSNODE
*****************************************************************************}

procedure tppcunaryminusnode.pass_2;

var
  src1: tregister;
  op: tasmop;

begin
  secondpass(left);
  begin
    location_copy(location, left.location);
    location.loc := LOC_REGISTER;
    case left.location.loc of
      LOC_FPUREGISTER, LOC_REGISTER:
        begin
          src1 := left.location.register;
          location.register := src1;
        end;
      LOC_CFPUREGISTER, LOC_CREGISTER:
        begin
          src1 := left.location.register;
          if left.location.loc = LOC_CREGISTER then
            location.register := cg.getintregister(exprasmlist, OS_INT)
          else
            location.register := cg.getfpuregister(exprasmlist, location.size);
        end;
      LOC_REFERENCE, LOC_CREFERENCE:
        begin
          if (left.resulttype.def.deftype = floatdef) then begin
            src1 := cg.getfpuregister(exprasmlist,
              def_cgsize(left.resulttype.def));
            location.register := src1;
            cg.a_loadfpu_ref_reg(exprasmlist,
              def_cgsize(left.resulttype.def),
              left.location.reference, src1);
          end else begin
            src1 := cg.getintregister(exprasmlist, OS_64);
            location.register := src1;
            cg.a_load_ref_reg(exprasmlist, OS_64, OS_64,
              left.location.reference, src1);
          end;
        end;
    end;
    { choose appropriate operand }
    if left.resulttype.def.deftype <> floatdef then begin
      if not (cs_check_overflow in aktlocalswitches) then
        op := A_NEG
      else
        op := A_NEGO_;
      location.loc := LOC_REGISTER;
    end else begin
      op := A_FNEG;
      location.loc := LOC_FPUREGISTER;
    end;
    { emit operation }
    exprasmlist.concat(taicpu.op_reg_reg(op, location.register, src1));
  end;
  cg.g_overflowcheck(exprasmlist, location, resulttype.def);
end;

{*****************************************************************************
                               TPPCNOTNODE
*****************************************************************************}

procedure tppcnotnode.pass_2;

var
  hl: tasmlabel;

begin
  if is_boolean(resulttype.def) then
  begin
    { if the location is LOC_JUMP, we do the secondpass after the
      labels are allocated
    }
    if left.expectloc = LOC_JUMP then
    begin
      hl := truelabel;
      truelabel := falselabel;
      falselabel := hl;
      secondpass(left);
      maketojumpbool(exprasmlist, left, lr_load_regvars);
      hl := truelabel;
      truelabel := falselabel;
      falselabel := hl;
      location.loc := LOC_JUMP;
    end
    else
    begin
      secondpass(left);
      case left.location.loc of
        LOC_FLAGS:
          begin
            location_copy(location, left.location);
            inverse_flags(location.resflags);
          end;
        LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_CREFERENCE:
          begin
            location_force_reg(exprasmlist, left.location,
              def_cgsize(left.resulttype.def), true);
            exprasmlist.concat(taicpu.op_reg_const(A_CMPDI,
              left.location.register, 0));
            location_reset(location, LOC_FLAGS, OS_NO);
            location.resflags.cr := RS_CR0;
            location.resflags.flag := F_EQ;
          end;
      else
        internalerror(2003042401);
      end;
    end;
  end
  else
  begin
    secondpass(left);
    location_force_reg(exprasmlist, left.location,
      def_cgsize(left.resulttype.def), true);
    location_copy(location, left.location);
    location.loc := LOC_REGISTER;
    location.register := cg.getintregister(exprasmlist, OS_INT);
    { perform the NOT operation }
    cg.a_op_reg_reg(exprasmlist, OP_NOT, def_cgsize(resulttype.def),
      left.location.register,
      location.register);
  end;
end;

begin
  cmoddivnode := tppcmoddivnode;
  cshlshrnode := tppcshlshrnode;
  cunaryminusnode := tppcunaryminusnode;
  cnotnode := tppcnotnode;
end.

