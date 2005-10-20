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
const
  // ts: todo, use 32 bit operations if possible (much faster!)
  { signed   overflow }
  divops: array[boolean, boolean] of tasmop =
  ((A_DIVDU, A_DIVDUO_), (A_DIVD, A_DIVDO_));
  zerocond: tasmcond = (dirhint: DH_Plus; simple: true; cond: C_NE; cr: RS_CR1);
var
  power: longint;
  op: tasmop;
  numerator,
    divider,
    resultreg: tregister;
  size: Tcgsize;
  hl: tasmlabel;

begin
  secondpass(left);
  secondpass(right);
  location_copy(location, left.location);

  { put numerator in register }
  size := def_cgsize(left.resulttype.def);
  location_force_reg(exprasmlist, left.location,
    size, true);
  location_copy(location, left.location);
  numerator := location.register;
  resultreg := location.register;
  if (location.loc = LOC_CREGISTER) then
  begin
    location.loc := LOC_REGISTER;
    location.register := cg.getintregister(exprasmlist, size);
    resultreg := location.register;
  end;
  if (nodetype = modn) then
  begin
    resultreg := cg.getintregister(exprasmlist, size);
  end;

  if (nodetype = divn) and
    (right.nodetype = ordconstn) and
    ispowerof2(tordconstnode(right).value, power) then
  begin
  	if (is_signed(right.resulttype.def)) then begin
      { From "The PowerPC Compiler Writer's Guide":                   }
      { This code uses the fact that, in the PowerPC architecture,    }
      { the shift right algebraic instructions set the Carry bit if   }
      { the source register contains a negative number and one or     }
      { more 1-bits are shifted out. Otherwise, the carry bit is      }
      { cleared. The addze instruction corrects the quotient, if      }
      { necessary, when the dividend is negative. For example, if     }
      { n = -13, (0xFFFF_FFF3), and k = 2, after executing the srawi  }
      { instruction, q = -4 (0xFFFF_FFFC) and CA = 1. After executing }
      { the addze instruction, q = -3, the correct quotient.          }
      cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_64, power,
        numerator, resultreg);
      exprasmlist.concat(taicpu.op_reg_reg(A_ADDZE, resultreg, resultreg));
    end else begin
      cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, power, numerator, resultreg);
    end;
  end
  else
  begin
    { load divider in a register if necessary }
    location_force_reg(exprasmlist, right.location,
      def_cgsize(right.resulttype.def), true);
    if (right.nodetype <> ordconstn) then
{$NOTE ts: testme}
      exprasmlist.concat(taicpu.op_reg_reg_const(A_CMPDI, NR_CR1,
        right.location.register, 0));
    divider := right.location.register;

    { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
    { And on PPC, the only way to catch a div-by-0 is by checking  }
    { the overflow flag (JM)                                       }
    op := divops[is_signed(right.resulttype.def),
      cs_check_overflow in aktlocalswitches];
    exprasmlist.concat(taicpu.op_reg_reg_reg(op, resultreg, numerator,
      divider));

    if (nodetype = modn) then
    begin
{$NOTE ts:testme}
      exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULLD, resultreg,
        divider, resultreg));
      exprasmlist.concat(taicpu.op_reg_reg_reg(A_SUB, location.register,
        numerator, resultreg));
      resultreg := location.register;
    end;
  end;
  { set result location }
  location.loc := LOC_REGISTER;
  location.register := resultreg;
  if right.nodetype <> ordconstn then
  begin
    objectlibrary.getjumplabel(hl);
    exprasmlist.concat(taicpu.op_cond_sym(A_BC, zerocond, hl));
    cg.a_call_name(exprasmlist, 'FPC_DIVBYZERO');
    cg.a_label(exprasmlist, hl);
  end;
  cg.g_overflowcheck(exprasmlist, location, resulttype.def);
end;

{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}


procedure tppcshlshrnode.pass_2;

var
  resultreg, hregister1, hregister2,
    hreg64hi, hreg64lo: tregister;
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
    resultreg := cg.getintregister(exprasmlist, OS_64);
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
          if (left.resulttype.def.deftype = floatdef) then
          begin
            src1 := cg.getfpuregister(exprasmlist,
              def_cgsize(left.resulttype.def));
            location.register := src1;
            cg.a_loadfpu_ref_reg(exprasmlist,
              def_cgsize(left.resulttype.def),
              left.location.reference, src1);
          end
          else
          begin
            src1 := cg.getintregister(exprasmlist, OS_64);
            location.register := src1;
            cg.a_load_ref_reg(exprasmlist, OS_64, OS_64,
              left.location.reference, src1);
          end;
        end;
    end;
    { choose appropriate operand }
    if left.resulttype.def.deftype <> floatdef then
    begin
      if not (cs_check_overflow in aktlocalswitches) then
        op := A_NEG
      else
        op := A_NEGO_;
      location.loc := LOC_REGISTER;
    end
    else
    begin
      op := A_FNEG;
      location.loc := LOC_FPUREGISTER;
    end;
    { emit operation }
    exprasmlist.concat(taicpu.op_reg_reg(op, location.register, src1));
  end;
  { Here was a problem...     }
  { Operand to be negated always     }
  { seems to be converted to signed  }
  { 32-bit before doing neg!!     }
  { So this is useless...     }
  { that's not true: -2^31 gives an overflow error if it is negated (FK) }
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

