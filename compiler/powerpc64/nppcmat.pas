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

{ helper functions }
procedure getmagic_unsigned32(d : dword; out magic_m : dword; out magic_add : boolean; out magic_shift : dword);
var
    p : longint;
    nc, delta, q1, r1, q2, r2 : dword;
    
begin
    assert(d > 0);
    
    magic_add := false;
    nc := - 1 - (-d) mod d;
    p := 31; { initialize p }
    q1 := $80000000 div nc; { initialize q1 = 2p/nc }
    r1 := $80000000 - q1*nc; { initialize r1 = rem(2p,nc) }
    q2 := $7FFFFFFF div d; { initialize q2 = (2p-1)/d }
    r2 := $7FFFFFFF - q2*d; { initialize r2 = rem((2p-1),d) }
    repeat
        inc(p);
        if (r1 >= (nc - r1)) then begin
            q1 := 2 * q1 + 1; { update q1 }
            r1 := 2*r1 - nc; { update r1 }
        end else begin
            q1 := 2*q1; { update q1 }
            r1 := 2*r1; { update r1 }
        end;
        if ((r2 + 1) >= (d - r2)) then begin
            if (q2 >= $7FFFFFFF) then
                magic_add := true;
            q2 := 2*q2 + 1; { update q2 }
            r2 := 2*r2 + 1 - d; { update r2 }
        end else begin
            if (q2 >= $80000000) then 
                magic_add := true;
            q2 := 2*q2; { update q2 }
            r2 := 2*r2 + 1; { update r2 }
        end;
        delta := d - 1 - r2;
    until not ((p < 64) and ((q1 < delta) or ((q1 = delta) and (r1 = 0))));
    magic_m := q2 + 1; { resulting magic number }
    magic_shift := p - 32; { resulting shift }
end;

procedure getmagic_signed32(d : longint; out magic_m : longint; out magic_s : longint);
const
    two_31 : DWord = high(longint)+1;
var
    p : Longint;
    ad, anc, delta, q1, r1, q2, r2, t : DWord;
    
begin
    assert((d < -1) or (d > 1));

    ad := abs(d);
    t := two_31 + (DWord(d) shr 31);
    anc := t - 1 - t mod ad; { absolute value of nc }
    p := 31; { initialize p }
    q1 := two_31 div anc; { initialize q1 = 2p/abs(nc) }
    r1 := two_31 - q1*anc; { initialize r1 = rem(2p,abs(nc)) }
    q2 := two_31 div ad; { initialize q2 = 2p/abs(d) }
    r2 := two_31 - q2*ad; { initialize r2 = rem(2p,abs(d)) }
    repeat 
        inc(p);
        q1 := 2*q1; { update q1 = 2p/abs(nc) }
        r1 := 2*r1; { update r1 = rem(2p/abs(nc)) }
        if (r1 >= anc) then begin { must be unsigned comparison }
            inc(q1);
            dec(r1, anc);
        end;
        q2 := 2*q2; { update q2 = 2p/abs(d) }
        r2 := 2*r2; { update r2 = rem(2p/abs(d)) }
        if (r2 >= ad) then begin { must be unsigned comparison }
            inc(q2);
            dec(r2, ad);
        end;
        delta := ad - r2;
    until not ((q1 < delta) or ((q1 = delta) and (r1 = 0)));
    magic_m := q2 + 1;
    if (d < 0) then begin
        magic_m := -magic_m; { resulting magic number }
    end;
    magic_s := p - 32; { resulting shift }
end;

{ helper functions }
procedure getmagic_unsigned64(d : qword; out magic_m : qword; out magic_add : boolean; out magic_shift : qword);
const
  two_63 : QWord = $8000000000000000;  
var
    p : int64;
    nc, delta, q1, r1, q2, r2 : qword;
    
begin
  assert(d > 0);
    
  magic_add := false;
  nc := - 1 - (-d) mod d;
  p := 63; { initialize p }
  q1 := two_63 div nc; { initialize q1 = 2p/nc }
  r1 := two_63 - q1*nc; { initialize r1 = rem(2p,nc) }
  q2 := (two_63-1) div d; { initialize q2 = (2p-1)/d }
  r2 := (two_63-1) - q2*d; { initialize r2 = rem((2p-1),d) }
  repeat
    inc(p);
    if (r1 >= (nc - r1)) then begin
      q1 := 2 * q1 + 1; { update q1 }
      r1 := 2*r1 - nc; { update r1 }
    end else begin
      q1 := 2*q1; { update q1 }
      r1 := 2*r1; { update r1 }
    end;
    if ((r2 + 1) >= (d - r2)) then begin
      if (q2 >= (two_63-1)) then
        magic_add := true;
      q2 := 2*q2 + 1; { update q2 }
      r2 := 2*r2 + 1 - d; { update r2 }
    end else begin
      if (q2 >= two_63) then 
        magic_add := true;
      q2 := 2*q2; { update q2 }
      r2 := 2*r2 + 1; { update r2 }
    end;
    delta := d - 1 - r2;
  until not ((p < 128) and ((q1 < delta) or ((q1 = delta) and (r1 = 0))));
  magic_m := q2 + 1; { resulting magic number }
  magic_shift := p - 64; { resulting shift }
end;

procedure getmagic_signed64(d : int64; out magic_m : int64; out magic_s : int64);
const
  two_63 : QWord = $8000000000000000;  
var
  p : int64;
  ad, anc, delta, q1, r1, q2, r2, t : QWord;
    
begin
  assert((d < -1) or (d > 1));

  ad := abs(d);
  t := two_63 + (QWord(d) shr 63);
  anc := t - 1 - t mod ad; { absolute value of nc }
  p := 63; { initialize p }
  q1 := two_63 div anc; { initialize q1 = 2p/abs(nc) }
  r1 := two_63 - q1*anc; { initialize r1 = rem(2p,abs(nc)) }
  q2 := two_63 div ad; { initialize q2 = 2p/abs(d) }
  r2 := two_63 - q2*ad; { initialize r2 = rem(2p,abs(d)) }
  repeat 
    inc(p);
    q1 := 2*q1; { update q1 = 2p/abs(nc) }
    r1 := 2*r1; { update r1 = rem(2p/abs(nc)) }
    if (r1 >= anc) then begin { must be unsigned comparison }
      inc(q1);
      dec(r1, anc);
    end;
    q2 := 2*q2; { update q2 = 2p/abs(d) }
    r2 := 2*r2; { update r2 = rem(2p/abs(d)) }
    if (r2 >= ad) then begin { must be unsigned comparison }
      inc(q2);
      dec(r2, ad);
    end;
    delta := ad - r2;
  until not ((q1 < delta) or ((q1 = delta) and (r1 = 0)));
  magic_m := q2 + 1;
  if (d < 0) then begin
    magic_m := -magic_m; { resulting magic number }
  end;
  magic_s := p - 64; { resulting shift }
end;



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
    ((A_DIVDU,A_DIVDU_),(A_DIVD,A_DIVDO_));
  zerocond: tasmcond = (dirhint: DH_Plus; simple: true; cond:C_NE; cr: RS_CR7);
var
  power  : longint;
  op  : tasmop;
  numerator, divider,
  resultreg  : tregister;
  size       : TCgSize;
  hl : tasmlabel;
  done: boolean;
         
  procedure genOrdConstNodeDiv;
  const
    negops : array[boolean] of tasmop = (A_NEG, A_NEGO);
  var
    magic, shift : int64;
    u_magic, u_shift : qword;
    u_add : boolean;
             
    divreg : tregister;
  begin
    if (tordconstnode(right).value = 0) then begin
      internalerror(2005061701);
    end else if (tordconstnode(right).value = 1) then begin
      cg.a_load_reg_reg(exprasmlist, OS_INT, OS_INT, numerator, resultreg);
    end else if (tordconstnode(right).value = -1) then begin
      { note: only in the signed case possible..., may overflow }
      exprasmlist.concat(taicpu.op_reg_reg(negops[cs_check_overflow in aktlocalswitches], resultreg, numerator));
    end else if (ispowerof2(tordconstnode(right).value, power)) then begin
      if (is_signed(right.resulttype.def)) then begin
        { From "The PowerPC Compiler Writer's Guide", pg. 52ff          }
        cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_INT, power,
        numerator, resultreg);
        exprasmlist.concat(taicpu.op_reg_reg(A_ADDZE, resultreg, resultreg));
      end else begin
        cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, power, numerator, resultreg)
      end;
    end else begin
      { replace division by multiplication, both implementations }
      { from "The PowerPC Compiler Writer's Guide" pg. 53ff      }
      divreg := cg.getintregister(exprasmlist, OS_INT);
      if (is_signed(right.resulttype.def)) then begin
        getmagic_signed64(tordconstnode(right).value, magic, shift);
        { load magic value }
        cg.a_load_const_reg(exprasmlist, OS_INT, magic, divreg);
        { multiply }
        exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULHD, resultreg, numerator, divreg));
        { add/subtract numerator }
        if (tordconstnode(right).value > 0) and (magic < 0) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, numerator, resultreg, resultreg);
        end else if (tordconstnode(right).value < 0) and (magic > 0) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_SUB, OS_INT, numerator, resultreg, resultreg);
        end;
        { shift shift places to the right (arithmetic) }
        cg.a_op_const_reg_reg(exprasmlist, OP_SAR, OS_INT, shift, resultreg, resultreg);                     
        { extract and add sign bit }
        if (tordconstnode(right).value >= 0) then begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, 63, numerator, divreg);
        end else begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, 63, resultreg, divreg);
        end;                     
        cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, resultreg, divreg, resultreg);
      end else begin
        getmagic_unsigned64(tordconstnode(right).value, u_magic, u_add, u_shift);
        { load magic in divreg }
        cg.a_load_const_reg(exprasmlist, OS_INT, u_magic, divreg);
        exprasmlist.concat(taicpu.op_reg_reg_reg(A_MULHDU, resultreg, numerator, divreg));
        if (u_add) then begin
          cg.a_op_reg_reg_reg(exprasmlist, OP_SUB, OS_INT, resultreg, numerator, divreg);
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT,  1, divreg, divreg);
          cg.a_op_reg_reg_reg(exprasmlist, OP_ADD, OS_INT, divreg, resultreg, divreg);
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, u_shift-1, divreg, resultreg);
        end else begin
          cg.a_op_const_reg_reg(exprasmlist, OP_SHR, OS_INT, u_shift, resultreg, resultreg);
        end;
      end;
    end;
    done := true;
  end;

  procedure genOrdConstNodeMod;
  var
    modreg, maskreg, tempreg : tregister;
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
        cg.a_op_const_reg_reg(exprasmlist, OP_AND, OS_INT, tordconstnode(right).value-1, numerator, resultreg);
      end;
    end else begin
      genOrdConstNodeDiv();
      cg.a_op_const_reg_reg(exprasmlist, OP_MUL, OS_INT, tordconstnode(right).value, resultreg, resultreg);
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
(*
  if (right.nodetype = ordconstn) then begin
    if (nodetype = divn) then
      genOrdConstNodeDiv
    else
      genOrdConstNodeMod;
    done := true;
  end;
*)

  if (not done) then begin
    { load divider in a register if necessary }
    location_force_reg(exprasmlist,right.location,
      def_cgsize(right.resulttype.def),true);
    if (right.nodetype <> ordconstn) then
      exprasmlist.concat(taicpu.op_reg_reg_const(A_CMPDI, NR_CR7,
        right.location.register, 0))
    else begin
      if (tordconstnode(right).value = 0) then 
        internalerror(2005100301);
    end;
    divider := right.location.register;

    { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
    op := divops[is_signed(right.resulttype.def),
      cs_check_overflow in aktlocalswitches];
    exprasmlist.concat(taicpu.op_reg_reg_reg(op, resultreg, numerator,
      divider));

    if (nodetype = modn) then begin
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

(*
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
  end else begin
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

    if (nodetype = modn) then begin
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
  if (right.nodetype <> ordconstn) then begin
    objectlibrary.getjumplabel(hl);
    exprasmlist.concat(taicpu.op_cond_sym(A_BC, zerocond, hl));
    cg.a_call_name(exprasmlist, 'FPC_DIVBYZERO');
    cg.a_label(exprasmlist, hl);
  end;
  cg.g_overflowcheck(exprasmlist, location, resulttype.def);
end;
*)
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

