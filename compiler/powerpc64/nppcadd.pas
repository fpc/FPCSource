{
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the PowerPC64

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
unit nppcadd;

{$I fpcdefs.inc}

interface

uses
  node, nadd, ncgadd, ngppcadd, cpubase;

type
  tppcaddnode = class(tgenppcaddnode)
    procedure pass_generate_code override;
  private
    procedure emit_compare(unsigned: boolean); override;
  end;

implementation

uses
  sysutils,

  globtype, systems,
  cutils, verbose, globals,
  symconst, symdef, paramgr,
  aasmbase, aasmtai,aasmdata, aasmcpu, defutil, htypechk,
  cgbase, cpuinfo, pass_1, pass_2, regvars,
  cpupara, cgcpu, cgutils,procinfo,
  ncon, nset,
  ncgutil, tgobj, rgobj, rgcpu, cgobj;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

procedure tppcaddnode.emit_compare(unsigned: boolean);
const
  {                  unsigned  useconst  32bit-op }
  cmpop_table : array[boolean, boolean, boolean] of TAsmOp = (
    ((A_CMPD, A_CMPW), (A_CMPDI, A_CMPWI)),
    ((A_CMPLD, A_CMPLW), (A_CMPLDI, A_CMPLWI))
   );

var
  op: TAsmOp;
  tmpreg: TRegister;
  useconst: boolean;

  opsize : TCgSize;
begin
  { get the constant on the right if there is one }
  if (left.location.loc = LOC_CONSTANT) then
    swapleftright;

  opsize := def_cgsize(left.resultdef);

  {$IFDEF EXTDEBUG}
  current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('tppcaddnode.emit_compare ' + inttostr(ord(opsize)) + ' ' + inttostr(tcgsize2size[opsize]))));
  {$ENDIF EXTDEBUG}

  { can we use a signed comparison or not? In case of equal/unequal comparison
   we can check whether this is possible because it does not matter. }
  if (right.location.loc = LOC_CONSTANT) then
    if (nodetype in [equaln,unequaln]) then
      if (unsigned and (aword(right.location.value) > high(word))) or
        (not unsigned and (aint(right.location.value) < low(smallint)) or
        (aint(right.location.value) > high(smallint))) then
        { we can then maybe use a constant in the 'othersigned' case
        (the sign doesn't matter for equal/unequal) }
        unsigned := not unsigned;

  { calculate the size of the comparison because ppc64 only has 32 and 64
   bit comparison opcodes; prefer 32 bits }
  if (not (opsize in [OS_32, OS_S32, OS_64, OS_S64])) then begin
    if (unsigned) then
      opsize := OS_32
    else
      opsize := OS_S32;
    cg.a_load_reg_reg(current_asmdata.CurrAsmList, def_cgsize(left.resultdef), opsize,
      left.location.register, left.location.register);
  end;

  { can we use an immediate, or do we have to load the
   constant in a register first? }
  if (right.location.loc = LOC_CONSTANT) then begin
    if (unsigned and
      (aword(right.location.value) <= high(word))) or
      (not (unsigned) and
      (aint(right.location.value) >= low(smallint)) and (aint(right.location.value) <= high(smallint))) then
      useconst := true
    else begin
      useconst := false;
      tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
      cg.a_load_const_reg(current_asmdata.CurrAsmList, opsize, right.location.value, tmpreg);
    end
  end else
    useconst := false;

  location.loc := LOC_FLAGS;
  location.resflags := getresflags;

  op := cmpop_table[unsigned, useconst, opsize in [OS_S32, OS_32]];

  { actually do the operation }
  if (right.location.loc = LOC_CONSTANT) then begin
    if useconst then
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(op, left.location.register,
        longint(right.location.value)))
    else
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.register, tmpreg));
  end else
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.register,
      right.location.register));
end;

{*****************************************************************************
                                pass_2
*****************************************************************************}

procedure tppcaddnode.pass_generate_code;
{ is also being used for xor, and "mul", "sub, or and comparative }
{ operators                                                }
var
  cgop: topcg;
  op: tasmop;
  tmpreg: tregister;
  hl: tasmlabel;
  cmpop: boolean;
  checkoverflow: boolean;

  { true, if unsigned types are compared }
  unsigned: boolean;

begin
  { to make it more readable, string and set (not smallset!) have their
    own procedures }
  case left.resultdef.typ of
    orddef:
      begin
        { handling boolean expressions }
        if is_boolean(left.resultdef) and
          is_boolean(right.resultdef) then
        begin
          second_addboolean;
          exit;
        end;
      end;
    stringdef:
      begin
        internalerror(2002072402);
        exit;
      end;
    setdef:
      begin
        { normalsets are already handled in pass1 }
        if not is_smallset(left.resultdef) then
          internalerror(200109041);
        second_addsmallset;
        exit;
      end;
    arraydef:
      begin
{$IFDEF SUPPORT_MMX}
        if is_mmx_able_array(left.resultdef) then
        begin
          second_addmmx;
          exit;
        end;
{$ENDIF SUPPORT_MMX}
      end;
    floatdef:
      begin
        second_addfloat;
        exit;
      end;
  end;

  { defaults }
  cmpop := nodetype in [ltn, lten, gtn, gten, equaln, unequaln];
  unsigned := not (is_signed(left.resultdef)) or
    not (is_signed(right.resultdef));

  pass_left_and_right;

  { Convert flags to register first }
  { can any of these things be in the flags actually?? (JM) }

  if (left.location.loc = LOC_FLAGS) or
    (right.location.loc = LOC_FLAGS) then
    internalerror(2002072602);

  { set result location }
  if not cmpop then
    location_reset(location, LOC_REGISTER, def_cgsize(resultdef))
  else
    location_reset(location, LOC_FLAGS, OS_NO);

  checkoverflow:=
    (nodetype in [addn,subn,muln]) and
    (cs_check_overflow in current_settings.localswitches) and
    (left.resultdef.typ<>pointerdef) and
    (right.resultdef.typ<>pointerdef);

  load_left_right(cmpop, checkoverflow);

  if not (cmpop) then
    location.register := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);

  if not (checkoverflow) then begin
    case nodetype of
      addn, muln, xorn, orn, andn:
        begin
          case nodetype of
            addn:
              cgop := OP_ADD;
            muln:
              if unsigned then
                cgop := OP_MUL
              else
                cgop := OP_IMUL;
            xorn:
              cgop := OP_XOR;
            orn:
              cgop := OP_OR;
            andn:
              cgop := OP_AND;
          end;
          if (left.location.loc = LOC_CONSTANT) then
            swapleftright;
          if (right.location.loc <> LOC_CONSTANT) then
            cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, cgop, OS_INT,
              left.location.register, right.location.register,
              location.register)
          else
            cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, cgop, OS_INT,
              right.location.value, left.location.register,
              location.register);
        end;
      subn:
        begin
          if (nf_swapped in flags) then
            swapleftright;
          if left.location.loc <> LOC_CONSTANT then
            if right.location.loc <> LOC_CONSTANT then begin
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT,
                right.location.register, left.location.register,
                location.register);
            end else begin
              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT,
                right.location.value, left.location.register,
                location.register);
            end
          else
          begin
            tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT,
              left.location.value, tmpreg);
            cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT,
              right.location.register, tmpreg, location.register);
          end;
        end;
      ltn, lten, gtn, gten, equaln, unequaln:
        begin
          {$ifdef extdebug}
          current_asmdata.CurrAsmList.concat(tai_comment.create('tppcaddnode.pass2'));
          {$endif extdebug}

          emit_compare(unsigned);
        end;
    end;
  end
  else
    // overflow checking is on and we have an addn, subn or muln
  begin
    if is_signed(resultdef) then
    begin
      case nodetype of
        addn:
          op := A_ADDO;
        subn:
          begin
            op := A_SUBO;
            if (nf_swapped in flags) then
              swapleftright;
          end;
        muln:
          op := A_MULLDO;
      else
        internalerror(2002072601);
      end;
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op, location.register,
        left.location.register, right.location.register));
      cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resultdef);
    end
    else
    begin
      case nodetype of
        addn:
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD, location.register,
              left.location.register, right.location.register));
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMPLD, location.register,
              left.location.register));
            cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resultdef);
          end;
        subn:
          begin
            if (nf_swapped in flags) then
              swapleftright;
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUB, location.register,
              left.location.register, right.location.register));
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMPLD,
              left.location.register, location.register));
            cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resultdef);
          end;
        muln:
          begin
            { calculate the upper 64 bits of the product, = 0 if no overflow }
            cg.a_reg_alloc(current_asmdata.CurrAsmList, NR_R0);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULHDU_, NR_R0,
              left.location.register, right.location.register));
            cg.a_reg_dealloc(current_asmdata.CurrAsmList, NR_R0);
            { calculate the real result }
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULLD, location.register,
              left.location.register, right.location.register));
            { g_overflowcheck generates a OC_AE instead of OC_EQ :/ }
            current_asmdata.getjumplabel(hl);
            tcgppc(cg).a_jmp_cond(current_asmdata.CurrAsmList, OC_EQ, hl);
            cg.a_call_name(current_asmdata.CurrAsmList, 'FPC_OVERFLOW');
            cg.a_label(current_asmdata.CurrAsmList, hl);
          end;
      end;
    end;
  end;
end;

begin
  caddnode := tppcaddnode;
end.

