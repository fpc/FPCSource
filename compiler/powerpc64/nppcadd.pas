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
  node, nadd, ncgadd, cpubase;

type
  tppcaddnode = class(tcgaddnode)
    function pass_1: tnode; override;
    procedure pass_2; override;
  private
    procedure pass_left_and_right;
    procedure load_left_right(cmpop, load_constants: boolean);
    function getresflags: tresflags;
    procedure emit_compare(unsigned: boolean);
    procedure second_addfloat; override;
    procedure second_addboolean; override;
    procedure second_addsmallset; override;
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
                                  Pass 1
*****************************************************************************}

function tppcaddnode.pass_1: tnode;
begin
  resulttypepass(left);
  if (nodetype in [equaln, unequaln]) and
    (left.resulttype.def.deftype = orddef) {and
  is_64bit(left.resulttype.def)}then
  begin
    result := nil;
    firstpass(left);
    firstpass(right);
    expectloc := LOC_FLAGS;
    calcregisters(self, 2, 0, 0);
    exit;
  end;
  result := inherited pass_1;
end;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

procedure tppcaddnode.pass_left_and_right;
begin
  { calculate the operator which is more difficult }
  firstcomplex(self);

  { in case of constant put it to the left }
  if (left.nodetype = ordconstn) then
    swapleftright;

  secondpass(left);
  secondpass(right);
end;

procedure tppcaddnode.load_left_right(cmpop, load_constants: boolean);

  procedure load_node(var n: tnode);
  begin
    case n.location.loc of
      LOC_CREGISTER:
        ;
      LOC_REGISTER:
        if not cmpop then
        begin
          location.register := n.location.register;
        end;
      LOC_REFERENCE, LOC_CREFERENCE:
        begin
          location_force_reg(current_asmdata.CurrAsmList, n.location,
            def_cgsize(n.resulttype.def), false);
          if not cmpop then
          begin
            location.register := n.location.register;
          end;
        end;
      LOC_CONSTANT:
        begin
          if load_constants then
          begin
            location_force_reg(current_asmdata.CurrAsmList, n.location,
              def_cgsize(n.resulttype.def), false);
            if not cmpop then
              location.register := n.location.register;
          end;
        end;
      else
        location_force_reg(current_asmdata.CurrAsmList,n.location,def_cgsize(n.resulttype.def),false);
    end;
  end;

begin
  load_node(left);
  load_node(right);
  if not (cmpop) and
    (location.register = NR_NO) then
  begin
    location.register := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
  end;
end;

function tppcaddnode.getresflags: tresflags;
begin
  if (left.resulttype.def.deftype <> floatdef) then
    result.cr := RS_CR0
  else
    result.cr := RS_CR1;
  case nodetype of
    equaln: result.flag := F_EQ;
    unequaln: result.flag := F_NE;
  else
    if nf_swaped in flags then
      case nodetype of
        ltn: result.flag := F_GT;
        lten: result.flag := F_GE;
        gtn: result.flag := F_LT;
        gten: result.flag := F_LE;
      end
    else
      case nodetype of
        ltn: result.flag := F_LT;
        lten: result.flag := F_LE;
        gtn: result.flag := F_GT;
        gten: result.flag := F_GE;
      end;
  end
end;

procedure tppcaddnode.emit_compare(unsigned: boolean);
var
  op: tasmop;
  tmpreg: tregister;
  useconst: boolean;
begin
  // get the constant on the right if there is one
  if (left.location.loc = LOC_CONSTANT) then
    swapleftright;
  // can we use an immediate, or do we have to load the
  // constant in a register first?
  if (right.location.loc = LOC_CONSTANT) then begin
    if (nodetype in [equaln, unequaln]) then
      if (unsigned and
        (aword(right.location.value) > high(word))) or
        (not unsigned and
        (aint(right.location.value) < low(smallint)) or
        (aint(right.location.value) > high(smallint))) then
        { we can then maybe use a constant in the 'othersigned' case
         (the sign doesn't matter for // equal/unequal)}
        unsigned := not unsigned;

    if (unsigned and
      (aword(right.location.value) <= high(word))) or
      (not (unsigned) and
      (aint(right.location.value) >= low(smallint)) and
      (aint(right.location.value) <= high(smallint))) then
      useconst := true
    else begin
      useconst := false;
      tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
      cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT,
        right.location.value, tmpreg);
    end
  end else
    useconst := false;
  location.loc := LOC_FLAGS;
  location.resflags := getresflags;
  if not unsigned then
    if useconst then
      op := A_CMPDI
    else
      op := A_CMPD
  else if useconst then
    op := A_CMPLDI
  else
    op := A_CMPLD;

  if (right.location.loc = LOC_CONSTANT) then begin
    if useconst then
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(op, left.location.register,
        longint(right.location.value)))
    else
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, left.location.register, tmpreg));
  end else
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,
      left.location.register, right.location.register));
end;

{*****************************************************************************
                                AddBoolean
*****************************************************************************}

procedure tppcaddnode.second_addboolean;
var
  cgop: TOpCg;
  cgsize: TCgSize;
  cmpop,
  isjump: boolean;
  otl, ofl: tasmlabel;
begin
  { calculate the operator which is more difficult }
  firstcomplex(self);

  cmpop := false;
  if (torddef(left.resulttype.def).typ = bool8bit) or
    (torddef(right.resulttype.def).typ = bool8bit) then
    cgsize := OS_8
  else if (torddef(left.resulttype.def).typ = bool16bit) or
    (torddef(right.resulttype.def).typ = bool16bit) then
    cgsize := OS_16
  else
    cgsize := OS_32;

  if (cs_full_boolean_eval in aktlocalswitches) or
    (nodetype in [unequaln, ltn, lten, gtn, gten, equaln, xorn]) then
  begin
    if left.nodetype in [ordconstn, realconstn] then
      swapleftright;

    isjump := (left.expectloc = LOC_JUMP);
    if isjump then
    begin
      otl := current_procinfo.CurrTrueLabel;
      current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
      ofl := current_procinfo.CurrFalseLabel;
      current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
    end;
    secondpass(left);
    if left.location.loc in [LOC_FLAGS, LOC_JUMP] then
      location_force_reg(current_asmdata.CurrAsmList, left.location, cgsize, false);
    if isjump then
    begin
      current_procinfo.CurrTrueLabel := otl;
      current_procinfo.CurrFalseLabel := ofl;
    end
    else if left.location.loc = LOC_JUMP then
      internalerror(2003122901);

    isjump := (right.expectloc = LOC_JUMP);
    if isjump then
    begin
      otl := current_procinfo.CurrTrueLabel;
      current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
      ofl := current_procinfo.CurrFalseLabel;
      current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
    end;
    secondpass(right);
    if right.location.loc in [LOC_FLAGS, LOC_JUMP] then
      location_force_reg(current_asmdata.CurrAsmList, right.location, cgsize, false);
    if isjump then
    begin
      current_procinfo.CurrTrueLabel := otl;
      current_procinfo.CurrFalseLabel := ofl;
    end
    else if right.location.loc = LOC_JUMP then
      internalerror(200312292);

    cmpop := nodetype in [ltn, lten, gtn, gten, equaln, unequaln];

    { set result location }
    if not cmpop then
      location_reset(location, LOC_REGISTER, def_cgsize(resulttype.def))
    else
      location_reset(location, LOC_FLAGS, OS_NO);

    load_left_right(cmpop, false);

    if (left.location.loc = LOC_CONSTANT) then
      swapleftright;

    { compare the }
    case nodetype of
      ltn, lten, gtn, gten,
        equaln, unequaln:
        begin
          if (right.location.loc <> LOC_CONSTANT) then
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMPLW,
              left.location.register, right.location.register))
          else
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMPLWI,
              left.location.register, longint(right.location.value)));
          location.resflags := getresflags;
        end;
    else
      begin
        case nodetype of
          xorn:
            cgop := OP_XOR;
          orn:
            cgop := OP_OR;
          andn:
            cgop := OP_AND;
        else
          internalerror(200203247);
        end;

        if right.location.loc <> LOC_CONSTANT then
          cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, cgop, OS_INT,
            left.location.register, right.location.register,
            location.register)
        else
          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, cgop, OS_INT,
            right.location.value, left.location.register,
            location.register);
      end;
    end;
  end
  else
  begin
    // just to make sure we free the right registers
    cmpop := true;
    case nodetype of
      andn,
        orn:
        begin
          location_reset(location, LOC_JUMP, OS_NO);
          case nodetype of
            andn:
              begin
                otl := current_procinfo.CurrTrueLabel;
                current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                secondpass(left);
                maketojumpbool(current_asmdata.CurrAsmList, left, lr_load_regvars);
                cg.a_label(current_asmdata.CurrAsmList, current_procinfo.CurrTrueLabel);
                current_procinfo.CurrTrueLabel := otl;
              end;
            orn:
              begin
                ofl := current_procinfo.CurrFalseLabel;
                current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
                secondpass(left);
                maketojumpbool(current_asmdata.CurrAsmList, left, lr_load_regvars);
                cg.a_label(current_asmdata.CurrAsmList, current_procinfo.CurrFalseLabel);
                current_procinfo.CurrFalseLabel := ofl;
              end;
          else
            internalerror(200403181);
          end;
          secondpass(right);
          maketojumpbool(current_asmdata.CurrAsmList, right, lr_load_regvars);
        end;
    end;
  end;
end;

{*****************************************************************************
                                AddFloat
*****************************************************************************}

procedure tppcaddnode.second_addfloat;
var
  op: TAsmOp;
  cmpop: boolean;
begin
  pass_left_and_right;

  cmpop := false;
  case nodetype of
    addn:
      op := A_FADD;
    muln:
      op := A_FMUL;
    subn:
      op := A_FSUB;
    slashn:
      op := A_FDIV;
    ltn, lten, gtn, gten,
      equaln, unequaln:
      begin
        op := A_FCMPO;
        cmpop := true;
      end;
  else
    internalerror(200403182);
  end;

  // get the operands in the correct order, there are no special cases
  // here, everything is register-based
  if nf_swaped in flags then
    swapleftright;

  // put both operands in a register
  location_force_fpureg(current_asmdata.CurrAsmList, right.location, true);
  location_force_fpureg(current_asmdata.CurrAsmList, left.location, true);

  // initialize de result
  if not cmpop then
  begin
    location_reset(location, LOC_FPUREGISTER, def_cgsize(resulttype.def));
    if left.location.loc = LOC_FPUREGISTER then
      location.register := left.location.register
    else if right.location.loc = LOC_FPUREGISTER then
      location.register := right.location.register
    else
      location.register := cg.getfpuregister(current_asmdata.CurrAsmList, location.size);
  end
  else
  begin
    location_reset(location, LOC_FLAGS, OS_NO);
    location.resflags := getresflags;
  end;

  // emit the actual operation
  if not cmpop then
  begin
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
      location.register, left.location.register,
      right.location.register))
  end
  else
  begin
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,
      newreg(R_SPECIALREGISTER, location.resflags.cr, R_SUBNONE),
        left.location.register, right.location.register))
  end;
end;

{*****************************************************************************
                                AddSmallSet
*****************************************************************************}

procedure tppcaddnode.second_addsmallset;
var
  cgop: TOpCg;
  tmpreg: tregister;
  opdone,
    cmpop: boolean;

  astring : string;
  // ts: todo - speed up by using 32 bit compares/adds/ands here
begin
  pass_left_and_right;

  { when a setdef is passed, it has to be a smallset }
  if ((left.resulttype.def.deftype = setdef) and
    (tsetdef(left.resulttype.def).settype <> smallset)) or
    ((right.resulttype.def.deftype = setdef) and
    (tsetdef(right.resulttype.def).settype <> smallset)) then
    internalerror(200203301);

  opdone := false;
  cmpop := nodetype in [equaln, unequaln, lten, gten];

  { set result location }
  if not cmpop then
    location_reset(location, LOC_REGISTER, def_cgsize(resulttype.def))
  else
    location_reset(location, LOC_FLAGS, OS_NO);

  load_left_right(cmpop, false);

  if not (cmpop) and
    (location.register = NR_NO) then
    location.register := cg.getintregister(current_asmdata.CurrAsmList, OS_64);

  astring := 'addsmallset0 ' + inttostr(aword(1) shl aword(right.location.value)) + ' ' + inttostr(right.location.value);
  current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew(astring)));


  case nodetype of
    addn:
      begin
        if (nf_swaped in flags) and (left.nodetype = setelementn) then
          swapleftright;
        { are we adding set elements ? }
        if right.nodetype = setelementn then begin
          { no range support for smallsets! }
          if assigned(tsetelementnode(right).right) then
            internalerror(43244);
          if (right.location.loc = LOC_CONSTANT) then begin

            astring := 'addsmallset1 ' + inttostr(aword(1) shl aword(right.location.value)) + ' ' + inttostr(right.location.value);
            current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew(astring)));


            cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_64,
              aint(1) shl aint(right.location.value),
              left.location.register, location.register)
          end else
          begin
            tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_64);
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_64, 1, tmpreg);
            cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_SHL, OS_64,
              right.location.register, tmpreg);
            if left.location.loc <> LOC_CONSTANT then begin
              cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_64, tmpreg,
                left.location.register, location.register)
            end else begin
              astring := 'addsmallset2 ' + inttostr(left.location.value);
              current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew(astring)));

              cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_64,
                left.location.value, tmpreg, location.register);
            end;
          end;
          opdone := true;
        end else begin
          cgop := OP_OR;
        end;
      end;
    symdifn:
      cgop := OP_XOR;
    muln:
      cgop := OP_AND;
    subn:
      begin
        cgop := OP_AND;
        if (not (nf_swaped in flags)) then
          if (right.location.loc = LOC_CONSTANT) then
            right.location.value := not (right.location.value)
          else
            opdone := true
        else if (left.location.loc = LOC_CONSTANT) then
          left.location.value := not (left.location.value)
        else begin
          swapleftright;
          opdone := true;
        end;
        if opdone then begin
          if left.location.loc = LOC_CONSTANT then
          begin
            tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_64);
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_64,
              left.location.value, tmpreg);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC,
              location.register, tmpreg, right.location.register));
          end
          else
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC,
              location.register, left.location.register,
              right.location.register));
        end;
      end;
    equaln,
      unequaln:
      begin
        emit_compare(true);
        opdone := true;
      end;
    lten, gten:
      begin
        if (not (nf_swaped in flags) and
          (nodetype = lten)) or
          ((nf_swaped in flags) and
          (nodetype = gten)) then
          swapleftright;
        // now we have to check whether left >= right
        tmpreg := cg.getintregister(current_asmdata.CurrAsmList, OS_64);
        if left.location.loc = LOC_CONSTANT then begin
          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_64,
            not (left.location.value), right.location.register, tmpreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMPDI, tmpreg, 0));
          // the two instructions above should be folded together by
          // the peepholeoptimizer
        end else begin
          if right.location.loc = LOC_CONSTANT then begin
            cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_64,
              right.location.value, tmpreg);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC_, tmpreg,
              tmpreg, left.location.register));
          end else
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC_, tmpreg,
              right.location.register, left.location.register));
        end;
        location.resflags.cr := RS_CR0;
        location.resflags.flag := F_EQ;
        opdone := true;
      end;
  else
    internalerror(2002072701);
  end;

  if not opdone then begin
    // these are all commutative operations
    if (left.location.loc = LOC_CONSTANT) then
      swapleftright;
    if (right.location.loc = LOC_CONSTANT) then begin
      astring := 'addsmallset4 ' + inttostr(right.location.value);
      current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew(astring)));

      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, cgop, OS_64,
        right.location.value, left.location.register,
        location.register)
    end else begin
      cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, cgop, OS_64,
        right.location.register, left.location.register,
        location.register);
    end;
  end;
end;

{*****************************************************************************
                                pass_2
*****************************************************************************}

procedure tppcaddnode.pass_2;
{ is also being used for xor, and "mul", "sub, or and comparative }
{ operators                                                }
var
  cgop: topcg;
  op: tasmop;
  tmpreg: tregister;
  hl: tasmlabel;
  cmpop: boolean;

  { true, if unsigned types are compared }
  unsigned: boolean;

begin
  { to make it more readable, string and set (not smallset!) have their
    own procedures }
  case left.resulttype.def.deftype of
    orddef:
      begin
        { handling boolean expressions }
        if is_boolean(left.resulttype.def) and
          is_boolean(right.resulttype.def) then
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
        if (tsetdef(left.resulttype.def).settype <> smallset) then
          internalerror(200109041);
        second_addsmallset;
        exit;
      end;
    arraydef:
      begin
{$IFDEF SUPPORT_MMX}
        if is_mmx_able_array(left.resulttype.def) then
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
  unsigned := not (is_signed(left.resulttype.def)) or
    not (is_signed(right.resulttype.def));

  pass_left_and_right;

  { Convert flags to register first }
  { can any of these things be in the flags actually?? (JM) }

  if (left.location.loc = LOC_FLAGS) or
    (right.location.loc = LOC_FLAGS) then
    internalerror(2002072602);

  { set result location }
  if not cmpop then
    location_reset(location, LOC_REGISTER, def_cgsize(resulttype.def))
  else
    location_reset(location, LOC_FLAGS, OS_NO);

  load_left_right(cmpop, (cs_check_overflow in aktlocalswitches) and
    (nodetype in [addn, subn, muln]));

  if (location.register = NR_NO) and
    not (cmpop) then
    location.register := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);

  if not (cs_check_overflow in aktlocalswitches) or
    (cmpop) or
    (nodetype in [orn, andn, xorn]) then
  begin
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
          if (nf_swaped in flags) then
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
          emit_compare(unsigned);
        end;
    end;
  end
  else
    // overflow checking is on and we have an addn, subn or muln
  begin
    if is_signed(resulttype.def) then
    begin
      case nodetype of
        addn:
          op := A_ADDO;
        subn:
          begin
            op := A_SUBO;
            if (nf_swaped in flags) then
              swapleftright;
          end;
        muln:
          op := A_MULLDO;
      else
        internalerror(2002072601);
      end;
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op, location.register,
        left.location.register, right.location.register));
      cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resulttype.def);
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
            cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resulttype.def);
          end;
        subn:
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUB, location.register,
              left.location.register, right.location.register));
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CMPLD,
              left.location.register, location.register));
            cg.g_overflowcheck(current_asmdata.CurrAsmList, location, resulttype.def);
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

