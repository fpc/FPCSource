{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for type converting nodes

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
unit nppccnv;

{$I fpcdefs.inc}

interface

uses
  node, ncnv, ncgcnv, ngppccnv;

type
  tppctypeconvnode = class(tgenppctypeconvnode)
  protected
    { procedure second_int_to_int;override; }
    { procedure second_string_to_string;override; }
    { procedure second_cstring_to_pchar;override; }
    { procedure second_string_to_chararray;override; }
    { procedure second_array_to_pointer;override; }
    function first_int_to_real: tnode; override;
    { procedure second_pointer_to_array;override; }
    { procedure second_chararray_to_string;override; }
    { procedure second_char_to_string;override; }
    procedure second_int_to_real; override;
    { procedure second_real_to_real; override;}
    { procedure second_cord_to_pointer;override; }
    { procedure second_proc_to_procvar;override; }
    { procedure second_bool_to_int;override; }
    { procedure second_int_to_bool; override; }
    { procedure second_load_smallset;override;  }
    { procedure second_ansistring_to_pchar;override; }
    { procedure second_pchar_to_string;override; }
    { procedure second_class_to_intf;override; }
    { procedure second_char_to_char;override; }
  end;

implementation

uses
  verbose, globtype, globals, systems,
  symconst, symdef, aasmbase, aasmtai,aasmdata,
  defutil,
  cgbase, cgutils, pass_1, pass_2,
  ncon, ncal,procinfo,
  ncgutil,
  cpubase, aasmcpu,
  rgobj, tgobj, cgobj;

{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

function tppctypeconvnode.first_int_to_real: tnode;
begin
  if (is_currency(left.resultdef)) then begin
    // hack to avoid double division by 10000, as it's
    // already done by typecheckpass.resultdef_int_to_real
    left.resultdef := s64inttype;
  end else begin
    // everything that is less than 64 bits is converted to a 64 bit signed
    // integer - because the int_to_real conversion is faster for 64 bit
    // signed ints compared to 64 bit unsigned ints.
    if (not (torddef(left.resultdef).ordtype in [s64bit, u64bit, scurrency])) then begin
      inserttypeconv(left, s64inttype);
    end;
  end;
  firstpass(left);
  result := nil;
  expectloc := LOC_FPUREGISTER;
end;

{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

procedure tppctypeconvnode.second_int_to_real;
const
  convconst : double = $100000000;
var
  tempconst : trealconstnode;
  disp, disp2: treference;
  // temp registers for converting signed ints
  valuereg, leftreg,
  // additional temp registers for converting unsigned 64 bit ints
  tmpintreg1, tmpintreg2, tmpfpureg, tmpfpuconst : tregister;
  size: tcgsize;
  signed: boolean;
begin

  location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));

  { the code here comes from the PowerPC Compiler Writer's Guide }
  { * longint to double (works for all rounding modes) }
  { std   R3,disp(R1) # store doubleword }
  { lfd   FR1,disp(R1) # load float double }
  { fcfid FR1,FR1 # convert to floating-point integer  }

  { * unsigned 64 bit int to fp value (works for all rounding modes) }
  { rldicl rT1,rS,32,32 # isolate high half }
  { rldicl rT2,rS,0,32 # isolate low half }
  { std rT1,disp(R1) # store high half }
  { std rT2,disp+8(R1) # store low half }
  { lfd frT1,disp(R1) # load high half }
  { lfd frD,disp+8(R1) # load low half }
  { fcfid frT1,frT1 # convert each half to floating }
  { fcfid frD,frD # point integer (no round) }
  { fmadd frD,frC,frT1,frD # (2^32)*high + low }
  { # (only add can round) }
  tg.Gettemp(current_asmdata.CurrAsmList, 8, tt_normal, disp);

  { do the signed case for everything but 64 bit unsigned integers }
  signed := (left.location.size <> OS_64);

  { we need a certain constant for the conversion of unsigned 64 bit integers,
    so create them here. Additonally another temporary location is neeted }
  if (not signed) then begin
    // allocate temp for constant value used for unsigned 64 bit ints
    tempconst :=
      crealconstnode.create(convconst, pbestrealtype^);
    typecheckpass(tempconst);
    firstpass(tempconst);
    secondpass(tempconst);
    if (tempconst.location.loc <> LOC_CREFERENCE) then
      internalerror(200110011);

    // allocate second temp memory
    tg.Gettemp(current_asmdata.CurrAsmList, 8, tt_normal, disp2);
  end;

  if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE]) then
    location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,false);
  case left.location.loc of
    // the conversion algorithm does not modify the input register, so it can
    // be used for both LOC_REGISTER and LOC_CREGISTER
    LOC_REGISTER, LOC_CREGISTER:
      begin
        leftreg := left.location.register;
        valuereg := leftreg;
      end;
    LOC_REFERENCE, LOC_CREFERENCE:
      begin
        leftreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
        valuereg := leftreg;
        if signed then
          size := OS_S64
        else
          size := OS_64;
        cg.a_load_ref_reg(current_asmdata.CurrAsmList, def_cgsize(left.resultdef),
          size, left.location.reference, leftreg);
      end
  else
    internalerror(200110012);
  end;

  if (signed) then begin
    // std rS, disp(r1)
    cg.a_load_reg_ref(current_asmdata.CurrAsmList, OS_S64, OS_S64, valuereg, disp);
    // lfd frD, disp(r1)
    location.register := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
    cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64, OS_F64, disp, location.register);
    // fcfid frD, frD
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCFID, location.register,
      location.register));
  end else begin
    { ts:todo use TOC for this constant or at least schedule better }
    // lfd frC, const
    tmpfpuconst := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
    cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64,OS_F64,tempconst.location.reference,
      tmpfpuconst);
    tempconst.free;

    tmpintreg1 := cg.getintregister(current_asmdata.CurrAsmList, OS_64);
    // rldicl rT1, rS, 32, 32
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const(A_RLDICL, tmpintreg1, valuereg, 32, 32));
    // rldicl rT2, rS, 0, 32
    tmpintreg2 := cg.getintregister(current_asmdata.CurrAsmList, OS_64);
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const(A_RLDICL, tmpintreg2, valuereg, 0, 32));

    // std rT1, disp(r1)
    cg.a_load_reg_ref(current_asmdata.CurrAsmList, OS_S64, OS_S64, tmpintreg1, disp);
    // std rT2, disp2(r1)
    cg.a_load_reg_ref(current_asmdata.CurrAsmList, OS_S64, OS_S64, tmpintreg2, disp2);

    // lfd frT1, disp(R1)
    tmpfpureg := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
    cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64, OS_F64, disp, tmpfpureg);
    // lfd frD, disp+8(R1)
    location.register := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
    cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64, OS_F64, disp2, location.register);

    // fcfid frT1, frT1
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCFID, tmpfpureg,
      tmpfpureg));
    // fcfid frD, frD
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FCFID, location.register,
      location.register));
    // fmadd frD,frC,frT1,frD # (2^32)*high + low }
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_reg(A_FMADD, location.register, tmpfpuconst,
      tmpfpureg, location.register));

    // free used temps
    tg.ungetiftemp(current_asmdata.CurrAsmList, disp2);
  end;
  // free reference
  tg.ungetiftemp(current_asmdata.CurrAsmList, disp);

  // make sure the precision is correct
  if (tfloatdef(resultdef).floattype = s32real) then
    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FRSP,location.register,
      location.register));
end;

begin
  ctypeconvnode := tppctypeconvnode;
end.

