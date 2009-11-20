{
    Copyright (c) 1998-2002 by Florian Klaempfl and David Zhang

    Generate MIPSEL assembler for type converting nodes

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

 ****************************************************************************}
unit ncpucnv;

{$i fpcdefs.inc}

interface

uses
  node, ncnv, ncgcnv, defcmp;

type
  tMIPSELtypeconvnode = class(TCgTypeConvNode)
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
    procedure second_real_to_real; override;
    { procedure second_cord_to_pointer;override; }
    { procedure second_proc_to_procvar;override; }
    { procedure second_bool_to_int;override; }
    procedure second_int_to_bool; override;
    { procedure second_load_smallset;override;  }
    { procedure second_ansistring_to_pchar;override; }
    { procedure second_pchar_to_string;override; }
    { procedure second_class_to_intf;override; }
    { procedure second_char_to_char;override; }
  end;

implementation

uses
  verbose, globtype, globals, systems,
  symconst, symdef, aasmbase, aasmtai, aasmdata,
  defutil,
  cgbase, cgutils, pass_1, pass_2, procinfo,
  ncon, ncal,
  ncgutil,
  cpubase, aasmcpu,
  tgobj, cgobj;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

function tMIPSELtypeconvnode.first_int_to_real: tnode;
var
  fname: string[19];
begin
  { converting a 64bit integer to a float requires a helper }
  if is_64bitint(left.resultdef) or
    is_currency(left.resultdef) then
  begin
            { hack to avoid double division by 10000, as it's
              already done by resulttypepass.resulttype_int_to_real }
    if is_currency(left.resultdef) then
      left.resultdef := s64inttype;
    if is_signed(left.resultdef) then
      fname := 'fpc_int64_to_double'
    else
      fname := 'fpc_qword_to_double';
    Result := ccallnode.createintern(fname, ccallparanode.Create(
      left, nil));
    left := nil;
    firstpass(Result);
    exit;
  end
  else
    { other integers are supposed to be 32 bit }
  begin
    if is_signed(left.resultdef) then
      inserttypeconv(left, s32inttype)
    else
      inserttypeconv(left, u32inttype);
    firstpass(left);
  end;
  Result := nil;
  expectloc := LOC_FPUREGISTER;
end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

procedure tMIPSELtypeconvnode.second_int_to_real;

  procedure loadsigned;
  begin
    location_force_mem(current_asmdata.CurrAsmList, left.location);
    location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, location.size);
    { Load memory in fpu register }
    cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList, OS_F32, OS_F32, left.location.reference, location.Register);
    tg.ungetiftemp(current_asmdata.CurrAsmList, left.location.reference);
    { Convert value in fpu register from integer to float }
    case tfloatdef(resultdef).floattype of
      s32real:
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVT_S_W, location.Register, location.Register));
      s64real:
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVT_D_W, location.Register, location.Register));
      else
        internalerror(200408011);
    end;
  end;

var
  href:      treference;
  hregister: tregister;
  l1, l2:    tasmlabel;

begin
  location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
  if is_signed(left.resultdef) then
    loadsigned
  else
  begin
    current_asmdata.getdatalabel(l1);
    current_asmdata.getjumplabel(l2);
    reference_reset_symbol(href, l1, 0, sizeof(aint));
    hregister := cg.getintregister(current_asmdata.CurrAsmList, OS_32);
    cg.a_load_loc_reg(current_asmdata.CurrAsmList, OS_32, left.location, hregister);

    loadsigned;

    current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg_sym(A_BGE, hregister, NR_R0, l2));

    case tfloatdef(resultdef).floattype of
      { converting dword to s64real first and cut off at the end avoids precision loss }
      s32real,
      s64real:
      begin
        hregister := cg.getfpuregister(current_asmdata.CurrAsmList, OS_F64);
        current_asmdata.asmlists[al_typedconsts].concat(tai_align.Create(const_align(8)));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));

        { I got this constant from a test program (FK) }
        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($0000f041));

        cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList, OS_F64, OS_F64, href, hregister);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD_D, location.Register, hregister, location.Register));
        cg.a_label(current_asmdata.CurrAsmList, l2);

        { cut off if we should convert to single }
        if tfloatdef(resultdef).floattype = s32real then
        begin
          hregister := location.Register;
          location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, location.size);
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVT_S_D, location.Register, hregister));
        end;
      end;
      else
        internalerror(200410031);
    end;
  end;
end;


procedure tMIPSELtypeconvnode.second_real_to_real;
const
  conv_op: array[tfloattype, tfloattype] of tasmop = (
    {    from:   s32      s64         s80     c64     cur    f128 }
    { s32 }  (A_MOV_S,   A_CVT_S_D, A_NONE, A_NONE, A_NONE, A_NONE),
    { s64 }  (A_CVT_D_S, A_MOV_D,   A_NONE, A_NONE, A_NONE, A_NONE),
    { s80 }  (A_NONE,    A_NONE,    A_NONE, A_NONE, A_NONE, A_NONE),
    { c64 }  (A_NONE,    A_NONE,    A_NONE, A_NONE, A_NONE, A_NONE),
    { cur }  (A_NONE,    A_NONE,    A_NONE, A_NONE, A_NONE, A_NONE),
    { f128 } (A_NONE,    A_NONE,    A_NONE, A_NONE, A_NONE, A_NONE)
    );
var
  op: tasmop;
begin
  location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
  location_force_fpureg(current_asmdata.CurrAsmList, left.location, False);
  { Convert value in fpu register from integer to float }
  op := conv_op[tfloatdef(resultdef).floattype, tfloatdef(left.resultdef).floattype];
  if op = A_NONE then
    internalerror(200401121);
  location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, location.size);
  current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, location.Register, left.location.Register));
end;


procedure tMIPSELtypeconvnode.second_int_to_bool;
var
  hreg1, hreg2: tregister;
  opsize: tcgsize;
  hlabel, oldtruelabel, oldfalselabel: tasmlabel;
begin
  oldtruelabel  := current_procinfo.CurrTrueLabel;
  oldfalselabel := current_procinfo.CurrFalseLabel;
  current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
  current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
  secondpass(left);
  if codegenerror then
    exit;

  { byte(boolean) or word(wordbool) or longint(longbool) must }
  { be accepted for var parameters                            }
  if (nf_explicit in flags) and
    (left.resultdef.size = resultdef.size) and
    (left.location.loc in [LOC_REFERENCE, LOC_CREFERENCE, LOC_CREGISTER]) then
  begin
    location_copy(location, left.location);
    current_procinfo.CurrTrueLabel  := oldtruelabel;
    current_procinfo.CurrFalseLabel := oldfalselabel;
    exit;
  end;
  location_reset(location, LOC_REGISTER, def_cgsize(resultdef));
  opsize := def_cgsize(left.resultdef);
  case left.location.loc of
    LOC_CREFERENCE, LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER:
    begin
      if left.location.loc in [LOC_CREFERENCE, LOC_REFERENCE] then
      begin
        hreg2 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
        cg.a_load_ref_reg(current_asmdata.CurrAsmList, opsize, opsize, left.location.reference, hreg2);
      end
      else
        hreg2 := left.location.Register;
{$ifndef cpu64bit}
      if left.location.size in [OS_64, OS_S64] then
      begin
        hreg1 := cg.getintregister(current_asmdata.CurrAsmList, OS_32);
        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_32, hreg2, tregister(succ(longint(hreg2))), hreg1);
        hreg2  := hreg1;
        opsize := OS_32;
      end;
{$endif cpu64bit}
      hreg1 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SNE, hreg1, hreg2, NR_R0));
    end;
    LOC_JUMP:
    begin
      hreg1 := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
      current_asmdata.getjumplabel(hlabel);
      cg.a_label(current_asmdata.CurrAsmList, current_procinfo.CurrTrueLabel);
      cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 1, hreg1);
      cg.a_jmp_always(current_asmdata.CurrAsmList, hlabel);
      cg.a_label(current_asmdata.CurrAsmList, current_procinfo.CurrFalseLabel);
      cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 0, hreg1);
      cg.a_label(current_asmdata.CurrAsmList, hlabel);
    end;
    else
      internalerror(10062);
  end;
  location.Register := hreg1;

  if location.size in [OS_64, OS_S64] then
    internalerror(200408241);

  current_procinfo.CurrTrueLabel  := oldtruelabel;
  current_procinfo.CurrFalseLabel := oldfalselabel;
end;


begin
  ctypeconvnode := tMIPSELtypeconvnode;
end.
