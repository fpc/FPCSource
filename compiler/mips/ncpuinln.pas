{
    Copyright (c) 1998-2009 by Florian Klaempfl and David Zhang

    Generate MIPSEL inline nodes

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
unit ncpuinln;

{$i fpcdefs.inc}

interface

uses
  node, ninl, ncginl;

type
  tMIPSELinlinenode = class(tcgInlineNode)
    function first_abs_real: tnode; override;
    function first_sqr_real: tnode; override;
    function first_sqrt_real: tnode; override;
    function first_gteCommand: tnode;
    procedure second_abs_real; override;
    procedure second_sqr_real; override;
    procedure second_sqrt_real; override;
    procedure second_get_frame; override;
    procedure second_gteCommand;
    procedure pass_generate_code_cpu;override;
    function pass_typecheck_cpu:tnode;override;
    function first_cpu : tnode;override;
  private
    procedure load_fpu_location;
  end;


implementation

uses
  systems, compinnr,
  globtype,globals,
  cutils, verbose,
  symconst, symdef, defutil,
  aasmtai, aasmcpu, aasmdata,
  cgbase, pass_2, htypechk,
  cpubase, paramgr,
  nbas, ncon, ncal, ncnv, nld,
  hlcgobj, ncgutil, cgobj, cgutils;

{*****************************************************************************
                              tMIPSELinlinenode
*****************************************************************************}

procedure tMIPSELinlinenode.load_fpu_location;
begin
  secondpass(left);
  hlcg.location_force_fpureg(current_asmdata.CurrAsmList, left.location, left.resultdef, True);
  location_copy(location, left.location);
  if left.location.loc = LOC_CFPUREGISTER then
  begin
    location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, location.size);
    location.loc      := LOC_FPUREGISTER;
  end;
end;


function tMIPSELinlinenode.first_abs_real: tnode;
begin
  if not (cs_fp_emulation in current_settings.moduleswitches) then
    begin
      expectloc      := LOC_FPUREGISTER;
      first_abs_real := nil;
    end
  else
    result:=inherited;
end;


function tMIPSELinlinenode.first_sqr_real: tnode;
begin
  if not (cs_fp_emulation in current_settings.moduleswitches) then
    begin
      expectloc      := LOC_FPUREGISTER;
      first_sqr_real := nil;
    end
  else
    result:=inherited;
end;


function tMIPSELinlinenode.first_sqrt_real: tnode;
begin
  if not (cs_fp_emulation in current_settings.moduleswitches) then
    begin
      expectloc    := LOC_FPUREGISTER;
      first_sqrt_real := nil;
    end
  else
    result:=inherited;
end;


procedure tMIPSELinlinenode.second_abs_real;
begin
  load_fpu_location;
  case tfloatdef(left.resultdef).floattype of
    s32real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_ABS_s, location.Register, left.location.Register));
    s64real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_ABS_d, location.Register, left.location.Register));
    else
      internalerror(2004100305);
  end;
end;


procedure tMIPSELinlinenode.second_sqr_real;
begin
  load_fpu_location;
  case tfloatdef(left.resultdef).floattype of
    s32real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MUL_s, location.Register, left.location.Register, left.location.Register));
    s64real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MUL_d, location.Register, left.location.Register, left.location.Register));
    else
      internalerror(200410032);
  end;
end;


procedure tMIPSELinlinenode.second_sqrt_real;
begin
  load_fpu_location;
  case tfloatdef(left.resultdef).floattype of
    s32real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SQRT_s, location.Register, left.location.Register));
    s64real:
      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SQRT_d, location.Register, left.location.Register));
    else
      internalerror(200410033);
  end;
end;


procedure tMIPSELinlinenode.second_get_frame;
begin
  location_reset(location,LOC_CREGISTER,OS_ADDR);
  location.register:=NR_FRAME_POINTER_REG;
end;


function tMIPSELinlinenode.first_gteCommand: tnode;
begin
  result:= nil;
end;


procedure tMIPSELinlinenode.second_gteCommand;
begin
  
  secondpass(left);

  current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
  current_asmdata.CurrAsmList.concat(taicpu.op_const(A_COP2, left.location.value));

end;

procedure tMIPSELinlinenode.pass_generate_code_cpu;
begin
  if inlinenumber = in_gtecommand_x then second_gteCommand;
end;


function tMIPSELinlinenode.pass_typecheck_cpu : tnode;
begin
  Result:=nil;
  case inlinenumber of
    in_gtecommand_x:
      begin
        set_varstate(left,vs_read,[vsf_must_be_valid]);
        if not is_integer(left.resultdef) then
          CGMessage1(type_e_integer_expr_expected,left.resultdef.typename);
        resultdef:=voidtype;
      end;
    else
      Result:=inherited pass_typecheck_cpu;
  end;
end;


function tMIPSELinlinenode.first_cpu : tnode;
  begin
    Result:=nil;
    case inlinenumber of
      in_gtecommand_x: 
      begin
        result:= first_gtecommand;
      end;
      else
        Result:=inherited first_cpu;
    end;
  end;


begin
  cInlineNode := tMIPSELinlinenode;
end.
