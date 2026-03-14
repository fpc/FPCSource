{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the Generic CPU
    This file is used by PPUDump program from utils subdirectory.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUBase;

{$i fpcdefs.inc}

Interface

uses
  cgbase;

const
  R_SUBWHOLE = R_SUBD;

  OS_ADDR = OS_32;
  OS_INT = OS_32;
  OS_SINT = OS_S32;
  OS_VECTOR = OS_M128;

  first_int_imreg = 1;
  first_fpu_imreg = 0;
  first_mm_imreg = 0;
  max_operands = 2;
  maxfpuregs = 0;

  NR_NO = tregister($00000000);
  NR_DEFAULTFLAGS = NR_NO;
  NR_STACK_POINTER_REG = NR_NO;
  NR_FRAME_POINTER_REG = NR_NO;
  NR_FUNCTION_RETURN_REG = NR_NO;
  NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
  NR_RETURN_ADDRESS_REG = NR_NO;

  std_param_align = 4;

type
  TAsmOp = (A_NOP);
  TAsmCond=(C_None);
  TResFlags = (F_NotPossible);
  tregisterindex=0..0;

const
  regnumber_table : array[tregisterindex] of tregister = (
    tregister($00000000)
  );

function std_regname(r:Tregister):string;
function std_regnum_search(const s:string):Tregister;
function condition_in(const Subset, c: TAsmCond): Boolean;
function inverse_cond(const c: TAsmCond): TAsmCond;
function reg_cgsize(const reg: tregister): tcgsize;
function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
function eh_return_data_regno(nr: longint): longint;
function dwarf_reg(r:tregister):shortint;

Implementation

function std_regname(r:Tregister):string;
begin
  result:='';
end;

function std_regnum_search(const s:string):Tregister;
begin
  result:=NR_NO;
end;

function condition_in(const Subset, c: TAsmCond): Boolean;
begin
  result:=false;
end;

function inverse_cond(const c: TAsmCond): TAsmCond;
begin
  result:=c;
end;

function reg_cgsize(const reg: tregister): tcgsize;
begin
  result:=OS_NO;
end;

function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
begin
  result:=R_SUBNONE;
end;

function eh_return_data_regno(nr: longint): longint;
begin
  result:=-1;
end;

function dwarf_reg(r:tregister):shortint;
begin
  result:=-1;
end;

end.
