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

  first_int_imreg = 1;
  first_fpu_imreg = 0;
  first_mm_imreg = 0;
  max_operands = 2;

  NR_NO = tregister($00000000);
  NR_DEFAULTFLAGS = NR_NO;
  NR_STACK_POINTER_REG = NR_NO;

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
function condition_in(const Subset, c: TAsmCond): Boolean;
function inverse_cond(const c: TAsmCond): TAsmCond;
function reg_cgsize(const reg: tregister): tcgsize;

Implementation

function std_regname(r:Tregister):string;
begin
  result:='';
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

end.
