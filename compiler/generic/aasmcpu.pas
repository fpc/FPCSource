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

Unit AAsmCpu;

{$i fpcdefs.inc}

Interface

uses
  aasmsym,cgbase,cgutils;

const
  { "mov reg,reg" source operand number }
  O_MOV_SOURCE = 1;
  { "mov reg,reg" source operand number }
  O_MOV_DEST   = 0;

type
  taicpu = class(tai_cpu_abstract_sym)
  end;

function spilling_create_load(const ref:treference;r:tregister):Taicpu;
function spilling_create_store(r:tregister; const ref:treference):Taicpu;

Implementation

function spilling_create_load(const ref:treference;r:tregister):Taicpu;
begin
  result:=nil;
end;

function spilling_create_store(r:tregister; const ref:treference):Taicpu;
begin
  result:=nil;
end;

end.
