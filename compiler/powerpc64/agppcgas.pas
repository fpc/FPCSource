{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asm for the PowerPC64. Heavily based on the one
    from the PowerPC architecture.

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
{ This unit implements the GNU Assembler writer for the PowerPC
}

unit agppcgas;

{$I fpcdefs.inc}

interface

uses
  aasmtai,aasmdata,
  aggas,
  cpubase;

type
  TPPCGNUAssembler = class(TGNUassembler)
  public
    constructor create(smart: boolean); override;
    procedure WriteExtraHeader; override;
  end;


implementation

uses
  cutils, globals, verbose,
  cgbase, cgutils, systems,
  assemble, globtype, fmodule,
  itcpugas, finput,
  aasmcpu, agppcutl;


{****************************************************************************}
{                         GNU PPC Assembler writer                           }
{****************************************************************************}

constructor TPPCGNUAssembler.create(smart: boolean);
begin
  inherited create(smart);
  InstrWriter := TPPCInstrWriter.create(self);
end;


procedure TPPCGNUAssembler.WriteExtraHeader;
var
  i: longint;
begin
  for i := 0 to 31 do
    AsmWriteln(#9'.set'#9'r' + tostr(i) + ',' + tostr(i));
  for i := 0 to 31 do
    AsmWriteln(#9'.set'#9'f' + tostr(i) + ',' + tostr(i));
end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

const
  as_ppc_gas_info: tasminfo =
  (
    id: as_gas;

    idtxt: 'AS';
    asmbin: 'as';
    asmcmd: '-a64 -o $OBJ $ASM';
    supported_target: system_any;
    flags: [af_allowdirect, af_needar, af_smartlink_sections];
    labelprefix: '.L';
    comment: '# ';
    );


begin
  RegisterAssembler(as_ppc_gas_info, TPPCGNUAssembler);
end.
