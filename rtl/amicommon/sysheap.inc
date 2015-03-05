{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level memory functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Enable this for memory allocation debugging }
{DEFINE ASYS_FPC_MEMDEBUG}

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

function SysOSAlloc(size: ptruint): pointer;
{$IFDEF ASYS_FPC_MEMDEBUG}
var values: array[0..2] of dword;
{$ENDIF}
begin
  result:=AllocPooled(ASYS_heapPool,size);
{$IFDEF ASYS_FPC_MEMDEBUG}
  values[0]:=dword(result);
  values[1]:=dword(size);
  values[2]:=DWord(Sptr-StackBottom);
  RawDoFmt('FPC_MEM_DEBUG: $%lx:=SysOSAlloc(%ld), free stack: %ld bytes'+#10,@values,pointer(1),nil);
{$ENDIF}
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
{$IFDEF ASYS_FPC_MEMDEBUG}
var values: array[0..2] of dword;
{$ENDIF}
begin
  FreePooled(ASYS_heapPool,p,size);
{$IFDEF ASYS_FPC_MEMDEBUG}
  values[0]:=dword(p);
  values[1]:=dword(size);
  values[2]:=DWord(Sptr-StackBottom);
  RawDoFmt('FPC_MEM_DEBUG: SysOSFree($%lx,%ld), free stack: %ld bytes'+#10,@values,pointer(1),nil);
{$ENDIF}
end;
