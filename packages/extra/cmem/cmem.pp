{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by Michael Van Canneyt, member of the
    Free Pascal development team

    Implements a memory manager that uses the C memory management.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit cmem;

{$mode objfpc}

interface

Const
{$ifndef win32}
  LibName = 'c';
{$else}
  LibName = 'msvcrt';
{$endif}

Function Malloc (Size : Longint) : Pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'malloc';
Procedure Free (P : pointer); {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'free';
function ReAlloc (P : Pointer; Size : longint) : pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'realloc';
Function CAlloc (unitSize,UnitCount : Longint) : pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'calloc';

implementation

Function CGetMem  (Size : Longint) : Pointer;

begin
  result:=Malloc(Size);
end;

Function CFreeMem ({$ifdef VER1_0}var{$endif} P : pointer) : Longint;

begin
  Free(P);
  Result:=0;
end;

Function CFreeMemSize({$ifdef VER1_0}var{$endif} p:pointer;Size:Longint):Longint;

begin
  Result:=CFreeMem(P);
end;

Function CAllocMem(Size : Longint) : Pointer;

begin
  Result:=calloc(Size,1);
end;

Function CReAllocMem (var p:pointer;Size:longint):Pointer;

begin
  p := realloc(p,size);
  Result:=p;
end;

Function CMemSize (p:pointer): Longint;

begin
  Result:=0;
end;

Function CMemAvail : Longint;

begin
  Result:=0;
end;

Function CMaxAvail: Longint;

begin
  Result:=0;
end;

Function CHeapSize : Longint;

begin
  Result:=0;
end;


Const
 CMemoryManager : TMemoryManager =
    (
      GetMem : {$ifdef fpc}@{$endif}CGetmem;
      FreeMem : {$ifdef fpc}@{$endif}CFreeMem;
      FreememSize : {$ifdef fpc}@{$endif}CFreememSize;
      AllocMem : {$ifdef fpc}@{$endif}CAllocMem;
      ReallocMem : {$ifdef fpc}@{$endif}CReAllocMem;
      MemSize : {$ifdef fpc}@{$endif}CMemSize;
      MemAvail : {$ifdef fpc}@{$endif fpc}CMemAvail;
      MaxAvail : {$ifdef fpc}@{$endif}MaxAvail;
      HeapSize : {$ifdef fpc}@{$endif}CHeapSize;
    );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);

Finalization
  SetMemoryManager (OldMemoryManager);
end.

{
 $Log$
 Revision 1.6  2002-09-08 15:43:47  michael
 + Fixed calling conventions

 Revision 1.5  2002/09/07 15:42:54  peter
   * old logs removed and tabs fixed

 Revision 1.4  2002/07/01 16:24:04  peter
   * updates for 1.0 compiler

 Revision 1.3  2002/06/13 05:01:44  michael
 + Added windows msvcrt support

 Revision 1.2  2002/06/13 04:54:47  michael
 + Fixed parameter type mismatch

 Revision 1.1  2002/01/29 17:54:59  peter
   * splitted to base and extra

}
