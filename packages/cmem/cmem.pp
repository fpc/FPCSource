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

Function Malloc (Size : Longint) : Pointer;cdecl; external 'c' name 'malloc';
Procedure Free (P : pointer); cdecl; external 'c' name 'free';
Procedure FreeMem (P : Pointer); cdecl; external 'c' name 'free';
function ReAlloc (P : Pointer; Size : longint) : pointer; cdecl; external 'c' name 'realloc';
Function CAlloc (unitSize,UnitCount : Longint) : pointer;cdecl;external 'c' name 'calloc';

implementation

Function CGetMem  (Size : Longint) : Pointer;

begin
  result:=Malloc(Size);
end;

Function CFreeMem (Var P : pointer) : Longint;

begin
  Free(P);
  Result:=0;
end;

Function CFreeMemSize(var p:pointer;Size:Longint):Longint;

begin
  Result:=CFreeMem(P);
end;

Function CAllocMem(Size : Longint) : Pointer;

begin
  Result:=calloc(Size,1); 
end;

Function CReAllocMem (var p:pointer;Size:longint):Pointer;

begin
  Result:=realloc(p,size);
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
      GetMem : CGetmem;
      FreeMem : CFreeMem;
      FreememSize : CFreememSize;
      AllocMem : CAllocMem;
      ReallocMem : CReAllocMem;
      MemSize : CMemSize;
      MemAvail : CMemAvail;
      MaxAvail : MaxAvail;
      HeapSize : CHeapSize;
    );
  
Var 
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);
  
Finalization
  SetMemoryManager (OldMemoryManager);
end.