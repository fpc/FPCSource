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

Function CFreeMem (P : pointer) : Longint;

begin
  Free(P);
  Result:=0;
end;

Function CFreeMemSize(p:pointer;Size:Longint):Longint;

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
 Revision 1.2  2002-06-13 04:54:47  michael
 + Fixed parameter type mismatch

 Revision 1.1  2002/01/29 17:54:59  peter
   * splitted to base and extra

 Revision 1.5  2001/10/23 12:14:36  jonas
   * fixed CReAllocMem (web bug 1637)

 Revision 1.4  2001/06/07 16:34:41  jonas
   * added ifdef fpc round @ for procvars

 Revision 1.3  2001/06/07 16:14:48  marco
  * Fixed @ procvar

    Revision 1.2  2000/07/13 11:33:10  michael
     + removed logs
 
}
