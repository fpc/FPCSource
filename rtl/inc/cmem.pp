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
  {$ifdef netware}
  LibName = 'clib';
  {$else}
  LibName = 'c';
  {$endif}
{$else}
  LibName = 'msvcrt';
{$endif}

Function Malloc (Size : ptrint) : Pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'malloc';
Procedure Free (P : pointer); {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'free';
function ReAlloc (P : Pointer; Size : ptrint) : pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'realloc';
Function CAlloc (unitSize,UnitCount : ptrint) : pointer; {$ifdef win32}stdcall{$else}cdecl{$endif}; external LibName name 'calloc';

implementation

type
  pptrint = ^ptrint;

Function CGetMem  (Size : ptrint) : Pointer;

begin
  result:=Malloc(Size+sizeof(ptrint));
  if (result <> nil) then
    begin
      pptrint(result)^ := size;
      inc(result,sizeof(ptrint));
    end;
end;

Function CFreeMem ({$ifdef VER1_0}var{$endif} P : pointer) : ptrint;

begin
  if (p <> nil) then
    dec(p,sizeof(ptrint));
  Free(P);
  Result:=0;
end;

Function CFreeMemSize({$ifdef VER1_0}var{$endif} p:pointer;Size:ptrint):ptrint;

begin
  if (p <> nil) then
    begin
      if (size <> pptrint(p-sizeof(ptrint))^) then
        runerror(204);
    end;
  Result:=CFreeMem(P);
end;

Function CAllocMem(Size : ptrint) : Pointer;

begin
  Result:=calloc(Size+sizeof(ptrint),1);
  if (result <> nil) then
    begin
      pptrint(result)^ := size;
      inc(result,sizeof(ptrint));
    end;
end;

Function CReAllocMem (var p:pointer;Size:ptrint):Pointer;

begin
  if size=0 then
    begin
      if p<>nil then
        begin
          free(p);
          p:=nil;
	end;  
    end
  else
    begin
      inc(size,sizeof(ptrint));
      if p=nil then
        p:=calloc(Size,1)
      else
        begin
          dec(p,sizeof(ptrint));
          p:=realloc(p,size);
	end;  
      if (p <> nil) then
        begin
          pptrint(p)^ := size-sizeof(ptrint);
          inc(p,sizeof(ptrint));
        end;
    end;		
  Result:=p;
end;

Function CMemSize (p:pointer): ptrint;

begin
  Result:=pptrint(p-sizeof(ptrint))^;
end;

Function CMemAvail : ptrint;

begin
  Result:=0;
end;

Function CMaxAvail: ptrint;

begin
  Result:=0;
end;

Function CHeapSize : ptrint;

begin
  Result:=0;
end;


Const
 CMemoryManager : TMemoryManager =
    (
{$ifndef VER1_0}
      NeedLock : false;
{$endif VER1_0}
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
 Revision 1.1  2004-03-15 21:48:26  peter
   * cmem moved to rtl
   * longint replaced with ptrint in heapmanagers

 Revision 1.9  2004/03/12 13:08:08  jonas
   + added memsize() support (needed to use cmem with the compiler)

 Revision 1.8  2003/03/17 15:40:05  armin
 + LibName for netware

 Revision 1.7  2002/11/01 17:56:39  peter
   * needlock field added for 1.1

 Revision 1.6  2002/09/08 15:43:47  michael
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
