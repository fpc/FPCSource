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

interface

Const
{$ifndef win32}
  {$ifdef netware}
  LibName = 'clib';
  {$else}
    {$ifdef netwlibc}
    LibName = 'libc';
    {$else}
      {$ifdef macos}
      LibName = 'StdCLib';
      {$else}
      LibName = 'c';
      {$endif macos}
    {$endif netwlibc}
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
  CGetMem:=Malloc(Size+sizeof(ptrint));
  if (CGetMem <> nil) then
    begin
      pptrint(CGetMem)^ := size;
      inc(CGetMem,sizeof(ptrint));
    end;
end;

Function CFreeMem (P : pointer) : ptrint;

begin
  if (p <> nil) then
    dec(p,sizeof(ptrint));
  Free(P);
  CFreeMem:=0;
end;

Function CFreeMemSize(p:pointer;Size:ptrint):ptrint;

begin
  if (p <> nil) then
    begin
      if (size <> pptrint(p-sizeof(ptrint))^) then
        runerror(204);
    end;
  CFreeMemSize:=CFreeMem(P);
end;

Function CAllocMem(Size : ptrint) : Pointer;

begin
  CAllocMem:=calloc(Size+sizeof(ptrint),1);
  if (CAllocMem <> nil) then
    begin
      pptrint(CAllocMem)^ := size;
      inc(CAllocMem,sizeof(ptrint));
    end;
end;

Function CReAllocMem (var p:pointer;Size:ptrint):Pointer;

begin
  if size=0 then
    begin
      if p<>nil then
        begin
          dec(p,sizeof(ptrint));
          free(p);
          p:=nil;
        end;
    end
  else
    begin
      inc(size,sizeof(ptrint));
      if p=nil then
        p:=malloc(Size)
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
  CReAllocMem:=p;
end;

Function CMemSize (p:pointer): ptrint;

begin
  CMemSize:=pptrint(p-sizeof(ptrint))^;
end;

Function CMemAvail : ptrint;

begin
  CMemAvail:=0;
end;

Function CMaxAvail: ptrint;

begin
  CMaxAvail:=0;
end;

Function CHeapSize : ptrint;

begin
  CHeapSize:=0;
end;


Const
 CMemoryManager : TMemoryManager =
    (
      NeedLock : false;
      GetMem : @CGetmem;
      FreeMem : @CFreeMem;
      FreememSize : @CFreememSize;
      AllocMem : @CAllocMem;
      ReallocMem : @CReAllocMem;
      MemSize : @CMemSize;
      MemAvail : @CMemAvail;
      MaxAvail : @CMaxAvail;
      HeapSize : @CHeapSize;
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
 Revision 1.8  2004-09-19 08:16:03  olle
   * reverted to $ifdef style, so 1.0.x can eat it.

 Revision 1.7  2004/09/18 08:40:26  olle
   + added support for macos

 Revision 1.6  2004/09/15 20:37:42  armin
   * add support for netware libc

 Revision 1.5  2004/05/05 13:00:43  jonas
   * fixed reallocmem (bug noted by Vincent Snijders)

 Revision 1.4  2004/03/23 22:35:20  peter
   * dec ptr before free in reallocmem

 Revision 1.3  2004/03/17 12:50:53  michael
   * Fixed: Macavail -> CMaxAvail (from Marc Weustinc)

 Revision 1.2  2004/03/16 15:25:16  peter
   * adaption to compile with 1.0.x in new rtl

 Revision 1.1  2004/03/15 21:48:26  peter
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
