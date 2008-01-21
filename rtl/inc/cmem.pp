{
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

{$if defined(win32)}
  LibName = 'msvcrt';
{$elseif defined(win64)}
  LibName = 'msvcrt';
{$elseif defined(wince)}
  LibName = 'coredll';
{$elseif defined(netware)}
  LibName = 'clib';
{$elseif defined(netwlibc)}
  LibName = 'libc';
{$elseif defined(macos)}
  LibName = 'StdCLib';
{$elseif defined(beos)}
  LibName = 'root';
{$else}
  LibName = 'c';
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
  if size<=0 then
    exit;
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

function CGetHeapStatus:THeapStatus;

var res: THeapStatus;

begin
  fillchar(res,sizeof(res),0);
  CGetHeapStatus:=res;
end;

function CGetFPCHeapStatus:TFPCHeapStatus;

begin
  fillchar(CGetFPCHeapStatus,sizeof(CGetFPCHeapStatus),0);
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
      GetHeapStatus : @CGetHeapStatus;
      GetFPCHeapStatus: @CGetFPCHeapStatus;	
    );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);

Finalization
  SetMemoryManager (OldMemoryManager);
end.
