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
{$IFNDEF FPC_DOTTEDUNITS}
unit cmem;
{$ENDIF FPC_DOTTEDUNITS}

interface

Const

{$if defined(go32v2) or defined(wii)}
  {$define USE_STATIC_LIBC}
{$endif}

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

{$ifdef USE_STATIC_LIBC}
  {$linklib c}
Function malloc (Size : ptruint) : Pointer;cdecl; external;
Procedure free (P : pointer); cdecl; external;
function realloc (P : Pointer; Size : ptruint) : pointer;cdecl; external;
Function calloc (unitSize,UnitCount : ptruint) : pointer;cdecl; external;
{$else not USE_STATIC_LIBC}
Function Malloc (Size : ptruint) : Pointer; cdecl; external LibName name 'malloc';
Procedure Free (P : pointer); cdecl; external LibName name 'free';
function ReAlloc (P : Pointer; Size : ptruint) : pointer; cdecl; external LibName name 'realloc';
Function CAlloc (unitSize,UnitCount : ptruint) : pointer; cdecl; external LibName name 'calloc';
{$endif not USE_STATIC_LIBC}

implementation

{$macro on}

const
  { poor man's max function for constants:
    headersize = max(sizeof(ptruint),FPC_STACKALIGNMENT); }    
  headersize = ord(sizeof(ptruint)>FPC_STACKALIGNMENT)*sizeof(ptruint)+ord(sizeof(ptruint)<=FPC_STACKALIGNMENT)*FPC_STACKALIGNMENT;

{$macro off}

Function CGetMem  (Size : ptruint) : Pointer;

begin
  CGetMem:=Malloc(Size+headersize);
  if (CGetMem <> nil) then
    begin
      Pptruint(CGetMem)^ := size;
      inc(CGetMem,headersize);
    end;
end;

Function CFreeMem (P : pointer) : ptruint;

begin
  if (p <> nil) then
    dec(p,headersize);
  Free(P);
  CFreeMem:=0;
end;

Function CFreeMemSize(p:pointer;Size:ptruint):ptruint;

begin
  if size<=0 then
    exit;
  if (p <> nil) then
    begin
      if (size <> Pptruint(p-headersize)^) then
        runerror(204);
    end;
  CFreeMemSize:=CFreeMem(P);
end;

Function CAllocMem(Size : ptruint) : Pointer;

begin
  CAllocMem:=calloc(Size+headersize,1);
  if (CAllocMem <> nil) then
    begin
      Pptruint(CAllocMem)^ := size;
      inc(CAllocMem,headersize);
    end;
end;

Function CReAllocMem (var p:pointer;Size:ptruint):Pointer;

begin
  if size=0 then
    begin
      if p<>nil then
        begin
          dec(p,headersize);
          free(p);
          p:=nil;
        end;
    end
  else
    begin
      inc(size,headersize);
      if p=nil then
        p:=malloc(Size)
      else
        begin
          dec(p,headersize);
          p:=realloc(p,size);
        end;
      if (p <> nil) then
        begin
          Pptruint(p)^ := size-headersize;
          inc(p,headersize);
        end;
    end;
  CReAllocMem:=p;
end;

Function CMemSize (p:pointer): ptruint;

begin
  CMemSize:=Pptruint(p-headersize)^;
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
      InitThread : nil;
      DoneThread : nil;
      RelocateHeap : nil;
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
