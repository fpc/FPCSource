unit gcmem;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Const
  LibName = 'gc';

type
    ptrdiff_t = longint;
    size_t = dword;
    wchar_t = longint;
    GC_PTR = pointer;
    GC_word = dword;
    GC_signed_word = longint;

// procedure GC_init;cdecl; external LibName name 'GC_init';
// not needed
Function Malloc(Size : size_t) : Pointer; cdecl; external LibName name 'GC_malloc';
Procedure Free(P : pointer); cdecl; external LibName name 'GC_free';
function ReAlloc(P : Pointer; Size : size_t) : pointer; cdecl; external LibName name 'GC_realloc';
Function Calloc(unitSize,UnitCount : size_t) : pointer;


implementation

Function Calloc(unitSize,UnitCount : size_t) : pointer;
var p:pointer;

begin
  p:=Malloc(unitSize*UnitCount);
  if p<>nil then FillChar(p^, unitSize*UnitCount,0); //not needed
  //GC_malloc seems to clear memory
  Calloc:=p;
end;


Function CGetMem  (Size : ptruint) : Pointer;

begin
  CGetMem:=Malloc(size_t(Size+sizeof(ptruint)));
  if (CGetMem <> nil) then
    begin
      Pptruint(CGetMem)^ := size;
      inc(CGetMem,sizeof(ptruint));
    end;
end;

Function CFreeMem (P : pointer) : ptruint;

begin
  if (p <> nil) then
    dec(p,sizeof(ptruint));
  Free(P);
  CFreeMem:=0;
end;

Function CFreeMemSize(p:pointer;Size:ptruint):ptruint;

begin
  if size<=0 then
    exit;
  if (p <> nil) then
    begin
      if (size <> Pptruint(p-sizeof(ptruint))^) then
        runerror(204);
    end;
  CFreeMemSize:=CFreeMem(P);
end;

Function CAllocMem(Size : ptruint) : Pointer;

begin
  CAllocMem:=calloc(size_t(Size+sizeof(ptruint)),size_t(1));
  if (CAllocMem <> nil) then
    begin
      Pptruint(CAllocMem)^ := size;
      inc(CAllocMem,sizeof(ptruint));
    end;
end;

Function CReAllocMem (var p:pointer;Size:ptruint):Pointer;

begin
  if size=0 then
    begin
      if p<>nil then
        begin
          dec(p,sizeof(ptruint));
          free(p);
          p:=nil;
        end;
    end
  else
    begin
      inc(size,sizeof(ptruint));
      if p=nil then
        p:=malloc(size_t(Size))
      else
        begin
          dec(p,sizeof(ptruint));
          p:=realloc(p,size_t(size));
        end;
      if (p <> nil) then
        begin
          Pptruint(p)^ := size-sizeof(ptruint);
          inc(p,sizeof(ptruint));
        end;
    end;
  CReAllocMem:=p;
end;

Function CMemSize (p:pointer): ptruint;

begin
  CMemSize:=Pptruint(p-sizeof(ptruint))^;
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

