
const
  fpcres2elf_version=1;

type
  TFPCResourceSectionInfo = packed record
    ptr: pointer;     // This always contains the absolute memory address of the section at runtime
    size: longint;    // The size of the section in bytes
  end;
  PTFPCResourceSectionInfo = ^TFPCResourceSectionInfo;

  TFPCResourceSectionTable = packed record
    version: longint;
    resentries: longint;
    ressym: TFPCResourceSectionInfo;
    reshash: TFPCResourceSectionInfo;
    resdata: TFPCResourceSectionInfo;
    resspare: TFPCResourceSectionInfo;
    resstr: TFPCResourceSectionInfo;
  end;
  PFPCResourceSectionTable = ^TFPCResourceSectionTable;

  TFPCResourceInfo = packed record
    reshash: longint;   // always 32bit, contains an ELF hash of the resource entries name
    restype: longint;   // always 32bit, contains the resource type ID compatible with Windows RES IDs
    ptr:     pointer;   // This contains the offset to the resource inside the resdata
                        // section.
    name:    pChar;     // The byte offset to the the resource name inside the ressym section.
    size:    longint;   // The size of the resource entry - 32/64 Bit, depending on platform
  end;
  PFPCResourceInfo = ^TFPCResourceInfo;

  TFPCRuntimeResourceInfo = packed record
    reshash: longint;    // always 32bit, contains an ELF hash of the resource entries name
    restype: longint;    // always 32bit, contains the resource type ID compatible with Windows RES IDs
    ptr:     pointer;    // Memory pointer to the reosource
    name:    ansistring; // String containing the name of the resource
    size:    longint;    // The size of the resource entry - 32/64 Bit, depending on platform
  end;
  PFPCRuntimeResourceInfo = ^TFPCRuntimeResourceInfo;

Var
  InitRes : Boolean = False;
{$ifdef FPC_HAS_RESOURCES}
  FPCResourceSectionLocation : pFPCResourceSectionTable; external name 'FPC_RESLOCATION';
{$else}
  FPCResourceSectionLocation : pFPCResourceSectionTable = Nil;
{$endif}
  FPCRuntimeResourceInfoArray : PFPCRuntimeResourceInfo;
  ResInfoCount : Cardinal;

function HashELF(const S : string) : longint;
{Note: this hash function is described in "Practical Algorithms For
       Programmers" by Andrew Binstock and John Rex, Addison Wesley,
       with modifications in Dr Dobbs Journal, April 1996}
var
  G : longint;
  i : longint;
begin
  Result := 0;
  for i := 1 to length(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

procedure InitializeResources;

var
  i:longint;
  CurrentResource:pFPCResourceInfo;

begin
  If (FPCResourceSectionLocation=Nil) then
    ResInfoCount:=0
  else
    ResInfoCount:=FPCResourceSectionLocation^.resentries;
  If (ResInfoCount<>0) then
    begin
    FPCRuntimeResourceInfoArray:=GetMem(SizeOf(TFPCRuntimeResourceInfo)*ResInfoCount);
    { we must zero out this because name is an ansistring }
    fillchar(FPCRuntimeResourceInfoArray,0,SizeOf(TFPCRuntimeResourceInfo)*ResInfoCount);

    for i:=0 to ResInfoCount-1 do
      begin
        CurrentResource:=pFPCResourceInfo(pointer(FPCResourceSectionLocation^.reshash.ptr+i*sizeof(TFPCResourceInfo)));
        FPCRuntimeResourceInfoArray[i].reshash:=CurrentResource^.reshash;
        FPCRuntimeResourceInfoArray[i].restype:=CurrentResource^.restype;
        FPCRuntimeResourceInfoArray[i].ptr:=pointer(CurrentResource^.ptr)+PtrInt(FPCResourceSectionLocation^.resdata.ptr);
        FPCRuntimeResourceInfoArray[i].name:=pchar(CurrentResource^.name)+PtrInt(FPCResourceSectionLocation^.ressym.ptr);
        FPCRuntimeResourceInfoArray[i].size:=CurrentResource^.size;
      end;
    end;
  InitRes:=true;
end;

Function HINSTANCE : HMODULE;

begin
  Result:=0;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;

var
  i:longint;
  searchhash:longint;
  n : string;

begin
  Result:=0;
  if (ResourceName=nil) then
    Exit;
  If Not InitRes then
    InitializeResources;
  { resources aren't case sensitive }
  n:=upcase(strpas(resourcename));
  searchhash:=HashELF(n);
  I:=0;
  While (Result=0) and (I<ResInfoCount) do
    begin
    if (FPCRuntimeResourceInfoArray[i].reshash=searchhash) and (upcase(FPCRuntimeResourceInfoArray[i].name)=n) then
      result:=i+1;
    Inc(I);
    end;
end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
begin
  If Not InitRes then
    InitializeResources;
  if (ResHandle>0) and (ResHandle-1<=ResInfoCount) then
    result:=HGLOBAL(FPCRuntimeResourceInfoArray[ResHandle-1].ptr)
  else
    result:=0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  If Not InitRes then
    InitializeResources;
  if (ResHandle>0) and (ResHandle-1<=ResInfoCount) then
    result:=FPCRuntimeResourceInfoArray[ResHandle-1].size
  else
    result:=0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  result:=Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  result:=False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  result:=True;
end;
