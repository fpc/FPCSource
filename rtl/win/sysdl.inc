{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS dependent part for loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Function SysLoadLibraryU(const Name : UnicodeString) : TlibHandle;

begin
  Result:=WinLoadLibraryW(PWideChar(Name));
end;

Function SysLoadLibraryA(const Name: RawByteString) : TLibHandle;

begin
  Result:=WinLoadLibraryW(PWideChar(UnicodeString(Name)));
end;

Function SysGetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=WinGetProcAddress(Lib,PAnsiChar(ProcName));
end;

{$push}
{$warn 4056 off}
Function SysGetProcedureAddressOrdinal(Lib : TLibHandle; Ordinal : TOrdinalEntry) : Pointer;

begin
  Result:=WinGetProcAddress(Lib,PAnsiChar(Ordinal));
end;
{$pop}

Function SysUnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=WinFreeLibrary(Lib);
end;

Function SysGetLoadErrorStr: ansistring;

Var
  rc,c : integer;
  temp: WideString;
begin
  rc := GetLastError;
  SetLength(temp,255);
  C:=FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM,nil,rc,
                 MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 @temp[1], 255,nil);
  SetLength(temp,c);
  Result:=AnsiString(temp);
end;

type
  TSysMemoryBasicInformation = record
    BaseAddress: Pointer;
    AllocationBase: Pointer;
    AllocationProtect: DWord;
    RegionSize: PtrUInt;
    State: DWord;
    Protect: DWord;
    MemType: DWord;
  end;

  TSysImageExportDirectory = packed record
    Characteristics: DWord;
    TimeDateStamp: DWord;
    MajorVersion: Word;
    MinorVersion: Word;
    Name: DWord;
    Base: DWord;
    NumberOfFunctions: DWord;
    NumberOfNames: DWord;
    AddressOfFunctions: DWord;
    AddressOfNames: DWord;
    AddressOfNameOrdinals: DWord;
  end;
  PSysImageExportDirectory = ^TSysImageExportDirectory;

const
  SYS_MEM_COMMIT = $1000;
  SYS_MEM_IMAGE = $1000000;

function SysVirtualQuery(lpAddress: Pointer; var lpBuffer: TSysMemoryBasicInformation; dwLength: PtrUInt): PtrUInt; stdcall; external KernelDLL name 'VirtualQuery';
function SysGetModuleFileNameA(hModule: THandle; lpFilename: PAnsiChar; nSize: DWord): DWord; stdcall; external KernelDLL name 'GetModuleFileNameA';

// Finds the module containing Addr and the exported function at or before it
Function SysGetSymbolByAddr(Addr: CodePointer; out Info: TSymbolAddressInfo): Boolean;

var
  lMemInfo: TSysMemoryBasicInformation;
  lBase: PByte;
  lNt: PByte;
  lDirOffset: PtrUInt;
  lExportRVA, lExportSize, lRVA, lFuncRVA, lBestRVA: DWord;
  lExports: PSysImageExportDirectory;
  lFunctions, lNames: PDWord;
  lOrdinals: PWord;
  lPath: array[0..1023] of AnsiChar;
  lLen, lBestName: SizeInt;
  i: DWord;

begin
  Info.SymbolName:='';
  Info.SymbolAddr:=nil;
  Info.ModuleName:='';
  Info.ModuleOffset:=0;
  Result:=False;
  if (SysVirtualQuery(Addr,lMemInfo,SizeOf(lMemInfo))=0) or (lMemInfo.State<>SYS_MEM_COMMIT) or
     (lMemInfo.MemType<>SYS_MEM_IMAGE) or (lMemInfo.AllocationBase=nil) then
    exit;
  lBase:=lMemInfo.AllocationBase;
  if PWord(lBase)^<>$5A4D then
    exit;
  Result:=True;
  Info.ModuleOffset:=CodePtrUInt(Addr)-CodePtrUInt(lBase);
  lLen:=SysGetModuleFileNameA(THandle(lBase),@lPath[0],Length(lPath));
  if lLen>255 then
    Info.ModuleName:=PAnsiChar(@lPath[lLen-255])
  else if lLen>0 then
    Info.ModuleName:=PAnsiChar(@lPath[0]);
  lNt:=lBase+PLongInt(lBase+$3C)^;
  if PDWord(lNt)^<>$00004550 then
    exit;
  { DataDirectory starts after the 24 byte file header and 96/112 bytes of the optional header }
  case PWord(lNt+24)^ of
    $10B: lDirOffset:=24+96;
    $20B: lDirOffset:=24+112;
  else
    exit;
  end;
  if PDWord(lNt+lDirOffset-4)^<1 then
    exit;
  lExportRVA:=PDWord(lNt+lDirOffset)^;
  lExportSize:=PDWord(lNt+lDirOffset+4)^;
  if (lExportRVA=0) or (lExportSize=0) then
    exit;
  lRVA:=DWord(PtrUInt(Addr)-PtrUInt(lBase));
  lExports:=PSysImageExportDirectory(lBase+lExportRVA);
  lFunctions:=PDWord(lBase+lExports^.AddressOfFunctions);
  lNames:=PDWord(lBase+lExports^.AddressOfNames);
  lOrdinals:=PWord(lBase+lExports^.AddressOfNameOrdinals);
  lBestRVA:=0;
  lBestName:=-1;
  if lExports^.NumberOfNames>0 then
    for i:=0 to lExports^.NumberOfNames-1 do
      begin
      if lOrdinals[i]>=lExports^.NumberOfFunctions then
        continue;
      lFuncRVA:=lFunctions[lOrdinals[i]];
      { forwarded exports point into the export directory }
      if (lFuncRVA>=lExportRVA) and (lFuncRVA<lExportRVA+lExportSize) then
        continue;
      if (lFuncRVA<=lRVA) and (lFuncRVA>=lBestRVA) and (lFuncRVA<>0) then
        begin
        lBestRVA:=lFuncRVA;
        lBestName:=i;
        end;
      end;
  if lBestName>=0 then
    begin
    Info.SymbolName:=PAnsiChar(lBase+lNames[lBestName]);
    Info.SymbolAddr:=CodePointer(lBase+lBestRVA);
    end;
end;

const
  SysDynLibsManager: TDynLibsManager = (
    LoadLibraryU: @SysLoadLibraryU;
    LoadLibraryA: @SysLoadLibraryA;
    GetProcAddress: @SysGetProcedureAddress;
    GetProcAddressOrdinal: @SysGetProcedureAddressOrdinal;
    UnloadLibrary: @SysUnloadLibrary;
    GetLoadErrorStr: @SysGetLoadErrorStr;
    GetSymbolByAddr: @SysGetSymbolByAddr;
  );

procedure InitSystemDynLibs;
begin
  SetDynLibsManager(SysDynLibsManager);
end;
