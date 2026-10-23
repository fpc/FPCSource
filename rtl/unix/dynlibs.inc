{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implement OS-dependent part of dynamic library loading.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Dl;
{$ELSE}
uses dl;
{$ENDIF}


Function SysLoadLibraryA(const Name : RawByteString) : TLibHandle;
{$ifdef aix}
var
  MemberName: RawByteString;
{$endif}
begin
{$ifndef aix}
  Result:=TLibHandle(dlopen(PAnsiChar(Name),RTLD_LAZY));
{$else aix}
  { in aix, most shared libraries are static libraries (archives) that contain
    a single object: shr.o for 32 bit, shr_64.o for 64 bit. You have to specify
    this object file explicitly via the RTLD_MEMBER member flag }
{$ifdef cpu64}
  MemberName:='(shr_64.o)';
{$else cpu64}
  MemberName:='(shr.o)';
{$endif cpu64}
  SetCodePage(MemberName,DefaultFileSystemCodePage,false);
  MemberName:=Name+MemberName;
  Result:=TLibHandle(dlopen(PAnsiChar(MemberName),RTLD_LAZY or RTLD_MEMBER));
{$endif aix}
end;

Function SysLoadLibraryU(const Name: UnicodeString) : TLibHandle;
begin
  Result := SysLoadLibraryA(ToSingleByteFileSystemEncodedFileName(Name));
end;

Function SysGetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=dlsym(lib,PAnsiChar(ProcName));
end;

Function SysUnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=dlClose(Lib)=0;
end;

Function SysGetLoadErrorStr: ansistring;

begin
{$IFDEF FPC_DOTTEDUNITS}
  Result:=UnixApi.Dl.dlerror;
{$ELSE}
  Result:=dl.dlerror;
{$ENDIF}
end;

// Returns the load bias of the module whose headers are mapped at aBase
Function ModuleLoadBias(aBase: PByte): CodePtrUInt;

{$ifdef darwin}
const
  MH_MAGIC = $FEEDFACE;
  MH_MAGIC_64 = $FEEDFACF;
  LC_SEGMENT = $1;
  LC_SEGMENT_64 = $19;
  cTextSegment: array[0..6] of AnsiChar = '__TEXT'#0;

var
  lCmd: PByte;
  lCount, i: LongWord;
{$else darwin}
const
  ET_EXEC = 2;
{$endif darwin}

begin
  Result:=CodePtrUInt(aBase);
  if aBase=nil then
    exit;
{$ifdef darwin}
  case PLongWord(aBase)^ of
    MH_MAGIC: lCmd:=aBase+28;
    MH_MAGIC_64: lCmd:=aBase+32;
  else
    exit;
  end;
  lCount:=PLongWord(aBase+16)^;
  for i:=1 to lCount do
    begin
    if (PLongWord(lCmd)^=LC_SEGMENT_64) and (CompareByte(lCmd[8],cTextSegment,SizeOf(cTextSegment))=0) then
      exit(CodePtrUInt(aBase)-CodePtrUInt(PQWord(lCmd+24)^));
    if (PLongWord(lCmd)^=LC_SEGMENT) and (CompareByte(lCmd[8],cTextSegment,SizeOf(cTextSegment))=0) then
      exit(CodePtrUInt(aBase)-CodePtrUInt(PLongWord(lCmd+24)^));
    lCmd:=lCmd+PLongWord(lCmd+4)^;
    end;
{$else darwin}
  if (aBase[0]=$7F) and (aBase[1]=Ord('E')) and (aBase[2]=Ord('L')) and (aBase[3]=Ord('F')) and
     (PWord(aBase+16)^=ET_EXEC) then
    Result:=0;
{$endif darwin}
end;


// Looks up the dynamic symbol at or before Addr with dladdr
Function SysGetSymbolByAddr(Addr: CodePointer; out Info: TSymbolAddressInfo): Boolean;

var
  lDlInfo: dl_info;
  lLen: SizeInt;

begin
  Info.SymbolName:='';
  Info.SymbolAddr:=nil;
  Info.ModuleName:='';
  Info.ModuleOffset:=0;
  FillChar(lDlInfo,SizeOf(lDlInfo),0);
  Result:=dladdr(Addr,@lDlInfo)<>0;
  if not Result then
    exit;
  if Assigned(lDlInfo.dli_sname) then
    begin
    Info.SymbolName:=lDlInfo.dli_sname;
    Info.SymbolAddr:=lDlInfo.dli_saddr;
    end;
  if Assigned(lDlInfo.dli_fname) then
    begin
    Info.ModuleOffset:=CodePtrUInt(Addr)-ModuleLoadBias(lDlInfo.dli_fbase);
    lLen:=IndexByte(lDlInfo.dli_fname^,-1,0);
    if lLen>255 then
      Info.ModuleName:=PAnsiChar(lDlInfo.dli_fname+lLen-255)
    else
      Info.ModuleName:=lDlInfo.dli_fname;
    end;
end;

const
  SysDynLibsManager: TDynLibsManager = (
    LoadLibraryU: @SysLoadLibraryU;
    LoadLibraryA: @SysLoadLibraryA;
    GetProcAddress: @SysGetProcedureAddress;
    GetProcAddressOrdinal: Nil;
    UnloadLibrary: @SysUnloadLibrary;
    GetLoadErrorStr: @SysGetLoadErrorStr;
    GetSymbolByAddr: @SysGetSymbolByAddr;
  );

procedure InitDynLibs;
begin
  SetDynLibsManager(SysDynLibsManager);
end;

