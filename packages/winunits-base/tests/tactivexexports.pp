{
    Checks on the running Windows that every static import of the ActiveX unit is exported,
    that the names removed from or implemented in the unit are not, and that the Var*
    conversions implemented in Pascal give the right results. Exit code: number of failures.
}
program tactivexexports;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, ActiveX;

type
  TImportEntry = record
    DllName: String;
    ProcName: String;
  end;

  TRtlOSVersionInfoW = record
    dwOSVersionInfoSize, dwMajorVersion, dwMinorVersion, dwBuildNumber, dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar;
  end;

const
{$i tactivexexports.inc}

  AbsentCount = 18;
  AbsentImports: array[0..AbsentCount-1] of TImportEntry = (
    (DllName: 'ole32.dll'; ProcName: 'CoHasStrongExternalConnections'),
    (DllName: 'ole32.dll'; ProcName: 'DllCanUnloadNow'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleCheckoutMacInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleCheckinMacInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleWrapMacInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleUnwrapMacInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleCheckoutWinInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleCheckinWinInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleWrapWinInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleUnwrapWinInterface'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleVersion'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleSetInPlaceWindow'),
    (DllName: 'ole32.dll'; ProcName: 'WlmOleRegisterUserWrap'),
    (DllName: 'oleaut32.dll'; ProcName: 'VarI4FromInt'),
    (DllName: 'oleaut32.dll'; ProcName: 'VarI8FromI4'),
    (DllName: 'oleaut32.dll'; ProcName: 'VarI8FromInt'),
    (DllName: 'oleaut32.dll'; ProcName: 'VarUI8FromI4'),
    (DllName: 'oleaut32.dll'; ProcName: 'VarUI8FromInt')
  );

var
  Failures: Integer = 0;
  Verbose: Boolean = False;
  OnWine: Boolean = False;


// Print and count a failed check, print a passed one in verbose mode
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  if not aCondition then
    begin
    Inc(Failures);
    Writeln('  FAIL  ',aMsg);
    end
  else if Verbose then
    Writeln('  PASS  ',aMsg);
end;


// Windows version as reported by RtlGetVersion, which is not affected by compatibility shims
function WindowsVersion: String;

type
  TRtlGetVersion = function(var aInfo: TRtlOSVersionInfoW): LONG; stdcall;

var
  lProc: TRtlGetVersion;
  lInfo: TRtlOSVersionInfoW;
  lNtDll: HMODULE;

begin
  Result:='unknown Windows version';
  lNtDll:=GetModuleHandle('ntdll.dll');
  if lNtDll=0 then
    Exit;
  OnWine:=GetProcAddress(lNtDll,'wine_get_version')<>nil;
  lProc:=TRtlGetVersion(GetProcAddress(lNtDll,'RtlGetVersion'));
  FillChar(lInfo,SizeOf(lInfo),0);
  lInfo.dwOSVersionInfoSize:=SizeOf(lInfo);
  if Assigned(lProc) and (lProc(lInfo)=0) then
    Result:=Format('Windows %d.%d build %d',[lInfo.dwMajorVersion,lInfo.dwMinorVersion,lInfo.dwBuildNumber]);
  if OnWine then
    Result:='Wine, '+Result;
end;


// True if aEntry.ProcName is exported by aEntry.DllName
function IsExported(const aEntry: TImportEntry): Boolean;

var
  lLib: HMODULE;

begin
  lLib:=LoadLibrary(PChar(aEntry.DllName));
  Result:=(lLib<>0) and (GetProcAddress(lLib,PChar(aEntry.ProcName))<>nil);
  if lLib<>0 then
    FreeLibrary(lLib);
end;


// True on Wine when aEntry is an ole32 function that Wine only exports from combase.dll
function IsWineCombaseOnly(const aEntry: TImportEntry): Boolean;

var
  lCombase: TImportEntry;

begin
  Result:=False;
  if not OnWine or not SameText(aEntry.DllName,'ole32.dll') then
    Exit;
  lCombase.DllName:='combase.dll';
  lCombase.ProcName:=aEntry.ProcName;
  Result:=IsExported(lCombase);
end;


// Every static import of ActiveX is exported
procedure CheckImports;

var
  I, lMissing, lWineOnly: Integer;

begin
  Writeln('Static imports of the ActiveX unit (',ActiveXImportCount,')');
  lMissing:=0;
  lWineOnly:=0;
  for I:=0 to ActiveXImportCount-1 do
    if not IsExported(ActiveXImports[I]) then
      if IsWineCombaseOnly(ActiveXImports[I]) then
        begin
        Inc(lWineOnly);
        Writeln('  WINE  ',ActiveXImports[I].ProcName,' is exported by Wine''s combase.dll, not by its ole32.dll');
        end
      else
        begin
        Inc(lMissing);
        Check(False,ActiveXImports[I].DllName+' does not export '+ActiveXImports[I].ProcName);
        end;
  Write('  ',ActiveXImportCount-lMissing-lWineOnly,' exported, ',lMissing,' missing');
  if lWineOnly>0 then
    Write(', ',lWineOnly,' only in Wine''s combase.dll');
  Writeln;
end;


// The names removed from or implemented in ActiveX are not exported
procedure CheckAbsent;

var
  I, lPresent: Integer;

begin
  Writeln('Names removed from or implemented in the ActiveX unit (',AbsentCount,')');
  lPresent:=0;
  for I:=0 to AbsentCount-1 do
    if IsExported(AbsentImports[I]) then
      begin
      Inc(lPresent);
      Check(False,AbsentImports[I].DllName+' exports '+AbsentImports[I].ProcName);
      end;
  Writeln('  ',AbsentCount-lPresent,' not exported, ',lPresent,' exported');
end;


// The Var* conversions implemented in Pascal
procedure CheckConversions;

var
  lL: LONG;
  lI8: LONG64;
  lU8: ULONG64;
  lStart: Integer;

begin
  Writeln('Var* conversions implemented in the ActiveX unit');
  lStart:=Failures;
  Check((VarI4FromInt(-7,lL)=S_OK) and (lL=-7),'VarI4FromInt(-7) gives -7');
  Check((VarI8FromI4(-123456,@lI8)=S_OK) and (lI8=-123456),'VarI8FromI4(-123456) gives -123456');
  Check((VarI8FromInt(High(LongInt),@lI8)=S_OK) and (lI8=High(LongInt)),'VarI8FromInt(MaxInt) gives MaxInt');
  Check((VarI8FromInt(Low(LongInt),@lI8)=S_OK) and (lI8=Low(LongInt)),'VarI8FromInt(MinInt) gives MinInt');
  Check((VarUI8FromI4(42,@lU8)=S_OK) and (lU8=42),'VarUI8FromI4(42) gives 42');
  Check(VarUI8FromI4(-1,@lU8)=DISP_E_OVERFLOW,'VarUI8FromI4(-1) returns DISP_E_OVERFLOW');
  Check((VarUI8FromInt(High(LongInt),@lU8)=S_OK) and (lU8=High(LongInt)),'VarUI8FromInt(MaxInt) gives MaxInt');
  Check(VarUI8FromInt(-5,@lU8)=DISP_E_OVERFLOW,'VarUI8FromInt(-5) returns DISP_E_OVERFLOW');
  Writeln('  ',8-(Failures-lStart),' of 8 correct');
end;


begin
  Verbose:=FindCmdLineSwitch('v',['-','/'],True);
  Writeln('tactivexexports, ',{$ifdef CPU64}'64'{$else}'32'{$endif},'-bit, on ',WindowsVersion);
  CheckImports;
  CheckAbsent;
  CheckConversions;
  if Failures=0 then
    Writeln('All checks passed')
  else
    Writeln(Failures,' check(s) failed');
  ExitCode:=Failures;
end.
