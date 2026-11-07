{
    Test of per-user and machine-wide COM registration with comperuserexe.exe and comperuserdll.dll.
    The three binaries must be in the same directory. Exit code is the number of failed checks.
}
program tcomperuser;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, ActiveX, ComObj, Registry, comperuserintf;

type
  TDllInstallProc = function(bInstall: LongBool; pszCmdLine: PWideChar): HResult; stdcall;

var
  FailCount: Integer = 0;
  SkipCount: Integer = 0;
  PassCount: Integer = 0;
  OnWine: Boolean = False;
  MergedView: Boolean = False;
  ExeName, DllName: String;


// Print and count the result of one check
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  if aCondition then
    begin
    Inc(PassCount);
    Writeln('  PASS  ',aMsg);
    end
  else
    begin
    Inc(FailCount);
    Writeln('  FAIL  ',aMsg);
    end;
end;


// Print and count a check that could not be done
procedure Skip(const aMsg: String);

begin
  Inc(SkipCount);
  Writeln('  SKIP  ',aMsg);
end;


// True if aKey exists under aRoot
function KeyExists(aRoot: HKEY; const aKey: String): Boolean;

var
  lReg: TRegistry;

begin
  lReg:=TRegistry.Create(KEY_READ);
  try
    lReg.RootKey:=aRoot;
    Result:=lReg.KeyExists(aKey);
  finally
    lReg.Free;
  end;
end;


// String value aName of aKey under aRoot, empty if it does not exist
function ReadValue(aRoot: HKEY; const aKey, aName: String): String;

var
  lReg: TRegistry;

begin
  Result:='';
  lReg:=TRegistry.Create(KEY_READ);
  try
    lReg.RootKey:=aRoot;
    if lReg.OpenKeyReadOnly(aKey) then
      try
        if lReg.ValueExists(aName) then
          Result:=lReg.ReadString(aName);
      finally
        lReg.CloseKey;
      end;
  finally
    lReg.Free;
  end;
end;


// Exit code of ExeName started with aSwitch
function RunExe(const aSwitch: String): Integer;

begin
  Flush(Output);
  try
    Result:=ExecuteProcess(ExeName,aSwitch);
  except
    on E: Exception do
      begin
      Writeln('  could not run ',ExeName,': ',E.Message);
      Result:=-1;
      end;
  end;
end;


// Registry key of class aClassID, relative to a classes root
function ClsidKey(const aClassID: TGUID): String;

begin
  Result:='CLSID\'+GUIDToString(aClassID);
end;


// Registry key of the test type library, relative to a classes root
function TypeLibKey: String;

begin
  Result:='TypeLib\'+GUIDToString(LIBID_PerUserTest);
end;


// True when running under Wine
function DetectWine: Boolean;

var
  lNtDll: HMODULE;

begin
  lNtDll:=GetModuleHandle('ntdll.dll');
  Result:=(lNtDll<>0) and (GetProcAddress(lNtDll,'wine_get_version')<>nil);
end;


// True if keys under HKCU\Software\Classes show up in HKEY_CLASSES_ROOT
function DetectMergedView: Boolean;

const
  ProbeKey = 'FPCPerUserMergeProbe';

var
  lReg: TRegistry;

begin
  lReg:=TRegistry.Create;
  try
    lReg.RootKey:=HKEY_CURRENT_USER;
    if not lReg.OpenKey('Software\Classes\'+ProbeKey,True) then
      Exit(False);
    lReg.CloseKey;
    Result:=KeyExists(HKEY_CLASSES_ROOT,ProbeKey);
    lReg.DeleteKey('Software\Classes\'+ProbeKey);
  finally
    lReg.Free;
  end;
end;


// Per-user registration of the out-of-process server
procedure TestExePerUser;

var
  lKey: String;
  lUnk: IUnknown;
  lClassID: TGUID;
  lRes: HResult;

begin
  Writeln('Out-of-process server, per user');
  lKey:='Software\Classes\'+ClsidKey(CLASS_PerUserExe);
  Check(RunExe('/regserverperuser')=0,'/regserverperuser exits with 0');
  Check(KeyExists(HKEY_CURRENT_USER,lKey+'\LocalServer32'),'LocalServer32 key under HKCU');
  Check(SameText(ExpandFileName(ReadValue(HKEY_CURRENT_USER,lKey+'\LocalServer32','')),ExpandFileName(ExeName)),
    'LocalServer32 names the server executable');
  Check(ReadValue(HKEY_CURRENT_USER,lKey+'\LocalServer32','ThreadingModel')='Apartment','ThreadingModel is Apartment');
  Check(ReadValue(HKEY_CURRENT_USER,lKey+'\ProgID','')=PerUserTypeLibName+'.PerUserExe','ProgID under the class key');
  Check(ReadValue(HKEY_CURRENT_USER,'Software\Classes\'+PerUserTypeLibName+'.PerUserExe\CLSID','')=GUIDToString(CLASS_PerUserExe),
    'ProgID key under HKCU names the class');
  Check(not KeyExists(HKEY_LOCAL_MACHINE,'Software\Classes\'+ClsidKey(CLASS_PerUserExe)),'No class key under HKLM');
  if KeyExists(HKEY_CURRENT_USER,'Software\Classes\'+TypeLibKey) then
    Check(True,'Type library registered under HKCU')
  else if OnWine and KeyExists(HKEY_LOCAL_MACHINE,'Software\Classes\'+TypeLibKey) then
    Skip('Type library registered under HKLM: Wine''s RegisterTypeLibForUser registers machine-wide')
  else
    Check(False,'Type library registered under HKCU');
  if not MergedView then
    Skip('Lookup through HKEY_CLASSES_ROOT, CLSIDFromProgID and CoCreateInstance: HKCU\Software\Classes is not merged into HKEY_CLASSES_ROOT')
  else
    begin
    Check(ReadValue(HKEY_CLASSES_ROOT,ClsidKey(CLASS_PerUserExe)+'\LocalServer32','')<>'',
      'Class visible through HKEY_CLASSES_ROOT');
    Check(Succeeded(CLSIDFromProgID(PWideChar(WideString(PerUserTypeLibName+'.PerUserExe')),lClassID))
      and IsEqualGUID(lClassID,CLASS_PerUserExe),'CLSIDFromProgID resolves the per-user ProgID');
    lRes:=CoCreateInstance(CLASS_PerUserExe,nil,CLSCTX_LOCAL_SERVER,IUnknown,lUnk);
    Check(Succeeded(lRes),Format('CoCreateInstance starts the per-user server (HRESULT $%.8x)',[lRes]));
    end;
  lUnk:=nil;
  Check(RunExe('/unregserverperuser')=0,'/unregserverperuser exits with 0');
  Check(not KeyExists(HKEY_CURRENT_USER,lKey),'Class key removed from HKCU');
  Check(not KeyExists(HKEY_CURRENT_USER,'Software\Classes\'+PerUserTypeLibName+'.PerUserExe'),'ProgID key removed from HKCU');
  Check(not KeyExists(HKEY_CURRENT_USER,'Software\Classes\'+TypeLibKey),'Type library removed from HKCU');
end;


// Machine-wide registration of the out-of-process server
procedure TestExeMachine;

var
  lKey: String;
  lUnk: IUnknown;
  lRes: HResult;
  lCode: Integer;

begin
  Writeln('Out-of-process server, machine-wide');
  lKey:='Software\Classes\'+ClsidKey(CLASS_PerUserExe);
  lCode:=RunExe('/regserver');
  if lCode<>0 then
    begin
    Check(lCode=1,Format('Failed /regserver exits with 1 instead of an unhandled exception (exit code %d)',[lCode]));
    Skip('/regserver failed, administrator rights are needed for machine-wide registration');
    Exit;
    end;
  Check(KeyExists(HKEY_LOCAL_MACHINE,lKey+'\LocalServer32'),'LocalServer32 key under HKLM');
  Check(not KeyExists(HKEY_CURRENT_USER,lKey),'No class key under HKCU');
  lRes:=CoCreateInstance(CLASS_PerUserExe,nil,CLSCTX_LOCAL_SERVER,IUnknown,lUnk);
  Check(Succeeded(lRes),Format('CoCreateInstance starts the machine-wide server (HRESULT $%.8x)',[lRes]));
  lUnk:=nil;
  Check(RunExe('/unregserver')=0,'/unregserver exits with 0');
  Check(not KeyExists(HKEY_LOCAL_MACHINE,lKey),'Class key removed from HKLM');
end;


// Registration of the in-process server through DllInstall
procedure TestDll;

var
  lLib: HMODULE;
  lInstall: TDllInstallProc;
  lKey: String;
  lObj: ITestPerUser;
  lRes: HResult;

begin
  Writeln('In-process server');
  lLib:=LoadLibrary(PChar(DllName));
  Check(lLib<>0,'Load '+DllName);
  if lLib=0 then
    Exit;
  try
    lInstall:=TDllInstallProc(GetProcAddress(lLib,'DllInstall'));
    Check(Assigned(lInstall),'DllInstall is exported');
    if not Assigned(lInstall) then
      Exit;
    lKey:='Software\Classes\'+ClsidKey(CLASS_PerUserDll);
    Check(lInstall(True,'user')=S_OK,'DllInstall(True, "user") returns S_OK');
    Check(SameText(ExpandFileName(ReadValue(HKEY_CURRENT_USER,lKey+'\InprocServer32','')),ExpandFileName(DllName)),
      'InprocServer32 under HKCU names the library');
    Check(not KeyExists(HKEY_LOCAL_MACHINE,lKey),'No class key under HKLM');
    if not MergedView then
      Skip('CoCreateInstance of the per-user class: HKCU\Software\Classes is not merged into HKEY_CLASSES_ROOT')
    else
      begin
      lRes:=CoCreateInstance(CLASS_PerUserDll,nil,CLSCTX_INPROC_SERVER,IID_ITestPerUser,lObj);
      Check(Succeeded(lRes),Format('CoCreateInstance finds the per-user class (HRESULT $%.8x)',[lRes]));
      if Succeeded(lRes) then
        Check(lObj.Answer=PerUserAnswer,'Method call on the per-user object');
      end;
    lObj:=nil;
    Check(lInstall(False,'user')=S_OK,'DllInstall(False, "user") returns S_OK');
    Check(not KeyExists(HKEY_CURRENT_USER,lKey),'Class key removed from HKCU');

    if lInstall(True,nil)<>S_OK then
      Skip('DllInstall(True, nil) failed, administrator rights are needed for machine-wide registration')
    else
      begin
      Check(KeyExists(HKEY_LOCAL_MACHINE,lKey+'\InprocServer32'),'DllInstall(True, nil) registers under HKLM');
      Check(not KeyExists(HKEY_CURRENT_USER,lKey),'DllInstall(True, nil) writes nothing under HKCU');
      lRes:=CoCreateInstance(CLASS_PerUserDll,nil,CLSCTX_INPROC_SERVER,IID_ITestPerUser,lObj);
      Check(Succeeded(lRes),Format('CoCreateInstance finds the machine-wide class (HRESULT $%.8x)',[lRes]));
      if Succeeded(lRes) then
        Check(lObj.Answer=PerUserAnswer,'Method call on the machine-wide object');
      lObj:=nil;
      Check(lInstall(False,nil)=S_OK,'DllInstall(False, nil) returns S_OK');
      Check(not KeyExists(HKEY_LOCAL_MACHINE,lKey),'Class key removed from HKLM');
      end;
  finally
    FreeLibrary(lLib);
  end;
end;


begin
  ExeName:=ExtractFilePath(ExpandFileName(ParamStr(0)))+'comperuserexe.exe';
  DllName:=ExtractFilePath(ExpandFileName(ParamStr(0)))+'comperuserdll.dll';
  OnWine:=DetectWine;
  if OnWine then
    Writeln('Running under Wine');
  MergedView:=DetectMergedView;
  if not MergedView then
    Writeln('HKCU\Software\Classes is not merged into HKEY_CLASSES_ROOT');
  TestExePerUser;
  TestExeMachine;
  TestDll;
  Writeln(Format('%d passed, %d failed, %d skipped',[PassCount,FailCount,SkipCount]));
  ExitCode:=FailCount;
end.
