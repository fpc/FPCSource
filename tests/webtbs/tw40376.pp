{ %TARGET = win32,win64 }

program tw40376;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Windows, ActiveX, ComObj, TypeLib;

const
  LibGUID: TGUID = '{5B0C2E61-3F1A-4C7B-9E21-40376A000001}';
  TargetGUID: TGUID = '{5B0C2E61-3F1A-4C7B-9E21-40376A000002}';
  PlainGUID: TGUID = '{5B0C2E61-3F1A-4C7B-9E21-40376A000003}';
  DualGUID: TGUID = '{5B0C2E61-3F1A-4C7B-9E21-40376A000004}';
  StdOleGUID: TGUID = '{00020430-0000-0000-C000-000000000046}';
  UIAutomationCore = 'UIAutomationCore.dll';

var
  Errors: Integer = 0;
  StdOle: ITypeLib;

// Reports a failure when aSource does not contain aExpected.
procedure CheckContains(const aSource, aExpected, aMsg: string);

begin
  if Pos(aExpected, aSource) = 0 then
  begin
    Writeln('FAIL: ', aMsg);
    Writeln('  expected: ', aExpected);
    Inc(Errors);
  end
  else
    Writeln('OK:   ', aMsg);
end;


// Reports a failure when aSource contains aUnexpected.
procedure CheckNotContains(const aSource, aUnexpected, aMsg: string);

begin
  if Pos(aUnexpected, aSource) <> 0 then
  begin
    Writeln('FAIL: ', aMsg);
    Writeln('  unexpected: ', aUnexpected);
    Inc(Errors);
  end
  else
    Writeln('OK:   ', aMsg);
end;


// Returns a reference in aInfo to the stdole2 type information for aIID.
function StdOleRef(const aInfo: ICreateTypeInfo; const aIID: TGUID): HREFTYPE;

var
  lTI: ITypeInfo;

begin
  OleCheck(StdOle.GetTypeInfoOfGuid(aIID, lTI));
  OleCheck(aInfo.AddRefTypeInfo(lTI, Result));
end;


// Creates an interface type information named aName with ancestor aBase from stdole2.
function NewInterface(const aLib: ICreateTypeLib2; const aName: WideString; const aGUID, aBase: TGUID;
  aFlags: UINT): ICreateTypeInfo;

begin
  OleCheck(aLib.CreateTypeInfo(PWideChar(aName), TKIND_INTERFACE, Result));
  OleCheck(Result.SetGuid(aGUID));
  OleCheck(Result.SetTypeFlags(aFlags));
  OleCheck(Result.AddImplType(0, StdOleRef(Result, aBase)));
end;


// Adds method aIndex returning HRESULT; the last of aParams is [out, retval].
procedure AddMethod(const aInfo: ICreateTypeInfo; aIndex: Integer; aMemID: MEMBERID;
  const aNames: array of WideString; const aParams: array of TYPEDESC);

var
  lFD: FUNCDESC;
  lElems: array of ELEMDESC;
  lNames: array of POleStr;
  i: Integer;

begin
  SetLength(lElems, Length(aParams));
  for i := 0 to High(aParams) do
  begin
    FillChar(lElems[i], SizeOf(ELEMDESC), 0);
    lElems[i].tdesc := aParams[i];
    if i = High(aParams) then
      lElems[i].paramdesc.wParamFlags := PARAMFLAG_FOUT or PARAMFLAG_FRETVAL
    else
      lElems[i].paramdesc.wParamFlags := PARAMFLAG_FIN;
  end;
  FillChar(lFD, SizeOf(lFD), 0);
  lFD.memid := aMemID;
  lFD.lprgelemdescParam := @lElems[0];
  lFD.funckind := FUNC_PUREVIRTUAL;
  lFD.invkind := INVOKE_FUNC;
  lFD.callconv := CC_STDCALL;
  lFD.cParams := Length(aParams);
  lFD.elemdescFunc.tdesc.vt := VT_HRESULT;
  OleCheck(aInfo.AddFuncDesc(aIndex, lFD));
  SetLength(lNames, Length(aNames));
  for i := 0 to High(aNames) do
    lNames[i] := PWideChar(aNames[i]);
  OleCheck(aInfo.SetFuncAndParamNames(aIndex, POleStr(@lNames[0]),Length(lNames)));
end;


// Writes a type library with a plain IUnknown interface and a dual interface to aFileName.
procedure BuildTypeLib(const aFileName: WideString);

var
  lLib: ICreateTypeLib2;
  lTarget, lPlain, lDual: ICreateTypeInfo;
  lTargetRef: HREFTYPE;
  lTargetType, lPtrTarget, lPtrPtrTarget, lInt, lPtrInt: TYPEDESC;

begin
  {$ifdef CPU64}
  OleCheck(CreateTypeLib2(SYS_WIN64, PWideChar(aFileName), lLib));
  {$else}
  OleCheck(CreateTypeLib2(SYS_WIN32, PWideChar(aFileName), lLib));
  {$endif}
  OleCheck(lLib.SetName('TW40376'));
  OleCheck(lLib.SetGuid(LibGUID));
  OleCheck(lLib.SetVersion(1, 0));
  OleCheck(lLib.SetLcid(0));

  lTarget := NewInterface(lLib, 'ITarget', TargetGUID, IUnknown, 0);
  lPlain := NewInterface(lLib, 'IPlain', PlainGUID, IUnknown, 0);
  OleCheck(lPlain.AddRefTypeInfo(lTarget as ITypeInfo, lTargetRef));

  FillChar(lTargetType, SizeOf(TYPEDESC), 0);
  lTargetType.vt := VT_USERDEFINED;
  lTargetType.hreftype := lTargetRef;
  FillChar(lPtrTarget, SizeOf(TYPEDESC), 0);
  lPtrTarget.vt := VT_PTR;
  lPtrTarget.lptdesc := @lTargetType;
  FillChar(lPtrPtrTarget, SizeOf(TYPEDESC), 0);
  lPtrPtrTarget.vt := VT_PTR;
  lPtrPtrTarget.lptdesc := @lPtrTarget;
  FillChar(lInt, SizeOf(TYPEDESC), 0);
  lInt.vt := VT_I4;
  FillChar(lPtrInt, SizeOf(TYPEDESC), 0);
  lPtrInt.vt := VT_PTR;
  lPtrInt.lptdesc := @lInt;

  AddMethod(lPlain, 0, $60010000, ['GetTarget', 'aTarget'], [lPtrPtrTarget]);
  AddMethod(lPlain, 1, $60010001, ['FromHandle', 'aHandle', 'aTarget'], [lInt, lPtrPtrTarget]);
  AddMethod(lPlain, 2, $60010002, ['GetCount', 'aCount'], [lPtrInt]);

  lDual := NewInterface(lLib, 'IDualTest', DualGUID, IDispatch, TYPEFLAG_FDUAL or TYPEFLAG_FOLEAUTOMATION);
  AddMethod(lDual, 0, 1, ['GetCount', 'aCount'], [lPtrInt]);
  AddMethod(lDual, 1, 2, ['Scale', 'aFactor', 'aResult'], [lInt, lPtrInt]);

  OleCheck(lTarget.LayOut);
  OleCheck(lPlain.LayOut);
  OleCheck(lDual.LayOut);
  OleCheck(lLib.SaveAllChanges);
end;


// Returns the unit generated by the importer for aFileName.
function Import(const aFileName: WideString): string;

begin
  with TTypeLibImporter.Create(nil) do
    try
      InputFileName := aFileName;
      Execute;
      Result := UnitSource.Text;
    finally
      Free;
    end;
end;


// Checks the import of a generated type library.
procedure TestGeneratedLib;

var
  lFileName: WideString;
  lSource: string;

begin
  lFileName := WideString(GetTempDir(False) + 'tw40376.tlb');
  BuildTypeLib(lFileName);
  try
    lSource := Import(lFileName);
  finally
    SysUtils.DeleteFile(string(lFileName));
  end;
  CheckContains(lSource, 'function GetTarget(out aTarget:ITarget): HRESULT;stdcall;',
    'stdcall retval interface parameter is kept as out parameter');
  CheckContains(lSource, 'function FromHandle(aHandle:Integer;out aTarget:ITarget): HRESULT;stdcall;',
    'stdcall retval parameter after an in parameter is kept as out parameter');
  CheckContains(lSource, 'function GetCount(out aCount:Integer): HRESULT;stdcall;',
    'stdcall retval integer parameter is kept as out parameter');
  CheckNotContains(lSource, 'function GetTarget: HRESULT;stdcall;',
    'stdcall method does not lose its retval parameter');
  CheckContains(lSource, 'function GetCount: Integer;safecall;',
    'safecall method folds retval parameter into the result');
  CheckContains(lSource, 'function Scale(aFactor:Integer): Integer;safecall;',
    'safecall method with in parameter folds retval parameter into the result');
  CheckContains(lSource, 'function GetCount: Integer;dispid 1;',
    'dispinterface method folds retval parameter into the result');
  CheckContains(lSource, 'function Scale(aFactor:Integer): Integer;dispid 2;',
    'dispinterface method with in parameter folds retval parameter into the result');
  if Errors > 0 then
    Writeln(lSource);
end;


// Returns the Windows system directory.
function SystemDir: string;

var
  lBuf: array[0..MAX_PATH] of AnsiChar;

begin
  SetString(Result, lBuf, GetSystemDirectoryA(lBuf, MAX_PATH));
end;


// Checks the import of UIAutomationCore.dll, the library of the original report.
procedure TestUIAutomation;

var
  lFileName: string;
  lSource: string;

begin
  lFileName := IncludeTrailingPathDelimiter(SystemDir) + UIAutomationCore;
  if not FileExists(lFileName) then
  begin
    Writeln('SKIP: ', lFileName, ' not found');
    Exit;
  end;
  lSource := Import(WideString(lFileName));
  if Pos('IUIAutomation = interface', lSource) = 0 then
  begin
    Writeln('SKIP: ', lFileName, ' does not contain IUIAutomation');
    Exit;
  end;
  CheckContains(lSource, 'function GetRootElement(out root:IUIAutomationElement): HRESULT;stdcall;',
    'IUIAutomation.GetRootElement has its out parameter');
  CheckContains(lSource, 'out element:IUIAutomationElement): HRESULT;stdcall;',
    'IUIAutomation.ElementFromHandle has its out parameter');
end;


begin
  OleCheck(CoInitialize(nil));
  try
    OleCheck(LoadRegTypeLib(StdOleGUID, 2, 0, 0, StdOle));
    TestGeneratedLib;
    TestUIAutomation;
    StdOle := nil;
  finally
    CoUninitialize;
  end;
  if Errors > 0 then
  begin
    Writeln(Errors, ' check(s) failed');
    Halt(1);
  end;
  Writeln('All checks passed');
end.
