{ Builds a type library with properties that have two index parameters and
  parameters with default values, imports it with TTypeLibImporter and checks
  the generated declarations.
  An optional first argument is the file name to save the generated unit to.
  Exit code 0 means all checks passed. }
program ttypelibidxprop;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Windows, ActiveX, ComObj, typelib;

const
  LibGUID: TGUID = '{5E1C4B1A-7A61-4F0B-9D8E-2B1F3C4D5E60}';
  DualGUID: TGUID = '{5E1C4B1A-7A61-4F0B-9D8E-2B1F3C4D5E61}';
  DispGUID: TGUID = '{5E1C4B1A-7A61-4F0B-9D8E-2B1F3C4D5E62}';

var
  Failures: Integer = 0;
  BSTRDesc: TYPEDESC;
  Defaults: array[0..2] of PARAMDESCEX;
  DefaultValues: array[0..2] of OleVariant;

// Set aElem to a parameter of type aVT with flags aFlags; VT_PTR points to a BSTR.
procedure SetParam(var aElem: ELEMDESC; aVT: TVarType; aFlags: USHORT);

begin
  FillChar(aElem,SizeOf(aElem),0);
  if aVT=VT_PTR then
    aElem.tdesc.lptdesc:=@BSTRDesc;
  aElem.tdesc.vt:=aVT;
  aElem.paramdesc.wParamFlags:=aFlags;
end;


// Give aElem default value aValue, stored in slot aSlot.
procedure SetDefault(var aElem: ELEMDESC; aSlot: Integer; const aValue: OleVariant);

begin
  DefaultValues[aSlot]:=aValue;
  FillChar(Defaults[aSlot],SizeOf(PARAMDESCEX),0);
  Defaults[aSlot].cBytes:=SizeOf(PARAMDESCEX);
  Defaults[aSlot].varDefaultValue:=TVariantArg(DefaultValues[aSlot]);
  aElem.paramdesc.pparamdescex:=@Defaults[aSlot];
  aElem.paramdesc.wParamFlags:=aElem.paramdesc.wParamFlags or PARAMFLAG_FOPT or PARAMFLAG_FHASDEFAULT;
end;


// Add function aIndex to aInfo with the given parameters and names.
procedure AddFunc(aInfo: ICreateTypeInfo; aIndex: Integer; aMemId: MEMBERID; aInvKind: INVOKEKIND;
  aFuncKind: FUNCKIND; aResultVT: TVarType; const aParams: array of ELEMDESC; const aNames: array of WideString);

var
  lDesc: FUNCDESC;
  lNames: array of POleStr;
  I: Integer;

begin
  FillChar(lDesc,SizeOf(lDesc),0);
  lDesc.memid:=aMemId;
  lDesc.funckind:=aFuncKind;
  lDesc.invkind:=aInvKind;
  lDesc.callconv:=CC_STDCALL;
  lDesc.cParams:=Length(aParams);
  if Length(aParams)>0 then
    lDesc.lprgelemdescParam:=@aParams[0];
  if aFuncKind=FUNC_PUREVIRTUAL then
    lDesc.oVft:=(7+aIndex)*SizeOf(Pointer);
  lDesc.elemdescFunc.tdesc.vt:=aResultVT;
  OleCheck(aInfo.AddFuncDesc(aIndex,lDesc));
  SetLength(lNames,Length(aNames));
  for I:=0 to Length(aNames)-1 do
    lNames[I]:=POleStr(aNames[I]);
  OleCheck(aInfo.SetFuncAndParamNames(aIndex,POleStr(@lNames[0]),Length(lNames)));
end;


// Create the type library aFileName with a dual interface and a dispinterface.
procedure CreateLib(const aFileName: WideString);

var
  lLib: ICreateTypeLib2;
  lInfo: ICreateTypeInfo;
  lStdOle: ITypeLib;
  lDispatch: ITypeInfo;
  lRef: HREFTYPE;
  lIdx: array[0..2] of ELEMDESC;
  lOne: array[0..0] of ELEMDESC;
  lFour: array[0..3] of ELEMDESC;

begin
  FillChar(BSTRDesc,SizeOf(BSTRDesc),0);
  BSTRDesc.vt:=VT_BSTR;
  OleCheck(LoadTypeLib('stdole2.tlb',lStdOle));
  OleCheck(lStdOle.GetTypeInfoOfGuid(IDispatch,lDispatch));
  OleCheck(CreateTypeLib2({$ifdef CPU64}SYS_WIN64{$else}SYS_WIN32{$endif},PWideChar(aFileName),lLib));
  OleCheck(lLib.SetName('IdxPropLib'));
  OleCheck(lLib.SetGuid(LibGUID));
  OleCheck(lLib.SetVersion(1,0));

  OleCheck(lLib.CreateTypeInfo('IIdxDual',TKIND_INTERFACE,lInfo));
  OleCheck(lInfo.SetGuid(DualGUID));
  OleCheck(lInfo.SetTypeFlags(TYPEFLAG_FDUAL or TYPEFLAG_FOLEAUTOMATION));
  OleCheck(lInfo.AddRefTypeInfo(lDispatch,lRef));
  OleCheck(lInfo.AddImplType(0,lRef));
  SetParam(lIdx[0],VT_I4,PARAMFLAG_FIN);
  SetParam(lIdx[1],VT_I4,PARAMFLAG_FIN);
  SetParam(lIdx[2],VT_PTR,PARAMFLAG_FOUT or PARAMFLAG_FRETVAL);
  AddFunc(lInfo,0,0,INVOKE_PROPERTYGET,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx,['Item','Row','Col','Value']);
  SetParam(lIdx[2],VT_BSTR,PARAMFLAG_FIN);
  AddFunc(lInfo,1,0,INVOKE_PROPERTYPUT,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx,['Item','Row','Col']);
  SetParam(lOne[0],VT_PTR,PARAMFLAG_FOUT or PARAMFLAG_FRETVAL);
  AddFunc(lInfo,2,1,INVOKE_PROPERTYGET,FUNC_PUREVIRTUAL,VT_HRESULT,lOne,['Caption','Value']);
  SetParam(lOne[0],VT_BSTR,PARAMFLAG_FIN);
  AddFunc(lInfo,3,1,INVOKE_PROPERTYPUT,FUNC_PUREVIRTUAL,VT_HRESULT,lOne,['Caption']);
  SetParam(lFour[0],VT_BSTR,PARAMFLAG_FIN);
  SetParam(lFour[1],VT_I4,PARAMFLAG_FIN);
  SetDefault(lFour[1],0,7);
  SetParam(lFour[2],VT_BSTR,PARAMFLAG_FIN);
  SetDefault(lFour[2],1,WideString('a''b'));
  SetParam(lFour[3],VT_PTR,PARAMFLAG_FOUT or PARAMFLAG_FRETVAL);
  AddFunc(lInfo,4,2,INVOKE_FUNC,FUNC_PUREVIRTUAL,VT_HRESULT,lFour,['Format','Src','Count','Sep','Value']);
  SetParam(lIdx[0],VT_I4,PARAMFLAG_FIN);
  SetDefault(lIdx[0],0,1);
  SetParam(lIdx[1],VT_I4,PARAMFLAG_FIN);
  AddFunc(lInfo,5,3,INVOKE_FUNC,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx[0..1],['Mix','First','Second']);
  SetParam(lIdx[0],VT_VARIANT,PARAMFLAG_FIN or PARAMFLAG_FOPT);
  AddFunc(lInfo,6,4,INVOKE_FUNC,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx[0..0],['Opt','V']);
  SetParam(lIdx[0],VT_BOOL,PARAMFLAG_FIN);
  SetDefault(lIdx[0],2,True);
  AddFunc(lInfo,7,5,INVOKE_FUNC,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx[0..0],['Flags','B']);
  SetParam(lIdx[0],VT_I4,PARAMFLAG_FIN);
  SetParam(lIdx[1],VT_I4,PARAMFLAG_FIN);
  SetDefault(lIdx[1],0,2);
  SetParam(lIdx[2],VT_PTR,PARAMFLAG_FOUT or PARAMFLAG_FRETVAL);
  AddFunc(lInfo,8,6,INVOKE_PROPERTYGET,FUNC_PUREVIRTUAL,VT_HRESULT,lIdx,['Scaled','Row','Factor','Value']);
  OleCheck(lInfo.LayOut);

  OleCheck(lLib.CreateTypeInfo('IIdxDisp',TKIND_DISPATCH,lInfo));
  OleCheck(lInfo.SetGuid(DispGUID));
  OleCheck(lInfo.AddRefTypeInfo(lDispatch,lRef));
  OleCheck(lInfo.AddImplType(0,lRef));
  SetParam(lIdx[0],VT_I4,PARAMFLAG_FIN);
  SetParam(lIdx[1],VT_I4,PARAMFLAG_FIN);
  AddFunc(lInfo,0,0,INVOKE_PROPERTYGET,FUNC_DISPATCH,VT_BSTR,lIdx[0..1],['Cell','Row','Col']);
  SetParam(lIdx[2],VT_BSTR,PARAMFLAG_FIN);
  AddFunc(lInfo,1,0,INVOKE_PROPERTYPUT,FUNC_DISPATCH,VT_VOID,lIdx,['Cell','Row','Col']);
  OleCheck(lInfo.LayOut);
  lInfo:=nil;
  OleCheck(lLib.SaveAllChanges);
end;


// Report a failure when aExpected does not occur in aSource.
procedure CheckContains(const aMsg, aExpected, aSource: AnsiString);

begin
  if Pos(aExpected,aSource)=0 then
    begin
    Writeln('FAIL: ',aMsg,': missing "',aExpected,'"');
    Inc(Failures);
    end;
end;


var
  lFileName: WideString;
  lSource: AnsiString;
  lImporter: TTypeLibImporter;

begin
  CoInitialize(nil);
  lFileName:=IncludeTrailingPathDelimiter(GetTempDir(False))+'ttypelibidxprop.tlb';
  CreateLib(lFileName);
  lImporter:=TTypeLibImporter.Create(nil);
  try
    lImporter.InputFileName:=lFileName;
    lImporter.Execute;
    lSource:=lImporter.UnitSource.Text;
  finally
    lImporter.Free;
  end;
  SysUtils.DeleteFile(lFileName);
  if ParamStr(1)<>'' then
    with TStringList.Create do
      try
        Text:=lSource;
        SaveToFile(ParamStr(1));
      finally
        Free;
      end;
  CheckContains('Dual getter','  function Get_Item(Row:Integer; Col:Integer) : WideString; Safecall;',lSource);
  CheckContains('Dual setter','  procedure Set_Item(Row:Integer; Col:Integer; const parItem:WideString); safecall;',lSource);
  CheckContains('Dual property','  property Item[Row:Integer; Col:Integer]: WideString read Get_Item write Set_Item; default;',lSource);
  CheckContains('Dual non-indexed setter','  procedure Set_Caption(const ',lSource);
  CheckContains('Dual non-indexed property','  property Caption: WideString read Get_Caption write Set_Caption;',lSource);
  CheckContains('Integer and string defaults','  function Format(Src:WideString;Count:Integer = 7;Sep:WideString = ''a''''b''): WideString;safecall;',lSource);
  CheckContains('No default before a required parameter','  procedure Mix(First:Integer;Second:Integer);safecall;',lSource);
  CheckContains('No default for an optional Variant','  procedure Opt(V:OleVariant);safecall;',lSource);
  CheckContains('Boolean default','  procedure Flags(B:WordBool = True);safecall;',lSource);
  CheckContains('Getter index default','  function Get_Scaled(Row:Integer; Factor:Integer = 2) : WideString; Safecall;',lSource);
  CheckContains('Property without default','  property Scaled[Row:Integer; Factor:Integer]: WideString read Get_Scaled;',lSource);
  CheckContains('No defaults in a dispinterface','  function Format(Src:WideString;Count:Integer;Sep:WideString): WideString;dispid 2;',lSource);
  CheckContains('Dispinterface property','  property Cell[Row:Integer; Col:Integer]: WideString dispid 0; default;',lSource);
  CoUninitialize;
  if Failures=0 then
    Writeln('PASS');
  Halt(Failures);
end.
