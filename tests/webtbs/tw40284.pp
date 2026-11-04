{ %TARGET = win32,win64,wince }

program tw40284;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, ActiveX, ComObj, Variants;

type
  { IDispatch that records the flags and value type of the last Invoke }
  TRecorder = class(TInterfacedObject, IDispatch)
  public
    // Number of type information interfaces: none.
    function GetTypeInfoCount(out aCount: longint): HResult; stdcall;
    // Type information: not implemented.
    function GetTypeInfo(aIndex, aLocaleID: longint; out aTypeInfo): HResult; stdcall;
    // Maps every name to DispID 1.
    function GetIDsOfNames(const aIID: TGUID; aNames: Pointer;
      aNameCount, aLocaleID: LongInt; aDispIDs: Pointer): HResult; stdcall;
    // Records the flags and the type of the first argument.
    function Invoke(aDispID: LongInt; const aIID: TGUID; aLocaleID: longint;
      aFlags: Word; var aParams; aVarResult, aExcepInfo, aArgErr: pointer): HResult; stdcall;
  end;

var
  LastFlags: Word;
  LastType: TVarType;
  Errors: Integer = 0;


function TRecorder.GetTypeInfoCount(out aCount: longint): HResult; stdcall;

begin
  aCount:=0;
  Result:=S_OK;
end;


function TRecorder.GetTypeInfo(aIndex, aLocaleID: longint; out aTypeInfo): HResult; stdcall;

begin
  Pointer(aTypeInfo):=nil;
  Result:=E_NOTIMPL;
end;


function TRecorder.GetIDsOfNames(const aIID: TGUID; aNames: Pointer;
  aNameCount, aLocaleID: LongInt; aDispIDs: Pointer): HResult; stdcall;

var
  i: Integer;

begin
  for i:=0 to aNameCount-1 do
    PDispIDList(aDispIDs)^[i]:=i+1;
  Result:=S_OK;
end;


function TRecorder.Invoke(aDispID: LongInt; const aIID: TGUID; aLocaleID: longint;
  aFlags: Word; var aParams; aVarResult, aExcepInfo, aArgErr: pointer): HResult; stdcall;

var
  lArg: PVarData;

begin
  LastFlags:=aFlags;
  LastType:=varEmpty;
  if TDispParams(aParams).cArgs>0 then
    begin
    lArg:=@TDispParams(aParams).rgvarg^[0];
    if (lArg^.VType and varByRef)<>0 then
      LastType:=PVarData(lArg^.VPointer)^.VType
    else
      LastType:=lArg^.VType;
    end;
  if ((aFlags and DISPATCH_PROPERTYPUTREF)<>0)
     and not ((LastType and varTypeMask) in [varDispatch,varUnknown]) then
    Result:=DISP_E_MEMBERNOTFOUND
  else
    Result:=S_OK;
end;


// Reports an error when the last put used other flags than aExpected.
procedure Check(const aName: string; aExpected: Word);

begin
  if LastFlags<>aExpected then
    begin
    Writeln(aName,': flags ',LastFlags,', expected ',aExpected,' (value type ',LastType,')');
    Inc(Errors);
    end;
end;


// Assigns several kinds of variants to a late-bound property.
procedure TestPut;

var
  lTarget, lArr, lInt, lStr, lObj: Variant;

begin
  lTarget:=IDispatch(TRecorder.Create);
  lArr:=VarArrayCreate([0,1,0,1],varVariant);
  lArr[0,0]:=1;
  lArr[1,1]:='a';
  lInt:=42;
  lStr:='text';
  lObj:=IDispatch(TRecorder.Create);
  try
    lTarget.Value:=lArr;
    Check('array variable',DISPATCH_PROPERTYPUT);
    lTarget.Value:=lInt;
    Check('integer variable',DISPATCH_PROPERTYPUT);
    lTarget.Value:=lStr;
    Check('string variable',DISPATCH_PROPERTYPUT);
    lTarget.Value:=VarArrayOf([1,2]);
    Check('array expression',DISPATCH_PROPERTYPUT);
    lTarget.Value:=lObj;
    Check('object variable',DISPATCH_PROPERTYPUT or DISPATCH_PROPERTYPUTREF);
  except
    on E: Exception do
      begin
      Writeln('Exception ',E.ClassName,': ',E.Message,' (last flags ',LastFlags,', value type ',LastType,')');
      Inc(Errors);
      end;
  end;
end;


begin
  TestPut;
  if Errors>0 then
    Halt(1);
  Writeln('ok');
end.
