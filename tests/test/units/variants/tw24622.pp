{ %opt=-gh }

{$mode objfpc}
{$h+}

uses
  sysutils, variants;

type
  TValueDispatch = class(TInterfacedObject, IDispatch)
  private
    FValue: Variant;
    FHasValue: Boolean;
  public
    // Creates an object whose default property returns aValue, or fails when aHasValue is false.
    constructor Create(const aValue: Variant; aHasValue: Boolean = True);
    // Reports no type information.
    function GetTypeInfoCount(out count: LongInt): HResult; stdcall;
    // Not implemented.
    function GetTypeInfo(Index, LocaleID: LongInt; out TypeInfo): HResult; stdcall;
    // Not implemented.
    function GetIDsOfNames(const iid: TGUID; names: Pointer;
      NameCount, LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;
    // Returns the value for a property get of DISPID_VALUE.
    function Invoke(DispID: LongInt; const iid: TGUID; LocaleID: LongInt;
      Flags: Word; var params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

constructor TValueDispatch.Create(const aValue: Variant; aHasValue: Boolean);

begin
  inherited Create;
  FValue := aValue;
  FHasValue := aHasValue;
end;


function TValueDispatch.GetTypeInfoCount(out count: LongInt): HResult; stdcall;

begin
  count := 0;
  Result := S_OK;
end;


function TValueDispatch.GetTypeInfo(Index, LocaleID: LongInt; out TypeInfo): HResult; stdcall;

begin
  Result := E_NOTIMPL;
end;


function TValueDispatch.GetIDsOfNames(const iid: TGUID; names: Pointer;
  NameCount, LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;

begin
  Result := E_NOTIMPL;
end;


function TValueDispatch.Invoke(DispID: LongInt; const iid: TGUID; LocaleID: LongInt;
  Flags: Word; var params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

begin
  if (DispID <> 0) or (Flags and 2 = 0) or not FHasValue then
    Exit(E_NOTIMPL);
  if Assigned(VarResult) then
    PVariant(VarResult)^ := FValue;
  Result := S_OK;
end;


procedure Check(aCondition: Boolean; const aMsg: string; aCode: Integer);

begin
  if not aCondition then
  begin
    Writeln('Failed: ', aMsg);
    Halt(aCode);
  end;
end;


procedure DoTest;

var
  lDisp: IDispatch;
  lVar, lRef: Variant;
  lStr: AnsiString;
  lWide: WideString;
  lUni: UnicodeString;
  lRaised: Boolean;
begin
  lDisp := TValueDispatch.Create('hello');
  lVar := lDisp;
  Check(VarType(lVar) = varDispatch, 'variant holds a dispatch interface', 1);
  lStr := lVar;
  Check(lStr = 'hello', 'AnsiString from dispatch default value', 2);
  lWide := lVar;
  Check(lWide = 'hello', 'WideString from dispatch default value', 3);
  lUni := lVar;
  Check(lUni = 'hello', 'UnicodeString from dispatch default value', 4);
  Check(VarToStr(lVar) = 'hello', 'VarToStr of dispatch default value', 5);
  Check(VarAsType(lVar, varString) = 'hello', 'VarAsType varString of dispatch', 6);
  Check(VarType(VarAsType(lVar, varOleStr)) = varOleStr, 'VarAsType varOleStr gives olestr', 7);

  lDisp := TValueDispatch.Create(42);
  lVar := lDisp;
  Check(Integer(lVar) = 42, 'Integer from dispatch default value', 10);
  Check(Int64(lVar) = 42, 'Int64 from dispatch default value', 11);
  Check(Double(lVar) = 42.0, 'Double from dispatch default value', 12);
  Check(VarAsType(lVar, varInteger) = 42, 'VarAsType varInteger of dispatch', 13);
  Check(String(lVar) = '42', 'String from integer default value', 14);

  TVarData(lRef).vType := varDispatch or varByRef;
  TVarData(lRef).vPointer := @TVarData(lVar).vDispatch;
  Check(Integer(lRef) = 42, 'Integer from by-reference dispatch', 20);
  TVarData(lRef).vType := varEmpty;

  lDisp := TValueDispatch.Create(Null, False);
  lVar := lDisp;
  lRaised := False;
  try
    lStr := lVar;
  except
    on EVariantError do
      lRaised := True;
  end;
  Check(lRaised, 'dispatch without default value raises EVariantError', 30);

  lVar := Unassigned;
  lDisp := nil;
end;

begin
  HaltOnNotReleased := True;
  DoTest;
  Writeln('ok');
end.
