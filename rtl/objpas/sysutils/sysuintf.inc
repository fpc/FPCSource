{
    *********************************************************************
    Copyright (C) 2002 Peter Vreman,
    member of the Free Pascal Development Team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    *********************************************************************
}

function Supports(const Instance: IInterface; const AClass: TClass; out Obj): Boolean;
begin
  Result := (Instance<>nil) and (Instance.QueryInterface(IObjectInstance,Obj)=S_OK) and (TObject(Obj).InheritsFrom(AClass));
end;

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.QueryInterface(IID,Intf)=S_OK);
end;

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  Temp: Pointer; // weak
begin
  Result:=(Instance<>nil) and ((Instance.GetInterfaceWeak(IInterface,Temp) and (IInterface(Temp).QueryInterface(IID,Intf)=S_OK))
    or Instance.GetInterface(IID,Intf));
  { Some applications expect that the QueryInterface method is invoked as first priority
    to query for an interface and GetInterface as 2nd priority }
end;

function Supports(const Instance: TObject; const IID: Shortstring; out Intf): Boolean;
begin
  Result:=(Instance<>nil) and Instance.GetInterface(IID,Intf);
end;



function Supports(const Instance: IInterface; const AClass: TClass): Boolean;
var
  Temp: TObject;
begin
  Result:=Supports(Instance,AClass,Temp);
end;

function Supports(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result:=Supports(Instance,IID,Temp);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result:=Supports(Instance,IID,Temp);
end;

function Supports(const Instance: TObject; const IID: Shortstring): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.GetInterfaceEntryByStr(IID)<>nil);
end;



function Supports(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result:=(AClass<>nil) and (AClass.GetInterfaceEntry(IID)<>nil);
end;

function Supports(const AClass: TClass; const IID: Shortstring): Boolean;
begin
  Result:=(AClass<>nil) and (AClass.GetInterfaceEntryByStr(IID)<>nil);
end;



function StringToGUID(const S: string): TGUID;
begin
  if not TryStringToGUID(S, Result) then
    raise EConvertError.CreateFmt(SInvalidGUID, [S]);
end;

function TryStringToGUID(const S: string; out Guid: TGUID): Boolean;
var
  e: Boolean;
  p: PChar;

  function rb: Byte;
  begin
    case p^ of
      '0'..'9': Result := Byte(p^) - Byte('0');
      'a'..'f': Result := Byte(p^) - Byte('a') + 10;
      'A'..'F': Result := Byte(p^) - Byte('A') + 10;
      else e := False;
    end;
    Inc(p);
  end;

  procedure nextChar(c: Char); inline;
  begin
    if p^ <> c then
      e := False;
    Inc(p);
  end;

begin
  if Length(S)<>38 then Exit(False);
  e := True;
  p := PChar(S);
  nextChar('{');
  Guid.D1 := rb shl 28 or rb shl 24 or rb shl 20 or rb shl 16 or rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D2 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D3 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D4[0] := rb shl 4 or rb;
  Guid.D4[1] := rb shl 4 or rb;
  nextChar('-');
  Guid.D4[2] := rb shl 4 or rb;
  Guid.D4[3] := rb shl 4 or rb;
  Guid.D4[4] := rb shl 4 or rb;
  Guid.D4[5] := rb shl 4 or rb;
  Guid.D4[6] := rb shl 4 or rb;
  Guid.D4[7] := rb shl 4 or rb;
  nextChar('}');
  Result := e;
end;

function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a1,a2: PIntegerArray;
begin
  a1:=PIntegerArray(@guid1);
  a2:=PIntegerArray(@guid2);
  Result:=(a1^[0]=a2^[0]) and
          (a1^[1]=a2^[1]) and
          (a1^[2]=a2^[2]) and
          (a1^[3]=a2^[3]);
end;

function GuidCase(const GUID: TGUID; const List: array of TGuid): Integer;
begin
  for Result := High(List) downto 0 do
    if IsEqualGUID(GUID, List[Result]) then
      Exit;
  Result := -1;
end;

function GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [
     GUID.D1, GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
end;

