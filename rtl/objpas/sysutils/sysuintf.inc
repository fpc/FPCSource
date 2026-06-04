{%MainUnit sysutils.pp}
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

begin
  Result := TGUID.FromString(S,GUID);
end;

function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
begin
  Result:=guid1=guid2;
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
  Result := GUID.AsString;
end;

