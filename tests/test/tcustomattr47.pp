program tcustomattr47;

{ attributes on the methods of an interface are in the method's RTTI, as
  Delphi lists them: several in a list or in several lists; a method without
  any has no table; those of the parameters stay the parameters' }

{$mode delphi}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  NameAttribute = class(TCustomAttribute)
  public
    FName: string;
    constructor Create(const aName: string);
  end;

  FlagAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  {$M+}
  IThing = interface
    ['{8E2A41C7-0B6D-4E5F-A1C3-7D9E0F1A2B47}']
    [Name('one')]
    procedure One;
    [Flag, Name('two')]
    procedure Two([Name('p')] aValue: Integer);
    [Flag] [Name('three')]
    function Three: Integer;
    procedure Plain([Flag] aValue: Integer);
  end;
  {$M-}

constructor NameAttribute.Create(const aName: string);
begin
  inherited Create;
  FName := aName;
end;

constructor FlagAttribute.Create;
begin
  inherited Create;
end;

function Listing(aTable: PAttributeTable): string;
var
  lIdx: Integer;
  lAttr: TCustomAttribute;
begin
  Result := '';
  if not Assigned(aTable) then
    Exit;
  for lIdx := 0 to aTable^.AttributeCount - 1 do begin
    lAttr := GetAttribute(aTable, lIdx);
    if lAttr is NameAttribute then
      Result := Result + 'N' + NameAttribute(lAttr).FName + ' '
    else
      Result := Result + 'F ';
    lAttr.Free;
  end;
end;

var
  gMethods: PIntfMethodTable;
  gErrors: Integer = 0;

procedure Check(const aWhat, aGot, aExpected: string);
begin
  if aGot <> aExpected then begin
    Writeln('error: ', aWhat, ': "', aGot, '", expected "', aExpected, '"');
    Inc(gErrors);
  end;
end;

{ parameter 0 of an interface method is its Self }
begin
  gMethods := PInterfaceData(GetTypeData(TypeInfo(IThing)))^.MethodTable;
  if gMethods^.Count <> 4 then
    Halt(1);
  Check('One', Listing(gMethods^.Method[0]^.AttributeTable), 'None ');
  Check('Two', Listing(gMethods^.Method[1]^.AttributeTable), 'F Ntwo ');
  Check('Two''s Self', Listing(gMethods^.Method[1]^.Param[0]^.AttributeTable), '');
  Check('Two''s aValue', Listing(gMethods^.Method[1]^.Param[1]^.AttributeTable), 'Np ');
  Check('Three', Listing(gMethods^.Method[2]^.AttributeTable), 'F Nthree ');
  Check('Three''s name', gMethods^.Method[2]^.Name, 'Three');
  if Assigned(gMethods^.Method[3]^.AttributeTable) then
    Check('Plain', 'a table', 'none');
  Check('Plain''s aValue', Listing(gMethods^.Method[3]^.Param[1]^.AttributeTable), 'F ');
  Check('Plain''s name', gMethods^.Method[3]^.Name, 'Plain');
  if gErrors <> 0 then
    Halt(1);
  Writeln('ok');
end.
