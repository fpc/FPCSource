program tcustomattr11;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  TypInfo;

type
  TTest = class(TCustomAttribute)
    constructor Create;
  end;

  TTestAttribute = class(TCustomAttribute)
    constructor Create;
  end;

  { the attribute with the Attribute suffix is preferred }
  [TTest]
  TTestObj = class

  end;

constructor TTestAttribute.Create;
begin

end;

constructor TTest.Create;
begin

end;

var
  at: PAttributeTable;
  attr: TCustomAttribute;

begin
  at := GetAttributeTable(TypeInfo(TTestObj));
  if not Assigned(at) then
    Halt(1);
  if at^.AttributeCount <> 1 then
    Halt(2);

  attr := GetAttribute(at, 0);
  if not Assigned(attr) then
    Halt(3);
  if not (attr is TTestAttribute) then
    Halt(4);
  Writeln('ok');
end.
