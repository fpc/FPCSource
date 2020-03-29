program tcustomattr19;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  TypInfo;

type
  TTestAttribute = class(TCustomAttribute)
    constructor Create;
    constructor Create(aArg: LongInt);
  end;

  [TTestAttribute(42), TTest]
  TMyTest = class

  end;
var
  at: PAttributeTable;

constructor TTestAttribute.Create;
begin

end;

constructor TTestAttribute.Create(aArg: LongInt);
begin

end;

begin
  at := GetAttributeTable(TMyTest.ClassInfo);
  if not Assigned(at) then
    Halt(1);
  if at^.AttributeCount <> 2 then
    Halt(2);

  Writeln('ok');
end.
