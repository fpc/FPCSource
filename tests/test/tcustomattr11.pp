program tcustomattr11;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  TypInfo;

type
  TTest = class(TCustomAttribute)

  end;

  TTestAttribute = class(TCustomAttribute)

  end;

  { the attribute with the Attribute suffix is preferred }
  [TTest]
  TTestObj = class

  end;

var
  ad: PAttributeData;
  attr: TCustomAttribute;
begin
  ad := GetAttributeData(TypeInfo(TTestObj));
  if not Assigned(ad) then
    Halt(1);
  if ad^.AttributeCount <> 1 then
    Halt(2);

  attr := GetAttribute(ad, 0);
  if not Assigned(attr) then
    Halt(3);
  if not (attr is TTestAttribute) then
    Halt(4);
  Writeln('ok');
end.
