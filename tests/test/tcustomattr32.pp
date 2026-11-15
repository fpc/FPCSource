program tcustomattr32;

{ in mode objfpc: an attribute class without a constructor of its own,
  TCustomAttribute itself, and a class whose ancestor declares only an
  overloaded constructor with arguments, used without arguments }

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  BareAttribute = class(TCustomAttribute);

  TBaseAttr = class(TCustomAttribute)
    constructor Create(x: Integer); overload;
  end;

  DerivedAttribute = class(TBaseAttr);

  [Bare]
  TFirst = class
  end;

  [TCustomAttribute]
  TSecond = class
  end;

  [Derived]
  TThird = class
  end;

constructor TBaseAttr.Create(x: Integer);
begin
  inherited Create;
end;

procedure Check(aInfo: Pointer; aClass: TClass; aError: Integer);
var
  lTable: PAttributeTable;
  lAttr: TCustomAttribute;
begin
  lTable := GetAttributeTable(aInfo);
  if not Assigned(lTable) then
    Halt(aError);
  if lTable^.AttributeCount <> 1 then
    Halt(aError + 1);
  lAttr := GetAttribute(lTable, 0);
  if not Assigned(lAttr) then
    Halt(aError + 2);
  if lAttr.ClassType <> aClass then
    Halt(aError + 3);
  lAttr.Free;
end;

begin
  Check(TFirst.ClassInfo, BareAttribute, 1);
  Check(TSecond.ClassInfo, TCustomAttribute, 5);
  Check(TThird.ClassInfo, DerivedAttribute, 9);
  Writeln('ok');
end.
