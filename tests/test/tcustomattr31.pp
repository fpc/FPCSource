program tcustomattr31;

{ an attribute class that declares no constructor can be used without
  arguments, with or without parentheses (based on MR !1728's test) }

{$mode delphi}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  BareAttribute = class(TCustomAttribute);
  DerivedAttribute = class(BareAttribute);

  [Bare]
  TFirst = class
  end;

  [Bare()]
  TSecond = class
  end;

  [Derived]
  TThird = class
  end;

  TMembers = class
  public
    [Bare]
    Field: Integer;
    [Bare()]
    procedure Method;
  end;

procedure TMembers.Method;
begin
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
  Check(TSecond.ClassInfo, BareAttribute, 5);
  Check(TThird.ClassInfo, DerivedAttribute, 9);
  Writeln('ok');
end.
