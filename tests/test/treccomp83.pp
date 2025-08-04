program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    class function Foo: Integer;static;
  end;

class function TChildRec.Foo: Integer;
begin
  Result:=42;
end;

type
  TComposed = record
    contains TChildRec;
  end;

begin
  if TChildRec.Foo<>42 then
    Halt(1);
  WriteLn('Ok');
end.
