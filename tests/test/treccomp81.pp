{ %FAIL }
{ Record composition: like a field, a composition cannot appear after a method
    declaration without starting a new visibility section }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    function Foo: Integer;
  end;

function TChildRec.Foo: Integer;
begin
  Result:=42;
end;

type
  TComposed = record
    function Foo: Integer;
    contains c: TChildRec; // needs a visibility
  end;

function TComposed.Foo: Integer;
begin
  Result:=32;
end;

var
  c: TComposed;
begin
  if c.Foo=c.c.Foo then ;
  WriteLn('Ok');
end.
