{ %FAIL }
{ %OPT=-Sew }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TTest = record
    a, b: LongInt;
    procedure Method1(aArg: LongInt);
  end;

  TTest1 = record
    c, d: LongInt;
    procedure Method1(aArg: String);
  end;

  TTest2 = record
    contains f1: TTest;
    contains f2: TTest1;
  end;

procedure TTest.Method1(aArg: LongInt);
begin
end;

procedure TTest1.Method1(aArg: String);
begin
end;

var
  a: TTest2;
begin
  a.Method1(42); // ok
  a.Method1('Hello World'); // error
end.
