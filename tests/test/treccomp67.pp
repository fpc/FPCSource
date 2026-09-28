{ %FAIL }
{ Record composition: composing an interface is not allowed, only records }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}
{$ModeSwitch TypeHelpers}

type
  ITest = interface
  end;

  TTest1 = record
    contains ITest;
  end;

begin
end.
