program tanonfunc71;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

type
  TTest = record
    f: LongInt;
    procedure Test;
  end;

  TProc = reference to procedure;

procedure TTest.Test;

  procedure Foobar(aArg: TProc);
  begin
    aArg();
  end;

begin
  Writeln('Before Nested: ', f);
  Foobar(procedure begin
    Writeln('Before Inc: ', f);
    Inc(f);
    Writeln('After Inc: ', f);
  end);
  Writeln('After Nested: ', f);
end;

var
  t: TTest;
begin
  t.f := 42;
  t.Test;
  Writeln('After Test: ', t.f);
  if t.f <> 43 then
    Halt(1);
end.
