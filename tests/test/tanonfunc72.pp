program tanonfunc72;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
{$modeswitch typehelpers}

type
  TTest = type helper for LongInt
    procedure Test;
  end;

  TProc = reference to procedure;

procedure TTest.Test;

  procedure Foobar(aArg: TProc);
  begin
    aArg();
  end;

begin
  Writeln('Before Nested: ', Self);
  Foobar(procedure begin
    Writeln('Before Inc: ', Self);
    Inc(Self);
    Writeln('After Inc: ', Self);
  end);
  Writeln('After Nested: ', Self);
end;

var
  l: LongInt;
begin
  l := 42;
  l.Test;
  Writeln('After Test: ', l);
  if l <> 43 then
    Halt(1);
end.
