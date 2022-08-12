program tw39858;

{$mode objfpc}{$H+}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

type
  TProc = reference to procedure;

procedure Meth1(proc: TProc);
begin
  Writeln('Enter Meth1');
  proc();
  Writeln('Exit Meth1');
end;

procedure Meth2(proc: TProc);
begin
  Writeln('Enter Meth2');
  proc();
  Writeln('Exit Meth2');
end;

procedure Test;
var
  z: integer;
begin
  z := 0;
  Meth1(procedure begin
    Writeln('Enter Anon1');
    z:= 42;                               // this captured assigment causes the problem
    Meth2(procedure begin
      Writeln('Anon2');
    end);
    Writeln('Exit Anon1');
  end);
  if z <> 42 then
    Halt(1);
end;

begin
  Test;
end.

