{ Source provided for Free Pascal Bug Report 3255 }
{ Submitted by "Patrick Dietrich" on  2004-08-17 }
{ e-mail: patrick.dietrich@informatik.uni-ulm.de }
program testclassptr;

{$mode delphi}

type
  TProc = procedure (Sender: TClass) of object;

  TTest = class
  public
    class procedure foo;
    class procedure bar(Sender: TClass);
    class procedure baz(proc: TProc);
  end;

class procedure TTest.foo;
begin
  baz(bar);
end;

class procedure TTest.bar;
begin
  writeln('hello world');
end;

class procedure TTest.baz;
begin
  proc(self);
end;

begin
  TTest.foo;
end.
