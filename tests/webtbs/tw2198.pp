{ Source provided for Free Pascal Bug Report 2198 }
{ Submitted by "Sebastian Günther" on  2002-10-23 }
{ e-mail: sg@freepascal.org }

{$mode objfpc}

type
  TTest = class
    procedure x;
    procedure x(i: Integer);
  end;

procedure TTest.x;
const s = 'Test1';
begin
  writeln(s);
end;

procedure TTest.x(i: Integer);
const s = 'Test2';
begin
  writeln(s);
end;

var
  t : ttest;
begin
  t:=ttest.create;
  t.x;
  t.x(1);
  t.free;
end.
