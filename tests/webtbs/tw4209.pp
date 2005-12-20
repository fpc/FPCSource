{ Source provided for Free Pascal Bug Report 4209 }
{ Submitted by "Ivo Steinmann" on  2005-07-22 }
{ e-mail: isteinmann@bluewin.ch }
Program testprog;

{$mode delphi}

var
  err : boolean;

type
  XMethod = procedure of object;
  XProcedure = procedure;

procedure Test(const Callback: XMethod); overload;
begin
end;

procedure Test(const Callback: XProcedure); overload;
begin
  writeln('ok');
  err:=false;
end;

procedure Foobar;
begin
end;

begin
  err:=true;
  Test(Foobar);
  if err then
    halt(1);
end.
