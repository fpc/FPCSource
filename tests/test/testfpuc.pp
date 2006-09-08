{ %target=linux,freebsd,darwin,solaris }

{$mode objfpc}
program testr;

uses sysutils;
function sprintf(buf,fmt : pchar) : longint; varargs;cdecl; external 'c';
function cosh(f : double) : double; cdecl; external 'm';
function ccos(f : double) : double; cdecl; external 'm' name 'cos';

Type
  TTest = Class(TObject)
    Constructor Create(Max : Integer);
  end;

Constructor TTest.Create(Max : Integer);

Var
  I,J : integer;
  F : single;
  buf : ansistring;

begin
  For I:=1 to Max do
    begin
    F:=ccos(I/180*pi);
    setlength(buf,100);
    setlength(buf,sprintf(pchar(buf),'%d: f: %f and round f*10: ',i,f));
    f:=f*10.0;
    f:=cosh(f);
    writeln(buf,round(F));
    end;
end;

procedure DoTest;

begin
  With TTest.Create(720) do
    Free;
end;

begin
  DoTest;
end.
