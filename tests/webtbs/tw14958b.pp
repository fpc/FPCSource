{ %target=linux }
{ %needlibrary }
{ %result=182 }
program loadlib;

{$mode objfpc}{$H+}

uses
  dl,dynlibs;

var
  p: Pointer;
  s: Longint;
begin
  Writeln('Opening ', ParamStr(1));
  p := dlopen('./libtw14958a.so', RTLD_LAZY);
  if Assigned(p) then
  begin
    Writeln('OK. Now closing.');
    s := dlclose(p);
    Writeln('After close.');
    if s = 0 then
      begin
        Writeln('Close OK.');
        halt(182);
      end
    else
      Writeln('Failed close. Status: ', s);
  end
  else
    begin
      Writeln('Failed open.');
      halt(1);
    end;
end.

