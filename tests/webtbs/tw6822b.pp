{ %needlibrary }
{ %delfiles=tw6822a }

program loader;
{$mode objfpc}{$H+}

uses
  popuperr,
  dynlibs;
var
  h: TLibHandle;
const
{$ifdef unix}
libname = './libtw6822a.'+SharedSuffix;
{$endif unix}

{$ifdef windows}
libname = 'tw6822a.dll';
{$endif windows}

begin
  writeln('hello from loader program');
  h:= loadlibrary(libname);
  if h = nilhandle then
  begin
    write('could not load library');
    exit;
  end;
  freelibrary(h);
end.
