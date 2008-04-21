{ %needlibrary }

program loader;
{$mode objfpc}{$H+}

uses
  dynlibs;
var
  h: TLibHandle;
const
{$ifdef unix}
{$ifdef darwin}
libname = './libtw6822a.dylib';
{$else darwin}
libname = './libtw6822a.so';
{$endif darwin}
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
