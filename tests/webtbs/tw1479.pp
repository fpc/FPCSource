{$ifdef fpc}{$mode objfpc}{$endif}

uses
  sysutils;

const
  fname = 'Makefile';

 ThisDir = '.'+DirectorySeparator;
var
  fn : string;
begin
  fn:=FileSearch(fname,PathSeparator);
  writeln('found: ',fn);
  if fn<>fname then
   halt(1);
  fn:=FileSearch(ThisDir+fname,PathSeparator);
  writeln('found: ',fn);
  if fn<>ThisDir+fname then
   halt(1);
end.
