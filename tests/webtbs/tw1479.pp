{$ifdef fpc}{$mode objfpc}{$endif}

uses
  sysutils;

const
  fname = 'tw1479.tmp';

 ThisDir = '.'+DirectorySeparator;
var
  fn : string;
  f : text;
begin
  assign(f,fname);
  rewrite(f);
  writeln(f,'hello');
  close(f);

  fn:=FileSearch(fname,PathSeparator);
  writeln('found: ',fn);
  if fn<>fname then
   halt(1);
  fn:=FileSearch(ThisDir+fname,PathSeparator);
  writeln('found: ',fn);
  if fn<>ThisDir+fname then
   halt(1);
  erase(f);
end.
