{$ifdef fpc}{$mode objfpc}{$endif}

uses
  sysutils;

const
  fname = 'Makefile';

var
  fn : string;
begin
  fn:=FileSearch(fname,';');
  writeln('found: ',fn);
  if fn<>fname then
   halt(1);
  fn:=FileSearch('./'+fname,';');
  writeln('found: ',fn);
  if fn<>'./'+fname then
   halt(1);
end.
