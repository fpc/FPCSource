{$ifdef fpc}{$mode objfpc}{$endif}

uses
  sysutils;

var
  fn : string;
begin
  fn:=FileSearch('tw1479.pp',';');
  writeln('found: ',fn);
  if fn<>'tw1479.pp' then
   halt(1);
  fn:=FileSearch('.\tw1479.pp',';');
  writeln('found: ',fn);
  if fn<>'.\tw1479.pp' then
   halt(1);
end.
