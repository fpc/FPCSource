{ Source provided for Free Pascal Bug Report 3534 }
{ Submitted by "Mattias Gaertner" on  2005-01-08 }
{ e-mail: mattias@freepascal.org }
program IntToStrSmallIntBug;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  i: SmallInt;
begin
  i:=-3;
  writeln(IntToStr(i));
  if IntToStr(i)<>'-3' then
    halt(1);
end.
