{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 3669 }
{ Submitted by "Mattias Gaertner" on  2005-02-19 }
{ e-mail: mattias@freepascal.org }
program UninitializedArrayBounds;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  a: array[1..2] of integer;
  i: Integer;
begin
  for i:=Low(a) to High(a) do a[i]:=0;
end.
