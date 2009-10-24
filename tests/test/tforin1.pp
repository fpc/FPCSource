program tforin1;

{$mode objfpc}{$H+}
{$apptype console}
uses
  Classes, typinfo;

type
  TColor = (clRed, clGreen, clBlue);

procedure LoopTypes;
type
  IntRange = 0..9;
var
  c: TColor;
  i: integer;
begin
  // check loop in range
  for i in IntRange do
    writeln(i);

  // check loop in enum
  for c in TColor do
    writeln(GetEnumName(TypeInfo(c), ord(c)));
end;

procedure LoopString(s: string);
var
  c: char;
begin
  // check loop in string
  for c in s do
    write(c);
  WriteLn;
end;

procedure LoopArray(a: array of integer);
var
  i: integer;
begin
  // check loop in array
  for i in a do
    write(i);
  WriteLn;
end;

procedure LoopSet;
const
  s = [clRed, clBlue];
var
  c: TColor;
begin
  // check loop in set
  for c in s do
    writeln(GetEnumName(TypeInfo(c), ord(c)));
end;

var
  a: array[0..2] of integer;
begin
  LoopTypes;
  LoopString('test');
  a[0]:=0;
  a[1]:=1;
  a[2]:=2;
  LoopArray(a);
  LoopSet;
end.

