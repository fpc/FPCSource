program tgeneric25;

{$mode objfpc}{$H+}

type
  generic TArr<T> = array[0..2] of T;
  generic TDynamicArr<T> = array of T;
  generic TRecArr<T> = array[0..1] of record
    A: T;
    B: String;
  end;

var
  ArrInt: specialize TArr<Integer>;
  ArrStr: specialize TArr<String>;
  DynArrInt: specialize TDynamicArr<Integer>;
  RecArr: specialize TRecArr<Integer>;
begin
  ArrInt[0] := 1;
  ArrStr[0] := '1';
  SetLength(DynArrInt, 1);
  DynArrInt[0] := 2;
  RecArr[0].A := 3;
  RecArr[0].B := '3';
  if ArrInt[0] <> 1 then
    halt(1);
  if ArrStr[0] <> '1' then
    halt(2);
  if DynArrInt[0] <> 2 then
    halt(3);
  if RecArr[0].A <> 3 then
    halt(4);
end.

