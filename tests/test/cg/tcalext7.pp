{ %CPU=x86_64 }
{ %TARGET=linux }

program tcalext7;

{$mode objfpc}{$H+}
{$L tcext7.o}

type
  TTest = record
    a, b: Int64;
    case Boolean of
      True: (c, d: Int64);
      { to enforce 16-Byte alignment }
      False: (e: Extended);
  end;

function TestFunc(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7: Int64; aArg8: TTest; aArg9: Int64; aArg10: TTest): LongInt; cdecl; external name 'TestFunc';

function MakeTest(aArg1, aArg2, aArg3, aArg4: Int64): TTest;
begin
  Result.a := aArg1;
  Result.b := aArg2;
  Result.c := aArg3;
  Result.d := aArg4;
end;

procedure Test(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7: Int64; aArg8: TTest; aArg9: Int64; aArg10: TTest);
begin
  if aArg1 <> 1 then
    Halt(1);
  if aArg2 <> 2 then
    Halt(2);
  if aArg3 <> 3 then
    Halt(3);
  if aArg4 <> 4 then
    Halt(4);
  if aArg5 <> 5 then
    Halt(5);
  if aArg6 <> 6 then
    Halt(6);
  if aArg7 <> 7 then
    Halt(7);
  if aArg8.a <> 801 then
    Halt(801);
  if aArg8.b <> 802 then
    Halt(802);
  if aArg8.c <> 803 then
    Halt(803);
  if aArg8.d <> 804 then
    Halt(804);
  if aArg9 <> 9 then
    Halt(9);
  if aArg10.a <> 1001 then
    Halt(1001);
  if aArg10.b <> 1002 then
    Halt(1002);
  if aArg10.c <> 1003 then
    Halt(1003);
  if aArg10.d <> 1004 then
    Halt(1004);
end;

var
  res: LongInt;
begin
  Test(1, 2, 3, 4, 5, 6, 7, MakeTest(801, 802, 803, 804), 9, MakeTest(1001, 1002, 1003, 1004));
  res := TestFunc(1, 2, 3, 4, 5, 6, 7, MakeTest(801, 802, 803, 804), 9, MakeTest(1001, 1002, 1003, 1004));
  if res <> 0 then
    Halt(res + 10000);
end.
