{ Source provided for Free Pascal Bug Report 3435 }
{ Submitted by "Michalis Kamburelis" on  2004-12-06 }
{ e-mail: michalis@camelot.homedns.org }
{$mode objfpc}

type
  TSomeLongLongLongTypeName = 0..1;

  TSomeClass = class
  public
    constructor Create(
      Param1 : TSomeLongLongLongTypeName;
      Param2 : TSomeLongLongLongTypeName;
      Param3 : TSomeLongLongLongTypeName;
      Param4 : TSomeLongLongLongTypeName;
      Param5 : TSomeLongLongLongTypeName;
      Param6 : TSomeLongLongLongTypeName;
      Param7 : TSomeLongLongLongTypeName;
      Param8 : TSomeLongLongLongTypeName;
      Param9 : TSomeLongLongLongTypeName;
      Param10: TSomeLongLongLongTypeName;
      Param11: TSomeLongLongLongTypeName;
      Param12: TSomeLongLongLongTypeName;
      Param13: TSomeLongLongLongTypeName;
      Param14: TSomeLongLongLongTypeName;
      Param15: TSomeLongLongLongTypeName;
      Param16: TSomeLongLongLongTypeName;
      Param17: TSomeLongLongLongTypeName;
      Param18: TSomeLongLongLongTypeName;
      Param19: TSomeLongLongLongTypeName);
    constructor Create(
      Param1 : TSomeLongLongLongTypeName;
      Param2 : TSomeLongLongLongTypeName;
      Param3 : TSomeLongLongLongTypeName;
      Param4 : TSomeLongLongLongTypeName;
      Param5 : TSomeLongLongLongTypeName;
      Param6 : TSomeLongLongLongTypeName;
      Param7 : TSomeLongLongLongTypeName;
      Param8 : TSomeLongLongLongTypeName;
      Param9 : TSomeLongLongLongTypeName;
      Param10: TSomeLongLongLongTypeName;
      Param11: TSomeLongLongLongTypeName;
      Param12: TSomeLongLongLongTypeName;
      Param13: TSomeLongLongLongTypeName;
      Param14: TSomeLongLongLongTypeName;
      Param15: TSomeLongLongLongTypeName;
      Param16: TSomeLongLongLongTypeName;
      Param17: TSomeLongLongLongTypeName;
      Param18: TSomeLongLongLongTypeName;
      Param19: TSomeLongLongLongTypeName;
      Param20: TSomeLongLongLongTypeName);
  end;

constructor TSomeClass.Create(
  Param1 : TSomeLongLongLongTypeName;
  Param2 : TSomeLongLongLongTypeName;
  Param3 : TSomeLongLongLongTypeName;
  Param4 : TSomeLongLongLongTypeName;
  Param5 : TSomeLongLongLongTypeName;
  Param6 : TSomeLongLongLongTypeName;
  Param7 : TSomeLongLongLongTypeName;
  Param8 : TSomeLongLongLongTypeName;
  Param9 : TSomeLongLongLongTypeName;
  Param10: TSomeLongLongLongTypeName;
  Param11: TSomeLongLongLongTypeName;
  Param12: TSomeLongLongLongTypeName;
  Param13: TSomeLongLongLongTypeName;
  Param14: TSomeLongLongLongTypeName;
  Param15: TSomeLongLongLongTypeName;
  Param16: TSomeLongLongLongTypeName;
  Param17: TSomeLongLongLongTypeName;
  Param18: TSomeLongLongLongTypeName;
  Param19: TSomeLongLongLongTypeName);

  procedure Nested(
  Param1 : TSomeLongLongLongTypeName;
  Param2 : TSomeLongLongLongTypeName;
  Param3 : TSomeLongLongLongTypeName;
  Param4 : TSomeLongLongLongTypeName;
  Param5 : TSomeLongLongLongTypeName;
  Param6 : TSomeLongLongLongTypeName;
  Param7 : TSomeLongLongLongTypeName;
  Param8 : TSomeLongLongLongTypeName;
  Param9 : TSomeLongLongLongTypeName;
  Param10: TSomeLongLongLongTypeName;
  Param11: TSomeLongLongLongTypeName;
  Param12: TSomeLongLongLongTypeName;
  Param13: TSomeLongLongLongTypeName;
  Param14: TSomeLongLongLongTypeName;
  Param15: TSomeLongLongLongTypeName;
  Param16: TSomeLongLongLongTypeName;
  Param17: TSomeLongLongLongTypeName;
  Param18: TSomeLongLongLongTypeName;
  Param19: TSomeLongLongLongTypeName;
  Param20: TSomeLongLongLongTypeName);
  begin
  end;

const
  SomeConstant1: array[0..1]of Integer = (0, 1);
  SomeConstant2: array[0..1]of Integer = (0, 1);
begin
end;

constructor TSomeClass.Create(
  Param1 : TSomeLongLongLongTypeName;
  Param2 : TSomeLongLongLongTypeName;
  Param3 : TSomeLongLongLongTypeName;
  Param4 : TSomeLongLongLongTypeName;
  Param5 : TSomeLongLongLongTypeName;
  Param6 : TSomeLongLongLongTypeName;
  Param7 : TSomeLongLongLongTypeName;
  Param8 : TSomeLongLongLongTypeName;
  Param9 : TSomeLongLongLongTypeName;
  Param10: TSomeLongLongLongTypeName;
  Param11: TSomeLongLongLongTypeName;
  Param12: TSomeLongLongLongTypeName;
  Param13: TSomeLongLongLongTypeName;
  Param14: TSomeLongLongLongTypeName;
  Param15: TSomeLongLongLongTypeName;
  Param16: TSomeLongLongLongTypeName;
  Param17: TSomeLongLongLongTypeName;
  Param18: TSomeLongLongLongTypeName;
  Param19: TSomeLongLongLongTypeName;
  Param20: TSomeLongLongLongTypeName);
const
  SomeConstant1: array[0..1]of Integer = (0, 1);
  SomeConstant2: array[0..1]of Integer = (0, 1);

  procedure Nested(
  Param1 : TSomeLongLongLongTypeName;
  Param2 : TSomeLongLongLongTypeName;
  Param3 : TSomeLongLongLongTypeName;
  Param4 : TSomeLongLongLongTypeName;
  Param5 : TSomeLongLongLongTypeName;
  Param6 : TSomeLongLongLongTypeName;
  Param7 : TSomeLongLongLongTypeName;
  Param8 : TSomeLongLongLongTypeName;
  Param9 : TSomeLongLongLongTypeName;
  Param10: TSomeLongLongLongTypeName;
  Param11: TSomeLongLongLongTypeName;
  Param12: TSomeLongLongLongTypeName;
  Param13: TSomeLongLongLongTypeName;
  Param14: TSomeLongLongLongTypeName;
  Param15: TSomeLongLongLongTypeName;
  Param16: TSomeLongLongLongTypeName;
  Param17: TSomeLongLongLongTypeName;
  Param18: TSomeLongLongLongTypeName;
  Param19: TSomeLongLongLongTypeName;
  Param20: TSomeLongLongLongTypeName);
  begin
  end;

begin
end;

begin
end.
