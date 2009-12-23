program TestProject;

{$mode objfpc}{$H+}

uses
  SysUtils, TypInfo;

type
  TTestObject = class(TObject)
  private
    FDate1: TDateTime;
    FDate2: TDateTime;
  published
    property Date1: TDateTime read FDate1 write FDate1;
    property Date2: TDateTime read FDate2 write FDate2;
  end;

var
  VarDate1: Variant;
  VarDate2: Variant;
  TestObject: TTestObject;

begin
  TestObject := TTestObject.Create;
  TestObject.Date1 := EncodeDate(1999, 02, 06) + EncodeTime(20, 0, 0, 0);
  TestObject.Date2 := EncodeDate(1999, 02, 06) + EncodeTime(20, 0, 0, 1);

  // works ok
  //VarDate1 := TestObject.Date1;
  //VarDate2 := TestObject.Date2;

  // rounding occurs
  // variants are interpreted as floats
  // but this code works in delphi as expected (different when comparing)
  // so don't know is this comparisson problem or values are rounded too much.
  VarDate1 := GetPropValue(TestObject, 'Date1');
  VarDate2 := GetPropValue(TestObject, 'Date2');

  if VarDate1 = VarDate2 then
    begin
      WriteLn('Dates are equal.');
      halt(1);
    end
  else
    WriteLn('Dates are not equal.');

  TestObject.Free;
end.

