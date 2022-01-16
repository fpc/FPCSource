program project1;

{$mode objfpc}

{$Inline On} // inline must be turned on for both methods
type
  TTest = object // both methods must be inside of object
    procedure ModifyValue(ValueModify: Int32); inline;
    procedure SetKey(const ValueConst: Int32); inline;
  end;

  procedure TTest.ModifyValue(ValueModify: Int32);
  begin
    ValueModify := 1;
  end;

  procedure TTest.SetKey(const ValueConst: Int32);
  var
    OriginalValue: Int32;
  begin
    OriginalValue := ValueConst;
    ModifyValue(ValueConst);
    WriteLn('Current Value: ', ValueConst); //Outputs 1
    WriteLn('Original Value: ', OriginalValue); //Outputs 2
    if (OriginalValue<>2) or
       (ValueConst<>2) then
      halt(1);
  end;

var
  TestObj: TTest;
  i: Int32;

begin
  i := 1;
  TestObj.SetKey(i + 1);
end.

