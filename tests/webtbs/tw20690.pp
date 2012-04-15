{%norun}
program tw20690;
{$MODE delphi}

type
  TTestClass = class
  strict private
    type TLocalInteger = Integer;
  public
    procedure WriteInteger(value: TLocalInteger);
  end;

procedure TTestClass.WriteInteger(value: TLocalInteger);

  procedure _InacessibleInDeclaration1(i: TLocalInteger); begin end;
    { The compiler will report that TLocalInteger is not found }

  procedure _InacessibleInDeclaration2(i: TTestClass.TLocalInteger); begin end;
    { The compiler will report the same error even if the fully qualified type
      identifier is used }

  function _InacessibleInDeclaration3(i: TLocalInteger):TLocalInteger; begin end;

  procedure _AccessibleInImplementation;
  begin
    Writeln(SizeOf(TLocalInteger));  { But this will work }
  end;

begin
  Writeln(value);
end;

begin
  with TTestClass.Create do begin
    WriteInteger(123);
    Free;
  end;
end.
