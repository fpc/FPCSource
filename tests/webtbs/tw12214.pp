{$mode objfpc}
program test_raise;

uses Math;

var
  X, Y: double;
  I: Integer;
  SomeFloat, SomeOtherFloat: Double;
begin
  X := -10.0;

  SomeFloat := 1.0; { any value }
  SomeOtherFloat := 1.0; { any value }

  for I := 0 to 20 do
  begin
    { This line does *any* valid float operation, just to show that
      floating-point exception (that should be raised, catched and silenced
      by try..except below) somehow arrived here. }
    SomeFloat := I * SomeOtherFloat;

    try
      { Any invalid fp operation. Tested on Sqrt(-10.0), Ln(-10.0).
        I use variable X to trick FPC into calculating this at run-time,
        otherwise "Error: Illegal constant passed to internal math function". }
      Y := Sqrt(X);      
      ClearExceptions(false);
    except
      Writeln('silenced exception');
      { Here I silence eventual exception raised by ClearExceptions.
        (Yes, I could just do ClearExceptions(false) do achieve the same,
        but imagine that this is embedded in some complicated code
        where I really want to raise exception to jump outside
        in case of problems.) }
    end;
  end;
end.
