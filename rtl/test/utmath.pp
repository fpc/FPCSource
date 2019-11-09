unit utmath;

interface

uses punit, utrtl;

implementation

uses math;

Function TestFMod : TTestString;

Begin
  Result:='';
end;

Begin
  AddTest('TestFMod',@TestFMod,EnsureSuite('Math'));
end.
