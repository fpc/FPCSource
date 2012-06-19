{ the important part of this test is a cross compilation which a change in the
  size of the bitness, e.g. from Win32 to Win64 where the unit "fgl" was
  compiled with the 32-to-64-bit cross compiler and this program itself is
  compiled with the native 64-bit compiler }

program tw20947;

uses
  fgl;

type
  TTestList = specialize TFPGList<Byte>;

Var
  Test : TTestList;
begin
  Test := TTestList.Create;
  Test.Add(2);
  WriteLn(Test[0]);  // This should output 2 to console
  Test.Free;
end.
