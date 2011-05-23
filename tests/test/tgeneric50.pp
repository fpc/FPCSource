{ %NORUN }

{ this tests that hint directives defined for a generic only apply when
  specializung a generic and that specializations may introduce their own
  directives }
program tgeneric50;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = class

  end deprecated 'Message A' platform;

  // this will print that TTest<T> is deprecated and platform dependant
  TTestInteger = TTest<Integer> deprecated 'Message B' experimental;
  TTestString = TTest<String>;

var
  // this will print that TTestInteger is deprecated and experimental
  t: TTestInteger;
  // this will print nothing
  t2: TTestString;
begin
  // this will print that TTest<T> is deprecated and platform dependant
  t2 := TTest<String>.Create;
end.
