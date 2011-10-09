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

  // these will both print that TTest<T> is deprecated and platform
  TTestInteger = TTest<Integer> deprecated 'Message B' experimental;
  TTestString = TTest<String>;

var
  // this will print that TTestInteger is deprecated and experimental
  t: TTestInteger;
  // this will print nothing
  t2: TTestString;
begin
  // this will print that TTest<T> is deprecated and platform
  t2 := TTest<String>.Create;
  // this will print that TTestInteger is deprecated and experimental
  t := TTestInteger.Create;
end.
