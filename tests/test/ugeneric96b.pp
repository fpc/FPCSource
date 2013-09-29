unit ugeneric96b;

{$mode objfpc}

interface

uses
  // difference to ugeneric96a: order of uses
  ugeneric96d, // contains generic TTest<>
  ugeneric96c; // contains non-generic TTest

type
  TLongIntTest = specialize TTest<LongInt>;

var
  lt: TLongIntTest;
  t: TTest;

implementation

end.
