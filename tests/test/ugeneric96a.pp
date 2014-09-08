unit ugeneric96a;

{$mode objfpc}

interface

uses
  // difference to ugeneric96b: order of uses
  ugeneric96c, // contains non-generic TTest
  ugeneric96d; // contains generic TTest<>

type
  TLongIntTest = specialize TTest<LongInt>;

var
  lt: TLongIntTest;
  t: TTest;

implementation

end.
