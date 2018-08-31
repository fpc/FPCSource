unit tb0650;

{$mode objfpc}{$H+}

interface

type
  TTest = record
    SomeField: String;
  end;

  TTestType = type TTest;

  TTestClass = class
    fField: TTestType;
  end;

implementation

end.

