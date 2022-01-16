{ %FAIL }

unit tb0266b;

{$mode objfpc}{$H+}

interface

type
  TTest1 = class
    fTest: String;
  end;

  TTest2 = record
    fTest: TTest1;
  end;

  TTest3 = class
  private
    fTest: TTest2;
  public
    property Test: String read fTest.fTest.fTest;
  end;

implementation

end.

