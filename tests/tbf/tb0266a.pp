{ %FAIL }

unit tb0266a;

{$mode objfpc}{$H+}

interface

type
  TTest1 = class
    fTest: String;
  end;

  TTest2 = class
  private
    fTest: TTest1;
  public
    property Test: String read fTest.fTest;
  end;

implementation

end.

