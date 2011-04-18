program terecs11;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TRecord = record
  private type
    TTestClass = class

    end;
    TTestRecord = record

    end;
    TTestObject = object

    end;
    TTestInterface = interface

    end;
    TTestString = String[20];
    TTestRange = 0..42;
  end;

begin
end.

