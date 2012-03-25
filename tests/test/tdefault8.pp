{ %NORUN }

{ nested types can be used as well }
program tdefault8;

{$mode objfpc}

type
  TTest = class
  public type
    TRecord = record
      f: LongInt;
    end;

    TRange = -5..5;

    TSomeClass = class

    end;
  end;

var
  trec: TTest.TRecord;
  trange: TTest.TRange;
  tclass: TTest.TSomeClass;
begin
  trec := Default(TTest.TRecord);
  trange := Default(TTest.TRange);
  tclass := Default(TTest.TSomeClass);
end.
