unit ugeneric99;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type
  generic TTest<T> = class
  type
    TTestT = specialize TTest<T>;
  end;

  TTestRec = record
    f: LongInt;
    type
      generic TTest<T> = class
        type
          TTestClass = class
          end;
      end;

      generic TTestArray<T> = array of T;
    var
      t: specialize TTest<LongInt>.TTestClass;
  end;

  TTestClass = class
    type
      generic TTest<T> = class
        type
          TTestRec = record
            f: LongInt;
          end;
      end;

      generic TTestArray<T> = array of T;
   var
      t: specialize TTest<LongInt>.TTestRec;
  end;

  generic TTestArray<T> = array of T;

implementation

end.

