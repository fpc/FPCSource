unit ufuncref10;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

interface

type
  TTestFunc = reference to function : LongInt;

  ITestFunc1 = interface(TTestFunc)
  end;

  ITestFunc2 = interface(ITestFunc1)
  end;

  ITestFunc3 = interface(TTestFunc)
    function Invoke: String;
  end;

  ITestFunc4 = interface(TTestFunc)
    function Invoke: String; overload;
  end;

  ITestFunc5 = interface(TTestFunc)
    function Invoke(aArg: LongInt): LongInt;
  end;

  ITestFunc6 = interface(TTestFunc)
    function Invoke(aArg: LongInt): LongInt; overload;
  end;

  ITestFunc7 = interface
    function Invoke(aArg: LongInt): LongInt;
  end;

  ITestFunc8 = interface(TTestFunc)
    function Foobar: LongInt;
  end;

  ITestFunc9 = interface(TTestFunc)
    procedure Invoke(aArg: LongInt); overload;
  end;

implementation

end.

