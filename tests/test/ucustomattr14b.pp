unit ucustomattr14b;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

interface

uses
  ucustomattr14a;

type
  [TTest]
  TMyClass = class

  end;

  [TTest2('Hello World')]
  TMyClass2 = class

  end;

  {$M+}
  TMyClass3 = class
  private
    fTest: LongInt;
  published
    [TTest2('Foobar')]
    [TTest]
    property Test: LongInt read fTest;
  end;
  {$M-}

implementation

end.

