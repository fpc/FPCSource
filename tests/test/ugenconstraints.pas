unit ugenconstraints;

{$ifdef fpc}
  {$mode delphi}
{$else}
  // Delphi only knows "MSWINDOWS"
  {$define windows}
{$endif}

interface

type
  TTestClass = class

  end;

  TTestClass2 = class(TTestClass)

  end;

  TTestRec = record

  end;

  ITest1 = interface

  end;

  ITest2 = interface(ITest1)

  end;

  TTestClass3 = class(TInterfacedObject, ITest1)

  end;

  TTestClass4 = class(TInterfacedObject, ITest1, ITest2)

  end;

  TTestClass5 = class(TInterfacedObject, ITest2)

  end;

  TTestClass6 = class(TTestClass3, ITest2)

  end;

  TTestClass7 = class(TTestClass, ITest1)
    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  TTestClass8 = class(TTestClass7, ITest2)

  end;

  TTestClass9 = class(TTestClass, ITest2)
    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  TTestObject1 = object

  end;

type
  TTest1<T: class> = class

  end;

  TTest2<T: record> = class

  end;

  TTest3<T: TTestClass> = class

  end;

  TTest4<T: class, constructor> = class

  end;

  TTest5<T: IInterface> = class

  end;

  TTest6<T: ITest1> = class

  end;

  TTest7<T: ITest1, ITest2> = class

  end;

  TTest8<T: class, ITest1> = class

  end;

  TTest9<T: ITest1, class> = class

  end;

  TTest10<T: class, constructor, ITest1> = class

  end;

  TTest11<T: constructor, ITest1, class> = class

  end;

  TTest12<T: TTestClass, ITest1> = class

  end;

  TTest13<T: TTestClass, constructor> = class

  end;

  TTest14<T: TTestClass, ITest1, constructor> = class

  end;

  TTest15<T: ITest1, constructor, ITest2, TTestClass> = class

  end;

  TTest16<T: ITest1, constructor> = class

  end;

  TTest17<T1, T2: ITest1> = class

  end;

  TTest18<T1: ITest1; T2: ITest2> = class

  end;

  TTest19<T1: record; T2: class> = class

  end;

  TTest20<T1: TTestClass; T2: constructor> = class

  end;

  TTest21<T: constructor> = class

  end;

implementation

function TTestClass7.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
begin

end;

function TTestClass7._AddRef : longint;
begin

end;

function TTestClass7._Release : longint;
begin

end;

function TTestClass9.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
begin

end;

function TTestClass9._AddRef : longint;
begin

end;

function TTestClass9._Release : longint;
begin

end;

end.
