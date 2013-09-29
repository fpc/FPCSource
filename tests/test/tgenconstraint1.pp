{ %NORUN }

{ Extensively test the Delphi compatible constraint syntax }

program tgenconstraint1;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{ types used for tests }
uses
  ugenconstraints;

type
  TTest1TObject = TTest1<TObject>;
  // the documentation did say something different here...
  //TTest1IInterface = TTest1<IInterface>;
  TTest1TTestClass = TTest1<TTestClass>;

  TTest2TTestRec = TTest2<TTestRec>;

  TTest3TTestClass = TTest3<TTestClass>;
  TTest3TTestClass2 = TTest3<TTestClass2>;

  { ToDo }
  TTest4TTestClass = TTest4<TTestClass>;
  TTest4TTestClass2 = TTest4<TTestClass2>;

  TTest5IInterface = TTest5<IInterface>;
  TTest5ITest1 = TTest5<ITest1>;
  TTest5ITest2 = TTest5<ITest2>;
  TTest5TInterfacedObject = TTest5<TInterfacedObject>;

  TTest6TTestClass3 = TTest6<TTestClass3>;
  TTest6TTestClass4 = TTest6<TTestClass4>;

  TTest7TTestClass4 = TTest7<TTestClass4>;

  TTest8TTestClass3 = TTest8<TTestClass3>;
  TTest8TTestClass4 = TTest8<TTestClass4>;
  //TTest8TTestClass5 = TTest8<TTestClass5>;

  // TTest9 is the same as TTest8

  TTest10TTestClass3 = TTest10<TTestClass3>;
  TTest10TTestClass6 = TTest10<TTestClass6>;

  // TTest11 is the same as TTest10

  TTest12TTestClass = TTest12<TTestClass7>;

  TTest13TTestClass = TTest13<TTestClass>;
  TTest13TTestClass6 = TTest13<TTestClass2>;

  // TTest14 is the same as TTest10

  TTest15TTestClass8 = TTest15<TTestClass8>;

  TTest16TTestClass3 = TTest16<TTestClass3>;

  TTest17ITest1ITest1 = TTest17<ITest1, ITest1>;
  TTest17ITestClass3ITest2 = TTest17<TTestClass3, ITest2>;

  TTest18ITest1ITest2 = TTest18<ITest1, ITest2>;
  TTest18TTestClass3TTestClass5 = TTest18<TTestClass3, TTestClass5>;
  TTest18TTestClass4TTestClass4TTestClass4 = TTest18<TTestClass4, TTestClass4>;

  TTest19TTestRecTObject = TTest19<TTestRec, TObject>;

  TTest20TTestClassTTestClass = TTest20<TTestClass, TTestClass>;

  TTest21TObject = TTest21<TObject>;
  TTest21TestClass = TTest21<TTestClass>;

begin

end.
