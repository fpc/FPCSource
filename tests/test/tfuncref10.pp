{ %NORUN }

{ interfaces that descend from function references can be used as function
  references as well including overloads and such }
program tfuncref10;

{$mode objfpc}
{$modeswitch functionreferences}

uses
  ufuncref10;

var
  l: LongInt;
  s: String;
  tf: TTestFunc;
  if1: ITestFunc1;
  if2: ITestFunc2;
  if3: ITestFunc3;
  if4: ITestFunc4;
  if5: ITestFunc5;
  if6: ITestFunc6;
  if7: ITestFunc7;
  if8: ITestFunc8;
  if9: ITestFunc9;
begin
  l := tf();
  { these two still call the Invoke of TTestFunc }
  l := if1();
  l := if2();
  { here only the String function is available }
  s := if3();
  //l := if3();
  { in principle both are available, but since we can't overload based on
    result type only the second one can be called }
  s := if4();
  //l := if4();
  { only the overload with the parameter is available here }
  //l := if5();
  l := if5(42);
  { both overloads can be used }
  l := if6();
  l := if6(42);
  { if it doesn't inherit from a function reference it can't be called directly }
  //l := if7();
  { ObjFPC mode requires parenthesis, so calling other methods on the interface
    can be done directly }
  l := if8.Foobar;
  { procedures and functions can be overloaded as well }
  l := if9();
  if9(42);
end.
