{ this tests that the correct method is called if a helper overloads an
  existing function and calls the original one recursively }
program thlp45;

{$mode objfpc}{$H+}

type
  TTest = class
    function Test(aRecurse: Boolean; aTest: String): Integer;
  end;

  TTestHelper = class helper for TTest
    function Test(aRecurse: Boolean; aTest: array of String): Integer; overload;
  end;

function TTest.Test(aRecurse: Boolean; aTest: String): Integer;
begin
  Result := 1;
end;

function TTestHelper.Test(aRecurse: Boolean; aTest: array of String): Integer;
begin
  if aRecurse then
    Result := Test(False, aTest[0])
  else
    Result := 2;  
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTest.Create;
  res := t.Test(True, ['Test']);
  Writeln('t.Test: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
