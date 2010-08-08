program gmp_accept_test;

{$mode objfpc}{$H+}

uses
  heaptrc, testutils, strutils, math, sysutils, gmp, classes;


type
  TTestCase = class(TPersistent);
  TTestCases = class of TTestCase;

{$include gmp_test_intf}
{$include gmp_test_impl}

procedure Run(Tests: array of TTestCases);
var
  TestObj: TTestCase;
  MethodList: TStringList;
  TI, MI: integer;
  Test: procedure of object;
begin
  for TI := 0 to Length(Tests) - 1 do begin
    TestObj := Tests[TI].Create;
    MethodList := TStringList.Create;
    try
      TMethod(Test).Data := TestObj;
      GetMethodList(TestObj, MethodList);
      for MI := 0 to MethodList.Count - 1 do begin
        TMethod(Test).Code := MethodList.Objects[MI];
        Test;
      end;
      WriteLn(Format('%s: Tests executed: %d.', [TestObj.ClassName, MethodList.Count]));
    finally
      MethodList.Free;
      TestObj.Free;
    end;
  end;
end;

begin
  HaltOnNotReleased := True; // exit code wanted
  Run([TTestGmpBinding, TTestGmpExtensions, TTestGmpOperators]);
end.

