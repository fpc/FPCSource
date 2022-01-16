program needassert;

uses fpcunit, testregistry, consoletestrunner;

Type
  TTestNeedAssert = Class(TTestCase) 
  Published
    Procedure NeedsToFail;
    Procedure NeedsToBeOK;
  end;

Procedure TTestNeedAssert.NeedsToFail;

begin
  // Do not call assert.
end;

Procedure TTestNeedAssert.NeedsToBeOK;

begin
  AssertTrue('Some message',True);
end;



Var
  Application : TTestRunner;
  
begin
  RegisterTest(TTestNeedAssert);
  TTestCase.CheckAssertCalled:=true;
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application:=TTestRunner.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.  