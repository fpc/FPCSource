{$mode objfpc}

program testpunit2;

uses punit;

Type
  EError = Class(TObject);

Function DoTest1 : AnsiString;

Begin
  Result:='';
  Result:='Error in test';
end;

Function DoTest2 : AnsiString;

Begin
  // Test OK if RequirePassed=False, but Unimplemented if RequirePassed=True !
//  AssertPassed('');
  Result:='';
end;

Function DoTest3 : AnsiString;

Begin
  Result:='';
  Fail('Must fail: Failed throug fail');
  Result:='';// This is ignored
end;

Function DoTest4 : AnsiString;

Begin
  Result:='';
  FailExit('Must fail: Failed throug fail exception');
  Result:='Nono';// This is not reached.
end;

Function DoTest5 : AnsiString;

Begin
  Result:='';
  Fail('Must fail: Failed throug fail');
  Result:='Failed through default';// This is ignored
end;

Function DoTest6: AnsiString;

Begin
  Result:='';
  // Will be marked as passed.
  AssertTrue('Some message',True);
  Result:='';
end;

Function DoTest7: AnsiString;

Begin
  Result:='';
  // Will be marked as Failed.
  if Not AssertTrue('Must fail: AssertTrue with false',False) then
    exit;
end;
Function DoTest9: AnsiString;

Begin
  Result:='';
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Strings equal','Expected result string','Actual result string') then
    exit;
end;

Function DoTest10: AnsiString;

Var
  O1,O2 : Integer;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Integers equal',O1,O2) then
    exit;
end;

Function DoTest11: AnsiString;

Var
  O1,O2 : Smallint;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Smallint equal',O1,O2) then
    exit;
end;

Function DoTest12: AnsiString;

Var
  O1,O2 : Longint;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Longint equal',O1,O2) then
    exit;
end;

Function DoTest13: AnsiString;

Var
  O1,O2 : Byte;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Bytes equal',O1,O2) then
    exit;
end;

Function DoTest14: AnsiString;

Var
  O1,O2 : Shortint;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Shortints equal',O1,O2) then
    exit;
end;

Function DoTest15: AnsiString;

Var
  O1,O2 : Cardinal;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Cardinals equal',O1,O2) then
    exit;
end;

Function DoTest16: AnsiString;

Var
  O1,O2 : Int64;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Int64s equal',O1,O2) then
    exit;
end;

Function DoTest17: AnsiString;

Var
  O1,O2 : QWord;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: QWords equal',O1,O2) then
    exit;
end;

Function DoTest18: AnsiString;

Var
  O1,O2 : Pointer;

Begin
  Result:='';
  O1:=Pointer(1);
  O2:=Pointer(2);
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: pointers equal',O1,O2) then
    exit;
end;

Function DoTest19: AnsiString;

Var
  O1,O2 : Word;

Begin
  Result:='';
  O1:=1;
  O2:=2;
  // Will be marked as Failed.
  if Not AssertEquals('Must fail: Word equal',O1,O2) then
    exit;
end;

Function DoTest20: AnsiString;

Begin
  Result:='';
  ExpectException('Must fail: Expect exception EError',EError);
end;

Function DoTest21: AnsiString;

Begin
  Result:='';
  ExpectException('Must fail: Expect exception EError',EError);
  Raise EFail.Create('Expected');
end;

Function DoTest22: AnsiString;

Begin
  Result:='';
  ExpectException('Expect exception EError',EFail);
  Raise EFail.Create('Expected');
end;

Function DoTest23: AnsiString;

Begin
  Result:='';
  AssertEquals('Must fail: Classes differ',EError,EFail);
end;

Function DoTest24: AnsiString;

Begin
  Result:='';
  AssertEquals('Must fail: Classes differ (expected is nil)',Nil,EFail);
end;

Function DoTest25: AnsiString;

Begin
  Result:='';
  AssertEquals('Must fail: Classes differ (actual is nil)',EFail,Nil);
end;

Function DoTest26: AnsiString;

Var
  A,B : TObject;

Begin
  Result:='';
  A:=EFail.Create('');
  B:=EError.Create();
  try
    AssertSame('Must fail: Instances differ',A,B);
  finally
    A.Free;
    B.Free
  end;
end;

Function DoTest27: AnsiString;

Var
  A : TObject;

Begin
  Result:='';
  A:=EFail.Create('');
  try
    AssertSame('Must fail: Instances differ (actual nil)',A,Nil);
  finally
    A.Free;
  end;
end;

Function DoTest28: AnsiString;

Var
  A : TObject;

Begin
  Result:='';
  A:=EFail.Create('');
  try
    AssertSame('Must fail: Instances differ (expected nil)',Nil,A);
  finally
    A.Free;
  end;
end;

Function DoTest29: AnsiString;

Var
  A : TObject;
  B : TObject;

Begin
  Result:='';
  A:=EFail.Create('');
  try
    B:=A;
    AssertSame('Instances equal(expected nil)',B,A);
  finally
    A.Free;
  end;
end;

Function DoTest30: AnsiString;

Var
  A,B : Double;

Begin
  Result:='';
  A:=1.2;
  B:=3.4;
  AssertEquals('Must fail: Doubles not within delta',B,A);
end;

Function DoTest31: AnsiString;

Var
  A,B : Double;

Begin
  Result:='';
  A:=1.2;
  B:=1.2+(DefaultDoubleDelta/2);
  AssertEquals('Doubles within delta',B,A);
end;

Function DoTest32: AnsiString;

Var
  A,B : Double;

Begin
  Result:='';
  A:=1.2;
  B:=3.4;
  AssertEquals('Doubles within delta',B,A,1);
end;

Function DoTest33: AnsiString;

Var
  A : Pointer;

Begin
  Result:='';
  A:=Nil;
  AssertNull('A is nil',A);
end;

Function DoTest34: AnsiString;

Var
  A : Pointer;

Begin
  Result:='';
  A:=Pointer(123);
  AssertNull('Must fail: A is nil',A);
end;

Function DoTest35: AnsiString;

Var
  A : Pointer;

Begin
  Result:='';
  A:=Nil;
  AssertNotNull('Must fail: A is nil',A);
end;

Function DoTest36: AnsiString;

Var
  A : Pointer;

Begin
  Result:='';
  A:=Pointer(123);
  AssertNotNull('A is not nil',A);
end;

Function DoTest37: AnsiString;


Begin
  Result:='';
  if not AssertFalse('Condition is false',False) then
    Fail('This is not supposed to happen');
end;

Function DoTest38: AnsiString;

Var
  PA,PB : Pointer;

Begin
  Result:='';
  PA:=@DoTest36;
  PB:=@DoTest37;
  if not AssertDiffers('Pointers differ',PA,PB) then
    Fail('This is not supposed to happen');
end;

Function DoTest39: AnsiString;

Var
  PA,PB : Pointer;

Begin
  Result:='';
  PA:=@DoTest36;
  PB:=@DoTest36;
  if AssertDiffers('Must fail: pointers differ',PA,PB) then
    Fail('This is not supposed to happen');
end;

Procedure DoExcept;

begin
  Raise EError.Create;
end;

Procedure DoNoExcept;

begin

end;

Procedure DoFailExcept;

begin
  Raise EFail.Create('err');
end;

Function DoTest40: AnsiString;

Begin
  Result:='';
  AssertException('Must not fail (correct exception',EError,@DoExcept);
end;

Function DoTest41: AnsiString;

Begin
  Result:='';
  AssertException('Must fail (no exception)',EError,@DoNoExcept);
end;

Function DoTest42: AnsiString;

Begin
  Result:='';
  AssertException('Must fail (Wrong exception)',EError,@DoFailExcept);
end;

Function DoTest43: AnsiString;

Begin
  Result:='';
  AssertNotSame('Pointers differ',EFail.Create(''),EError.Create);
end;

Function DoTest44: AnsiString;

Begin
  Result:='';
  if Not AssertInheritsFrom('EError is TObject',EError,TObject) then
    Fail('This should not happen');
end;

Function DoTest45: AnsiString;

Begin
  Result:='';
  if AssertInheritsFrom('Must fail, nil parent',EError,Nil) then
    Fail('This should not happen');
end;

Function DoTest46: AnsiString;

Begin
  Result:='';
  if AssertInheritsFrom('Must fail, nil child',Nil,EError) then
    Fail('This should not happen');
end;

Function DoTest47: AnsiString;

Begin
  Result:='';
  AssertInheritsFrom('Instances. Must fail',EFail.Create(''),EError.Create);
end;

Begin
  RequirePassed:=True;
  AddTest('Test1',@DoTest1);
  AddTest('Test2',@DoTest2);
  AddTest('Test3',@DoTest3);
  AddTest('Test4',@DoTest4);
  AddTest('Test5',@DoTest5);
  AddTest('Test6',@DoTest6);
  AddTest('Test7',@DoTest7);
  AddTest('Test8',@DoTest7)^.Active:=False;
  AddTest('Test9',@DoTest9);
  AddTest('Test10',@DoTest10);
  AddTest('Test11',@DoTest11);
  AddTest('Test12',@DoTest12);
  AddTest('Test13',@DoTest13);
  AddTest('Test14',@DoTest14);
  AddTest('Test15',@DoTest15);
  AddTest('Test16',@DoTest16);
  AddTest('Test17',@DoTest17);
  AddTest('Test18',@DoTest18);
  AddTest('Test19',@DoTest19);
  AddTest('Test20',@DoTest20);
  AddTest('Test21',@DoTest21);
  AddTest('Test22',@DoTest22);
  AddTest('Test23',@DoTest23);
  AddTest('Test24',@DoTest24);
  AddTest('Test25',@DoTest25);
  AddTest('Test26',@DoTest26);
  AddTest('Test27',@DoTest27);
  AddTest('Test28',@DoTest28);
  AddTest('Test29',@DoTest29);
  AddTest('Test30',@DoTest30);
  AddTest('Test31',@DoTest31);
  AddTest('Test32',@DoTest32);
  AddTest('Test33',@DoTest33);
  AddTest('Test34',@DoTest34);
  AddTest('Test35',@DoTest35);
  AddTest('Test36',@DoTest36);
  AddTest('Test37',@DoTest37);
  AddTest('Test38',@DoTest38);
  AddTest('Test39',@DoTest39);
  AddTest('Test40',@DoTest40);
  AddTest('Test41',@DoTest41);
  AddTest('Test43',@DoTest43);
  AddTest('Test44',@DoTest44);
  AddTest('Test45',@DoTest45);
  AddTest('Test46',@DoTest46);
  AddTest('Test47',@DoTest47);
  RunAllSysTests;
end.

