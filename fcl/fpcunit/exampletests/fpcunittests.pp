{$mode objfpc}
{$h+}
{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt
    
    unit tests of the FPCUnit framework. 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpcunittests;

interface

uses
  SysUtils, Classes, fpcunit, testutils;

type

  EMyException = class(Exception);

  TTestCaseTest = class(TTestCase)
  private
    FFlag: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetUp;
    procedure TestAsString;
  end;

  TTestSuiteTest = class(TTestCase)
  private
    FSuite: TTestSuite;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCountTestCases;
    procedure TestExtractMethods;
  end;

  TAssertTest = class(TTestCase)
  private
    Fa,
    Fb: TObject;
    procedure FailEqualsInt;
    procedure FailEqualsInt64;
    procedure FailEqualsCurrency;
    procedure FailEqualsDouble;
    procedure FailEqualsBoolean;
    procedure FailEqualsChar;
    procedure FailEqualsTClass;
    procedure FailEqualsTObject;
    procedure FailAssertNull;
    procedure FailAssertNotNull;
    procedure RaiseMyException;
    procedure InterceptFailure(AMethod: TRunMethod; const ExpectedMessage: string);
  published
    procedure TestEqualsInt;
    procedure TestEqualsInt64;
    procedure TestEqualsCurrency;
    procedure TestEqualsDouble;
    procedure TestEqualsBoolean;
    procedure TestEqualsChar;
    procedure TestEqualsTClass;
    procedure TestEqualsTObject;
    procedure TestNull;
    procedure TestNotNull;
    procedure TestFailEqualsInt;
    procedure TestFailEqualsInt64;
    procedure TestFailEqualsCurrency;
    procedure TestFailEqualsDouble;
    procedure TestFailEqualsBoolean;
    procedure TestFailEqualsChar;
    procedure TestFailEqualsTClass;
    procedure TestFailEqualsTObject;
    procedure TestFailNull;
    procedure TestFailNotNull;
    procedure TestAssertException;
    procedure TestComparisonMsg;
  end;

  TMockListener = class(TNoRefCountObject, ITestListener)
  private
    FList: TStringList;
    FFailureList: TStringList;
    FErrorList: TStringList;
    FExpectedList: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure AddExpectedLine(ALine: string);
    procedure Verify(ActualList: TStrings);
  end;

  TExampleTest = class(TTestCase)
  published
    procedure TestOne;
    procedure TestWithError;
    procedure TestWithFailure;
  end;

  TListenerTest = class(TTestCase)
  private
    FMockListener: TMockListener;
    FResult: TTestResult;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartAndEndTest;
    procedure TestAddError;
    procedure TestAddFailure;
  end;

implementation

procedure TTestCaseTest.SetUp;
begin
  FFlag := 1
end;

procedure TTestCaseTest.TearDown;
begin
  FFlag := 0;
end;

procedure TTestCaseTest.TestSetUp;
begin
  AssertTrue( 'TTestCaseTest: wrong SetUp', FFlag = 1);
end;

procedure TTestCaseTest.TestAsString;
begin
  AssertEquals( 'TTestCaseTest: wrong AsString output', 'TESTASSTRING(TTestCaseTest)', AsString);
end;

procedure TTestSuiteTest.SetUp;
begin
  FSuite := TTestSuite.Create(TTestSuiteTest);
end;

procedure TTestSuiteTest.TearDown;
begin
  FSuite.Free;
end;



procedure TTestSuiteTest.TestCountTestCases;
begin
  AssertTrue(FSuite.CountTestCases = 2);
end;

procedure TTestSuiteTest.TestExtractMethods;
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to FSuite.CountTestCases - 1 do
    s := s + UpperCase(FSuite[i].TestName) + ' ';
  AssertEquals('Failure in extracting methods:', 'TESTCOUNTTESTCASES TESTEXTRACTMETHODS ', s );
end;

procedure TAssertTest.TestEqualsInt;
var
  i, j: integer;
begin
  AssertEquals(33,33);
  i := 33;
  j := 33;
  AssertEquals(i, j);
end;

procedure TAssertTest.TestEqualsInt64;
var
  i, j: int64;
begin
  AssertEquals(1234567891234,1234567891234);
  i := 1234567891234;
  j := 1234567891234;
  AssertEquals(i, j);
end;

procedure TAssertTest.TestEqualsCurrency;
var
  i, j: currency;
begin
  AssertEquals(12345678912345.6789, 12345678912345.6789);
  i := 12345678912345.6789;
  j := 12345678912345.6789;
  AssertEquals(i, j);
end;


procedure TAssertTest.TestEqualsDouble;
var
  i, j, delta: double;
begin
  i := 0.123456;
  j := 0.123456;
  delta := 0.0000001;
  AssertEquals(i,j, delta);
end;

procedure TAssertTest.TestEqualsBoolean;
var
  a, b: boolean;
begin
  a := true;
  b := true;
  AssertEquals(a, b);
end;

procedure TAssertTest.TestEqualsChar;
var
  a, b: char;
begin
  a := 'a';
  b := 'a';
  AssertEquals(a, b);
end;

procedure TAssertTest.TestEqualsTClass;
var
  a, b: TClass;
begin
  a := TAssertTest;
  b := TAssertTest;
  AssertEquals(a, b);
end;

procedure TAssertTest.TestEqualsTObject;
var
  a, b: TObject;
begin
  a := TMockListener.Create;
  b := a;
  AssertSame(a, b);
  a.Free;
end;

procedure TAssertTest.TestNull;
begin
  AssertNull(nil);
end;

procedure TAssertTest.TestNotNull;
var
  obj: TTestCase;
begin
  obj := TTestCase.Create;
  AssertNotNull(obj);
  obj.Free;
end;

procedure TAssertTest.InterceptFailure(AMethod: TRunMethod; const ExpectedMessage: string);
var
  failureIntercepted: boolean;
begin
  failureIntercepted := False;
  try
    AMethod;
  except
    on E: EAssertionFailedError do
       begin
         failureIntercepted := True;
         if (E.Message <> ExpectedMessage) then
	   raise EAssertionFailedError.Create('Wrong failure message: expected <'+ ExpectedMessage + '>'
	     + 'but was <' + E.Message +'>');
       end
    else
      raise;
  end;
  if not failureIntercepted then
    raise EAssertionFailedError.Create('Expected an EAssertionFailedError');
end;

procedure TAssertTest.FailEqualsInt;
var
  i, j: integer;
begin
  i := 33;
  j := 34;
  AssertEquals(i, j);
end;

procedure TAssertTest.FailEqualsInt64;
var
  i, j: int64;
begin
  i := 33;
  j := 34;
  AssertEquals(i,j);
end;

procedure TAssertTest.FailEqualsCurrency;
var
  i, j: Currency;
begin
  i := 12345678912.6789;
  j := 12345678912.6788;
  AssertEquals(i,j);
end;

procedure TAssertTest.FailEqualsDouble;
var
  i, j, delta: double;
begin
  i := 33.00;
  j := 34.00;
  delta := 0.0000001;
  AssertEquals(i, j, delta);
end;

procedure TAssertTest.FailEqualsBoolean;
var
  a, b: boolean;
begin
  a := true;
  b := false;
  AssertEquals(a, b);
end;

procedure TAssertTest.FailEqualsChar;
var
  a, b: char;
begin
  a := 'a';
  b := 'b';
  AssertEquals(a, b);
end;

procedure TAssertTest.FailEqualsTClass;
var
  a, b: TClass;
begin
  a := TAssertTest;
  b := TTestSuiteTest;
  AssertEquals(a, b);
end;

procedure TAssertTest.FailEqualsTObject;
begin
  AssertSame(Fa,Fb);
  FA.Free;
  FB.Free;
end;

procedure TAssertTest.FailAssertNull;
var
  obj: TTestCase;
begin
  obj := TTestCase.Create;
  try
    AssertNull(obj);
  finally
    obj.Free;
  end;  
end;

procedure TAssertTest.FailAssertNotNull;
var
  obj: TObject;
begin
  obj := nil;
  AssertNotNull(obj);
end;

procedure TAssertTest.TestFailEqualsInt;
begin
  InterceptFailure(@FailEqualsInt, ' expected: <33> but was: <34>');  
end;

procedure TAssertTest.TestFailEqualsInt64;
begin
  InterceptFailure(@FailEqualsInt64, ' expected: <33> but was: <34>');
end;

procedure TAssertTest.TestFailEqualsCurrency;
begin
  InterceptFailure(@FailEqualsCurrency, ' expected: <'+FloatToStr(12345678912.6789)+'> but was: <'+FloatToStr(12345678912.6788)+'>');
end;

procedure TAssertTest.TestFailEqualsDouble;
begin
  InterceptFailure(@FailEqualsDouble, ' expected: <33> but was: <34>')
end;

procedure TAssertTest.TestFailEqualsBoolean;
begin
  InterceptFailure(@FailEqualsBoolean, ' expected: <TRUE> but was: <FALSE>');
end;

procedure TAssertTest.TestFailEqualsChar;
begin
  InterceptFailure(@FailEqualsChar, ' expected: <a> but was: <b>');
end;

procedure TAssertTest.TestFailEqualsTClass;
begin
  InterceptFailure(@FailEqualsTClass, ' expected: <TAssertTest> but was: <TTestSuiteTest>');
end;

procedure TAssertTest.TestFailEqualsTObject;
begin
  FA := TAssertTest.Create;
  FB := TAssertTest.Create;
  InterceptFailure(@FailEqualsTObject, ' expected: <'+ IntToStr(PtrInt(FA)) +
    '> but was: <' + IntToStr(PtrInt(FB))+ '>');
  FA.Free;
  FB.Free;  
end;

procedure TAssertTest.TestFailNull;
begin
  InterceptFailure(@FailAssertNull, '');
end;

procedure TAssertTest.TestFailNotNull;
begin
  InterceptFailure(@FailAssertNotNull, '');
end;

procedure TAssertTest.RaiseMyException;
begin
  raise EMyException.Create('EMyException raised');
end;

procedure TAssertTest.TestAssertException;
begin
  AssertException(EMyException, @RaiseMyException);
end;

procedure TAssertTest.TestComparisonMsg;
begin
  AssertEquals(' expected: <expectedstring> but was: <actualstring>', 
    ComparisonMsg('expectedstring', 'actualstring'));
end;

constructor TMockListener.Create;
begin
  FList := TStringList.Create;
  FFailureList := TStringList.Create;
  FErrorList := TStringList.Create;
  FExpectedList := TStringList.Create;
end;

destructor TMockListener.Destroy;
begin
  FList.Free;
  FFailureList.Free;
  FErrorList.Free;
  FExpectedList.Free;
end;

procedure TMockListener.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FFailureList.Add(ATest.TestName + ': ' + AFailure.ExceptionMessage);
end;

procedure TMockListener.AddError(ATest: TTest; AError: TTestFailure);
begin
  FErrorList.Add(ATest.TestName + ': ' + AError.ExceptionMessage);
end;

procedure TMockListener.StartTest(ATest: TTest);
begin
  FList.Add('Started: ' + ATest.TestName)
end;

procedure TMockListener.EndTest(ATest: TTest);
begin
  FList.Add('Ended: ' + ATest.TestName)
end;


procedure TMockListener.AddExpectedLine(ALine: string);
begin
  FExpectedList.Add(ALine)
end;

procedure TMockListener.Verify(ActualList: TStrings);
begin
  TAssert.AssertEquals('Error in comparing text', FExpectedList.Text, ActualList.Text);
end;

procedure TExampleTest.TestOne;
var 
  i: integer;
begin
  i := 1;
  AssertEquals(1, i);
end;

procedure TExampleTest.TestWithError;
begin
  raise Exception.Create('Error Raised');
end;

procedure TExampleTest.TestWithFailure;
begin
  Fail('Failure Raised');
end;

procedure TListenerTest.SetUp;
begin
  FMockListener := TMockListener.Create;
  FResult := TTestResult.Create;
  FResult.AddListener(FMockListener);
end;
    
procedure TListenerTest.TearDown;
begin
  FMockListener.Free;
  FResult.Free;
end;

procedure TListenerTest.TestStartAndEndTest;
var 
  t: TTestCase;
begin
  t := TExampleTest.CreateWith('TestOne','TExampleTest');
  try
    t.Run(FResult);
    FMockListener.AddExpectedLine('Started: TestOne');
    FMockListener.AddExpectedLine('Ended: TestOne');
    FMockListener.Verify(FMockListener.FList);
  finally
    t.Free;
  end;
end;

procedure TListenerTest.TestAddError;
var
  t: TTestCase;
begin
  t := TExampleTest.CreateWith('TestWithError', 'TExampleTest');
  try
    t.Run(FResult);
    FMockListener.AddExpectedLine('TestWithError: Error Raised');
    FMockListener.Verify(FMockListener.FErrorList);
  finally
    t.Free;
  end;  
end;


procedure TListenerTest.TestAddFailure;
var 
  t: TTestCase;
begin
  t := TExampleTest.CreateWith('TestWithFailure', 'TExampleTest');
  try
    t.Run(FResult);
    FMockListener.AddExpectedLine('TestWithFailure: Failure Raised');
    FMockListener.Verify(FMockListener.FFailureList);
  finally
    t.Free;
  end;    
end;

end.
