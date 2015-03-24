{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec, Michael Van Canneyt

    Port to Free Pascal of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpcunit;

{$mode objfpc}
{$h+}

interface

{ Uncomment this define to remove the DUnit compatibility interface. }
{$DEFINE DUnit}

uses
  SysUtils
  ,Classes
  ;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$define read_interface}
{$undef read_implementation}


type

  EAssertionFailedError = class(Exception)
    constructor Create; overload;
    constructor Create(const msg :string); overload;
  end;


  EIgnoredTest = class(EAssertionFailedError);


  TTestStep = (stSetUp, stRunTest, stTearDown, stNothing);


  TRunMethod = procedure of object;

  TTestResult = class;
  TTestSuite = class;

  {$M+}
  TTest = class(TObject)
  protected
    FLastStep: TTestStep;
    function GetTestName: string; virtual;
    function GetTestSuiteName: string; virtual;
    function GetEnableIgnores: boolean; virtual;
    procedure SetTestSuiteName(const aName: string); virtual; abstract;
    procedure SetEnableIgnores(Value: boolean); virtual; abstract;
  public
    function CountTestCases: integer; virtual;
    procedure Run(AResult: TTestResult); virtual;
    procedure Ignore(const AMessage: string);
  published
    property TestName: string read GetTestName;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    property LastStep: TTestStep read FLastStep;
    property EnableIgnores: boolean read GetEnableIgnores write SetEnableIgnores;
  end;
  {$M-}


  { TAssert }

  TAssert = class(TTest)
  public
    class procedure Fail(const AMessage: string);
    class procedure Fail(const AFmt: string; Args : Array of const);
    class procedure AssertTrue(const AMessage: string; ACondition: boolean); overload;
    class procedure AssertTrue(ACondition: boolean); overload;
    class procedure AssertFalse(const AMessage: string; ACondition: boolean); overload;
    class procedure AssertFalse(ACondition: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: string); overload;
    class procedure AssertEquals(Expected, Actual: string); overload;
    {$IFDEF UNICODE}
    class procedure AssertEquals(const AMessage: string; Expected, Actual: UnicodeString); overload;
    class procedure AssertEquals(Expected, Actual: UnicodeString); overload;
    {$ENDIF}
    class procedure AssertEquals(const AMessage: string; Expected, Actual: integer); overload;
    class procedure AssertEquals(Expected, Actual: integer); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: int64); overload;
    class procedure AssertEquals(Expected, Actual: int64); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: currency); overload;
    class procedure AssertEquals(Expected, Actual: currency); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: boolean); overload;
    class procedure AssertEquals(Expected, Actual: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: char); overload;
    class procedure AssertEquals(Expected, Actual: char); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: TClass); overload;
    class procedure AssertEquals(Expected, Actual: TClass); overload;
    class procedure AssertSame(const AMessage: string; Expected, Actual: TObject); overload;
    class procedure AssertSame(Expected, Actual: TObject); overload;
    class procedure AssertSame(const AMessage: string; Expected, Actual: Pointer); overload;
    class procedure AssertSame(Expected, Actual: Pointer); overload;
    class procedure AssertNotSame(const AMessage: string; Expected, Actual: TObject); overload;
    class procedure AssertNotSame(Expected, Actual: TObject); overload;
    class procedure AssertNotSame(const AMessage: string; Expected, Actual: Pointer); overload;
    class procedure AssertNotSame(Expected, Actual: Pointer); overload;
    class procedure AssertNotNull(const AMessage: string; AObject: TObject); overload;
    class procedure AssertNotNull(AObject: TObject); overload;
    class procedure AssertNotNullIntf(const AMessage: string; AInterface: IInterface); overload;
    class procedure AssertNotNullIntf(AInterface: IInterface); overload;
    class procedure AssertNotNull(const AMessage: string; APointer: Pointer); overload;
    class procedure AssertNotNull(APointer: Pointer); overload;
    class procedure AssertNull(const AMessage: string; AObject: TObject); overload;
    class procedure AssertNull(AObject: TObject); overload;
    class procedure AssertNullIntf(const AMessage: string; AInterface: IInterface); overload;
    class procedure AssertNullIntf(AInterface: IInterface); overload;
    class procedure AssertNull(const AMessage: string; APointer: Pointer); overload;
    class procedure AssertNull(APointer: Pointer); overload;
    class procedure AssertNotNull(const AMessage, AString: string); overload;
    class procedure AssertNotNull(const AString: string); overload;
    class procedure AssertException(const AMessage: string; AExceptionClass: ExceptClass; AMethod: TRunMethod); overload;
    class procedure AssertException(AExceptionClass: ExceptClass; AMethod: TRunMethod); overload;

    {$IFDEF DUnit}
      {$I DUnitCompatibleInterface.inc}
    {$ENDIF DUnit}
  end;

  TTestFailure = class(TObject)
  private
    FTestName: string;
    FTestSuiteName: string;
    FLineNumber: longint;
    FFailedMethodName: string;
    FRaisedExceptionClass: TClass;
    FRaisedExceptionMessage: string;
    FSourceUnitName: string;
    FTestLastStep: TTestStep;
    function GetAsString: string;
    function GetExceptionMessage: string;
    function GetIsFailure: boolean;
    function GetIsIgnoredTest: boolean;
    function GetExceptionClassName: string;
    procedure SetTestLastStep(const Value: TTestStep);
  public
    constructor CreateFailure(ATest: TTest; E: Exception; LastStep: TTestStep);
    property ExceptionClass: TClass read FRaisedExceptionClass;
  published
    property AsString: string read GetAsString;
    property IsFailure: boolean read GetIsFailure;
    property IsIgnoredTest: boolean read GetIsIgnoredTest;
    property ExceptionMessage: string read GetExceptionMessage;
    property ExceptionClassName: string read GetExceptionClassName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property LineNumber: longint read FLineNumber write FLineNumber;
    property FailedMethodName: string read FFailedMethodName write FFailedMethodName;
    property TestLastStep: TTestStep read FTestLastStep write SetTestLastStep;
  end;

  ITestListener = interface
  ['{0CE9D3AE-882A-D811-9401-ADEB5E4C7FC1}']
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  end;

  TTestCase = class(TAssert)
  private
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
  protected
    function CreateResult: TTestResult; virtual;
    procedure SetUp; virtual;
    procedure TearDown; virtual;
    procedure RunTest; virtual;
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure SetTestName(const Value: string); virtual;
    procedure SetEnableIgnores(Value: boolean); override;
    procedure RunBare; virtual;
  public
    constructor Create; virtual;
    constructor CreateWith(const ATestName: string; const ATestSuiteName: string); virtual;
    constructor CreateWithName(const AName: string); virtual;
    function CountTestCases: integer; override;
    function CreateResultAndRun: TTestResult; virtual;
    procedure Run(AResult: TTestResult); override;
    function AsString: string;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
  published
    property TestName: string read GetTestName write SetTestName;
  end;

  TTestCaseClass = class of TTestCase;

  TTestSuite = class(TTest)
  private
    FTests: TFPList;
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
    function GetTest(Index: integer): TTest;
  protected
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure SetTestName(const Value: string); virtual;
    procedure SetEnableIgnores(Value: boolean); override;
  public
    constructor Create(AClass: TClass; AName: string); reintroduce; overload; virtual;
    constructor Create(AClass: TClass); reintroduce; overload; virtual;
    constructor Create(AClassArray: Array of TClass); reintroduce; overload; virtual;
    constructor Create(AName: string); reintroduce; overload; virtual;
    constructor Create; reintroduce; overload; virtual;
    destructor Destroy; override;
    function CountTestCases: integer; override;
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); virtual;
    procedure AddTest(ATest: TTest); overload; virtual;
    procedure AddTestSuiteFromClass(ATestClass: TClass); virtual;
    class function Warning(const aMessage: string): TTestCase;
    property Test[Index: integer]: TTest read GetTest; default;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    property TestName: string read GetTestName write SetTestName;
    property Tests: TFPList read FTests;
  end;
  
  TProtect = procedure(aTest: TTest; aResult: TTestResult);

  { TTestResult }

  TTestResult = class(TObject)
  protected
    FRunTests: integer;
    FFailures: TFPList;
    FIgnoredTests: TFPList;
    FErrors: TFPList;
    FListeners: TFPList;
    FSkippedTests: TFPList;
    FStartingTime: TDateTime;
    function GetNumErrors: integer;
    function GetNumFailures: integer;
    function GetNumIgnoredTests: integer;
    function GetNumSkipped: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearErrorLists;
    procedure StartTest(ATest: TTest);
    procedure AddFailure(ATest: TTest; E: EAssertionFailedError; aFailureList: TFPList);
    procedure AddError(ATest: TTest; E: Exception; AUnitName: string;
      AFailedMethodName: string; ALineNumber: longint);
    procedure EndTest(ATest: TTest);
    procedure AddListener(AListener: ITestListener);
    procedure RemoveListener(AListener: ITestListener);
    procedure Run(ATestCase: TTestCase);
    procedure RunProtected(ATestCase: TTest; protect: TProtect);
    function WasSuccessful: boolean;
    function SkipTest(ATestCase: TTestCase): boolean;
    procedure AddToSkipList(ATestCase: TTestCase);
    procedure RemoveFromSkipList(ATestCase: TTestCase);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  published
    property Listeners: TFPList read FListeners;
    property Failures: TFPList read FFailures;
    property IgnoredTests: TFPList read FIgnoredTests;
    property Errors: TFPList read FErrors;
    property RunTests: integer read FRunTests;
    property NumberOfErrors: integer read GetNumErrors;
    property NumberOfFailures: integer read GetNumFailures;
    property NumberOfIgnoredTests: integer read GetNumIgnoredTests;
    property NumberOfSkippedTests: integer read GetNumSkipped;
    property StartingTime: TDateTime read FStartingTime;
  end;

  function ComparisonMsg(const aExpected: string; const aActual: string; const aCheckEqual: boolean=true): string;
  {$IFDEF UNICODE}
  function ComparisonMsg(const aExpected: UnicodeString; const aActual: UnicodeString; const aCheckEqual: boolean=true): string; overload;
  {$ENDIF}

  // Made public for 3rd party developers extending TTestCase with new AssertXXX methods
  function CallerAddr: Pointer;

  
Resourcestring

  SCompare = ' expected: <%s> but was: <%s>';
  SCompareNotEqual = ' expected: not equal to <%s> but was: <%s>';
  SExpectedNotSame = 'expected not same';
  SExceptionCompare = 'Exception %s expected but %s was raised';
  SMethodNotFound = 'Method <%s> not found';
  SNoValidInheritance = ' does not inherit from TTestCase';
  SNoValidTests = 'No valid tests found in ';
  SNoException = 'no exception';

implementation

uses
  testutils;

Const
  sExpectedButWasFmt = 'Expected:' + LineEnding + '"%s"' + LineEnding + 'But was:' + LineEnding + '"%s"';
  sExpectedButWasAndMessageFmt = '%s' + LineEnding + sExpectedButWasFmt;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}


function CallerAddr: Pointer;
var
  bp: Pointer;
begin
  bp := get_caller_frame(get_frame);
  if bp <> nil then
    Result := get_caller_addr(bp)
  else
    Result := nil;
end;

function AddrsToStr(Addrs: Pointer): string;
begin
  if PtrUInt(Addrs) > 0 then
    Result := '$'+Format('%p', [Addrs])
  else
    Result := 'n/a';
end;


function PointerToLocationInfo(Addrs: Pointer): string;

begin
  Result := BackTraceStrFunc(Addrs);
  if Trim(Result) = '' then
    Result := AddrsToStr(Addrs) + '  <no map file>';
end;


type

  TTestWarning = class(TTestCase)
  private
    FMessage: String;
  protected
    procedure RunTest; override;
  end;


procedure TTestWarning.RunTest;
begin
  Fail(FMessage);
end;


function ComparisonMsg(const aExpected: string; const aActual: string; const aCheckEqual: boolean=true): string;
// aCheckEqual=false gives the error message if the test does *not* expect the results to be the same.
begin
  if aCheckEqual then
    Result := format(SCompare, [aExpected, aActual])
  else {check unequal requires opposite error message}
    Result := format(SCompareNotEqual, [aExpected, aActual]);
end;

{$IFDEF UNICODE}
function ComparisonMsg(const aExpected: UnicodeString; const aActual: UnicodeString; const aCheckEqual: boolean=true): string;
// aCheckEqual=false gives the error message if the test does *not* expect the results to be the same.
begin
  if aCheckEqual then
    Result := format(UnicodeString(SCompare), [aExpected, aActual])
  else {check unequal requires opposite error message}
    Result := format(UnicodeString(SCompareNotEqual), [aExpected, aActual]);
end;
{$ENDIF}



constructor EAssertionFailedError.Create;
begin
  inherited Create('');
end;


constructor EAssertionFailedError.Create(const msg: string);
begin
  inherited Create(msg);
end;


constructor TTestFailure.CreateFailure(ATest: TTest; E: Exception; LastStep: TTestStep);
begin
  inherited Create;
  FTestName := ATest.GetTestName;
  FTestSuiteName := ATest.GetTestSuiteName;
  FRaisedExceptionClass := E.ClassType;
  FRaisedExceptionMessage := E.Message;
  FTestLastStep := LastStep;
end;


function TTestFailure.GetAsString: string;
var
  s: string;
begin
  if FTestSuiteName <> '' then
    s := FTestSuiteName + '.'
  else
    s := '';
  Result := s + FTestName + ': ' + FRaisedExceptionMessage;
end;


function TTestFailure.GetExceptionClassName: string;
begin
  if Assigned(FRaisedExceptionClass) then
    Result := FRaisedExceptionClass.ClassName
  else
    Result := '<NIL>'
end;


function TTestFailure.GetExceptionMessage: string;
begin
  Result := FRaisedExceptionMessage;
  if TestLastStep = stSetUp then
    Result := '[SETUP] ' + Result
  else if TestLastStep = stTearDown then
    Result := '[TEARDOWN] ' + Result;
end;


function TTestFailure.GetIsFailure: boolean;
begin
  Result := FRaisedExceptionClass.InheritsFrom(EAssertionFailedError);
end;

function TTestFailure.GetIsIgnoredTest: boolean;
begin
  Result := FRaisedExceptionClass.InheritsFrom(EIgnoredTest);
end;

procedure TTestFailure.SetTestLastStep(const Value: TTestStep);
begin
  FTestLastStep := Value;
end;


{ TTest}

function TTest.GetTestName: string;
begin
  Result := 'TTest';
end;


function TTest.GetTestSuiteName: string;
begin
  Result := 'TTest';
end;


function TTest.CountTestCases: integer;
begin
  Result := 0;
end;

function TTest.GetEnableIgnores: boolean;
begin
  Result := True;
end;

procedure TTest.Run(AResult: TTestResult);
begin
  { do nothing }
end;

procedure TTest.Ignore(const AMessage: String);
begin
  if EnableIgnores then raise EIgnoredTest.Create(AMessage);
end;

{ TAssert }

class procedure TAssert.Fail(const AMessage: string);
begin
  raise EAssertionFailedError.Create(AMessage);
end;

class procedure TAssert.Fail(const AFmt: string; Args: array of const);
begin
  raise EAssertionFailedError.CreateFmt(AFmt,Args);
end;


class procedure TAssert.AssertTrue(const AMessage: string; ACondition: boolean);
begin
  if (not ACondition) then
    Fail(AMessage);
end;


class procedure TAssert.AssertTrue(ACondition: boolean);
begin
  AssertTrue('', ACondition);
end;


class procedure TAssert.AssertFalse(const AMessage: string; ACondition: boolean
  );
begin
  AssertTrue(AMessage, not ACondition);
end;


class procedure TAssert.AssertFalse(ACondition: boolean);
begin
  AssertFalse('', ACondition);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: string);
begin
  AssertTrue(AMessage + ComparisonMsg(Expected, Actual), AnsiCompareStr(Expected, Actual) = 0);
end;


class procedure TAssert.AssertEquals(Expected, Actual: string);
begin
  AssertEquals('', Expected, Actual);
end;

{$IFDEF UNICODE}
class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: UnicodeString);
begin
  AssertTrue(AMessage + ComparisonMsg(Expected, Actual), (Expected=Actual));
end;


class procedure TAssert.AssertEquals(Expected, Actual: UnicodeString);
begin
  AssertEquals('', Expected, Actual);
end;
{$ENDIF}

class procedure TAssert.AssertNotNull(const AString: string);
begin
  AssertNotNull('', AString);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: integer);
begin
  AssertTrue(AMessage + ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: integer);
begin
  AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: int64);
begin
  AssertTrue(AMessage + ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: int64);
begin
  AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: currency);
begin
  AssertTrue(AMessage + ComparisonMsg(FloatToStr(Expected), FloatToStr(Actual)), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: currency);
begin
   AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual, Delta: double);
begin
  AssertTrue(AMessage + ComparisonMsg(FloatToStr(Expected),FloatToStr(Actual)),
    (Abs(Expected - Actual) <= Delta));
end;


class procedure TAssert.AssertEquals(Expected, Actual, Delta: double);
begin
  AssertEquals('', Expected, Actual, Delta);
end;


class procedure TAssert.AssertNotNull(const AMessage, AString: string);
begin
  AssertTrue(AMessage, AString <> '');
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: boolean);
begin
  AssertTrue(AMessage + ComparisonMsg(BoolToStr(Expected, true), BoolToStr(Actual, true)), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: boolean);
begin
  AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: char);
begin
  AssertTrue(AMessage + ComparisonMsg(Expected, Actual), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: char);
begin
  AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: TClass);

  Function GetN(C : TClass) : string;
  begin
    if C=Nil then
      Result:='<NIL>'
    else
      Result:=C.ClassName;
  end;

begin
  AssertTrue(AMessage + ComparisonMsg(GetN(Expected), GetN(Actual)), Expected = Actual);
end;


class procedure TAssert.AssertEquals(Expected, Actual: TClass);
begin
  AssertEquals('', Expected, Actual);
end;


class procedure TAssert.AssertSame(const AMessage: string; Expected, Actual: TObject);
begin
  AssertTrue(AMessage + ComparisonMsg(IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))),
    Expected = Actual);
end;


class procedure TAssert.AssertSame(Expected, Actual: TObject);
begin
  AssertSame('', Expected, Actual);
end;


class procedure TAssert.AssertSame(const AMessage: string; Expected, Actual: Pointer);
begin
  AssertTrue(AMessage + ComparisonMsg(IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))),
    Expected = Actual);
end;


class procedure TAssert.AssertSame(Expected, Actual: Pointer);
begin
  AssertSame('', Expected, Actual);
end;


class procedure TAssert.AssertNotSame(const AMessage: string; Expected, Actual: TObject);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual);
end;


class procedure TAssert.AssertNotSame(Expected, Actual: TObject);
begin
  AssertNotSame('', Expected, Actual);
end;


class procedure TAssert.AssertNotSame(const AMessage: string; Expected, Actual: Pointer);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual);
end;


class procedure TAssert.AssertNotSame(Expected, Actual: Pointer);
begin
  AssertNotSame('', Expected, Actual);
end;


class procedure TAssert.AssertNotNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject <> nil));
end;


class procedure TAssert.AssertNotNull(AObject: TObject);
begin
  AssertNotNull('', AObject);
end;


class procedure TAssert.AssertNotNullIntf(const AMessage: string; AInterface: IInterface);
begin
  AssertTrue(AMessage, (AInterface <> nil));
end;


class procedure TAssert.AssertNotNullIntf(AInterface: IInterface);
begin
  AssertNotNull('', AInterface);
end;


class procedure TAssert.AssertNotNull(const AMessage: string; APointer: Pointer);
begin
  AssertTrue(AMessage, (APointer <> nil));
end;


class procedure TAssert.AssertNotNull(APointer: Pointer);
begin
  AssertNotNull('', APointer);
end;


class procedure TAssert.AssertNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject = nil));
end;


class procedure TAssert.AssertNull(AObject: TObject);
begin
  AssertNull('', AObject);
end;


class procedure TAssert.AssertNullIntf(const AMessage: string; AInterface: IInterface);
begin
  AssertTrue(AMessage, (AInterface = nil));
end;


class procedure TAssert.AssertNullIntf(AInterface: IInterface);
begin
  AssertNull('', AInterface);
end;


class procedure TAssert.AssertNull(const AMessage: string; APointer: Pointer);
begin
  AssertTrue(AMessage, (APointer = nil));
end;


class procedure TAssert.AssertNull(APointer: Pointer);
begin
  AssertNull('', APointer);
end;


class procedure TAssert.AssertException(const AMessage: string; AExceptionClass: ExceptClass;
  AMethod: TRunMethod);
var
  Passed : Boolean;
  ExceptionName: string;
begin
  Passed := False;
  try
    AMethod;
    ExceptionName:=SNoException;
  except
    on E: Exception do
    begin
      ExceptionName := E.ClassName;
      if E.ClassType.InheritsFrom(AExceptionClass) then
      begin
        Passed := AExceptionClass.ClassName = E.ClassName;
      end;
    end;
  end;
  AssertTrue(Format(SExceptionCompare, [AExceptionClass.ClassName, ExceptionName])+ ': ' + AMessage, Passed);
end;


class procedure TAssert.AssertException(AExceptionClass: ExceptClass;
  AMethod: TRunMethod);
begin
  AssertException('', AExceptionClass, AMethod);
end;


{ DUnit compatibility interface }
{$IFDEF DUnit}
  {$I DUnitCompatibleInterface.inc}
{$ENDIF DUnit}


constructor TTestCase.Create;
begin
  inherited Create;
  FEnableIgnores := True;
end;


constructor TTestCase.CreateWithName(const AName: string);
begin
  Create;
  FName := AName;
end;


constructor TTestCase.CreateWith(const ATestName: string; const ATestSuiteName: string);
begin
  Create;
  FName := ATestName;
  FTestSuiteName := ATestSuiteName;
end;


function TTestCase.AsString: string;
begin
  Result := TestName + '(' + ClassName + ')';
end;


function TTestCase.CountTestCases: integer;
begin
  Result := 1;
end;


function TTestCase.CreateResult: TTestResult;
begin
  Result := TTestResult.Create;
end;


function TTestCase.GetTestName: string;
begin
  Result := FName;
end;


function TTestCase.GetEnableIgnores: boolean;
begin
  Result := FEnableIgnores;
end;


function TTestCase.GetTestSuiteName: string;
begin
  Result := FTestSuiteName;
end;


procedure TTestCase.SetTestSuiteName(const aName: string);
begin
  if FTestSuiteName <> aName then
    FTestSuiteName := aName;
end;


procedure TTestCase.SetTestName(const Value: string);
begin
  FName := Value;
end;


procedure TTestCase.SetEnableIgnores(Value: boolean);
begin
  FEnableIgnores := Value;
end;


function TTestCase.CreateResultAndRun: TTestResult;
begin
  Result := CreateResult;
  Run(Result);
end;


procedure TTestCase.Run(AResult: TTestResult);
begin
  (AResult).Run(Self);
end;


procedure TTestCase.RunBare;
begin
  FLastStep := stSetUp;
  SetUp;
  try
    FLastStep := stRunTest;
    RunTest;
    FLastStep := stTearDown;
  finally
    TearDown;
  end;
  FLastStep := stNothing;
end;


procedure TTestCase.RunTest;
var
  m: TMethod;
  RunMethod: TRunMethod;
  pMethod : Pointer;
begin
  AssertNotNull('name of the test not assigned', FName);
  pMethod := Self.MethodAddress(FName);
  if (Assigned(pMethod)) then
  begin
    m.Code := pMethod;
    m.Data := self;
    RunMethod := TRunMethod(m);
    RunMethod;
  end
  else
    begin
      Fail(format(SMethodNotFound, [FName]));
    end;
end;


procedure TTestCase.SetUp;
begin
  { do nothing }
end;


procedure TTestCase.TearDown;
begin
  { do nothing }
end;


constructor TTestSuite.Create(AClass: TClass; AName: string);
begin
  Create(AClass);
  FName := AName;
end;


constructor TTestSuite.Create(AClass: TClass);
var
  ml: TStringList;
  i: integer;
  tc: TTestCaseClass;
begin
  TAssert.AssertNotNull(AClass);
  Create(AClass.ClassName);
  if AClass.InheritsFrom(TTestCase) then
  begin
    tc := TTestCaseClass(AClass);
    ml := TStringList.Create;
    try
      GetMethodList(AClass, ml);
      for i := 0 to ml.Count -1 do
      begin
        AddTest(tc.CreateWith(ml.Strings[i], tc.ClassName));
      end;
    finally
      ml.Free;
    end;
  end
  else
    AddTest(Warning(AClass.ClassName + SNoValidInheritance));
  if FTests.Count = 0 then
    AddTest(Warning(SNoValidTests + AClass.ClassName));
end;


constructor TTestSuite.Create(AClassArray: Array of TClass);
var
  i: integer;
begin
  Create;
  for i := Low(AClassArray) to High(AClassArray) do
    if Assigned(AClassArray[i]) then
      AddTest(TTestSuite.Create(AClassArray[i]));
end;


constructor TTestSuite.Create(AName: string);
begin
  Create();
  FName := AName;
end;


constructor TTestSuite.Create;
begin
  inherited Create;
  FTests := TFPList.Create;
  FEnableIgnores := True;
end;


destructor TTestSuite.Destroy;
begin
  FreeObjects(FTests);
  FTests.Free;
  inherited Destroy;
end;


function TTestSuite.GetTest(Index: integer): TTest;
begin
  Result := TTest(FTests[Index]);
end;


function TTestSuite.GetTestName: string;
begin
  Result := FName;
end;


function TTestSuite.GetTestSuiteName: string;
begin
  Result := FTestSuiteName;
end;


function TTestSuite.GetEnableIgnores: boolean;
begin
  Result := FEnableIgnores;
end;


procedure TTestSuite.SetTestName(const Value: string);
begin
  FName := Value;
end;


procedure TTestSuite.SetTestSuiteName(const aName: string);
begin
  if FTestSuiteName <> aName then
    FTestSuiteName := aName;
end;


procedure TTestSuite.SetEnableIgnores(Value: boolean);
var
  i: integer;
begin
  if FEnableIgnores <> Value then
  begin
    FEnableIgnores := Value;
    for i := 0 to FTests.Count - 1 do
      TTest(FTests[i]).EnableIgnores := Value;
  end
end;

function TTestSuite.CountTestCases: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FTests.Count - 1 do
  begin
    Result := Result + TTest(FTests[i]).CountTestCases;
  end;
end;


procedure TTestSuite.Run(AResult: TTestResult);
var
  i: integer;
begin
  if FTests.Count > 0 then
    AResult.StartTestSuite(self);
    
  for i := 0 to FTests.Count - 1 do
    RunTest(TTest(FTests[i]), AResult);
    
  if FTests.Count > 0 then
    AResult.EndTestSuite(self);
end;


procedure TTestSuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  ATest.Run(AResult);
end;


procedure TTestSuite.AddTest(ATest: TTest);
begin
  FTests.Add(ATest);
  if ATest.TestSuiteName = '' then
    ATest.TestSuiteName := Self.TestName;
  ATest.EnableIgnores := Self.EnableIgnores;
end;


procedure TTestSuite.AddTestSuiteFromClass(ATestClass: TClass);
begin
  AddTest(TTestSuite.Create(ATestClass));
end;


class function TTestSuite.Warning(const aMessage: string): TTestCase;
var
  w: TTestWarning;
begin
  w := TTestWarning.Create;
  w.FMessage := aMessage;
  Result := w;
end;


constructor TTestResult.Create;
begin
  inherited Create;
  FFailures       := TFPList.Create;
  FIgnoredTests   := TFPList.Create;
  FErrors         := TFPList.Create;
  FListeners      := TFPList.Create;
  FSkippedTests   := TFPList.Create;
  FStartingTime   := Now;
end;


destructor TTestResult.Destroy;
begin
  FreeObjects(FFailures);
  FFailures.Free;
  FreeObjects(FIgnoredTests);
  FIgnoredTests.Free;
  FreeObjects(FErrors);
  FErrors.Free;
  FListeners.Free;
  FSkippedTests.Free;
end;


procedure TTestResult.ClearErrorLists;
begin
  FreeObjects(FFailures);
  FFailures.Clear;
  FreeObjects(FIgnoredTests);
  FIgnoredTests.Clear;
  FreeObjects(FErrors);
  FErrors.Clear;
end;


function TTestResult.GetNumErrors: integer;
begin
  Result := FErrors.Count;
end;


function TTestResult.GetNumFailures: integer;
begin
  Result := FFailures.Count;
end;

function TTestResult.GetNumIgnoredTests: integer;
begin
  Result := FIgnoredTests.Count;
end;

function TTestResult.GetNumSkipped: integer;
begin
  Result := FSkippedTests.Count;
end;


procedure TTestResult.AddListener(AListener: ITestListener);
begin
  FListeners.Add(pointer(AListener));
end;


procedure TTestResult.RemoveListener(AListener: ITestListener);
begin
  FListeners.Remove(pointer(AListener));
end;


procedure TTestResult.AddFailure(ATest: TTest; E: EAssertionFailedError; aFailureList: TFPList);
var
  i: integer;
  f: TTestFailure;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep);
  aFailureList.Add(f);
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).AddFailure(ATest, f);
  //unlock mutex
end;


procedure TTestResult.AddError(ATest: TTest; E: Exception;
  AUnitName: string; AFailedMethodName: string; ALineNumber: longint);
var
  i: integer;
  f: TTestFailure;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep);
  f.SourceUnitName := AUnitName;
  f.FailedMethodName := AFailedMethodName;
  f.LineNumber := ALineNumber;
  FErrors.Add(f);
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).AddError(ATest, f);
  //unlock mutex
end;


procedure TTestResult.EndTest(ATest: TTest);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).EndTest(ATest);
end;


procedure ProtectTest(aTest: TTest; aResult: TTestResult);
begin
  TTestCase(aTest).RunBare;
end;


procedure TTestResult.Run(ATestCase: TTestCase);
begin
  if not SkipTest(ATestCase) then
  begin
    StartTest(ATestCase);
    RunProtected(ATestCase, @ProtectTest);
    EndTest(ATestCase);
  end;
end;


procedure TTestResult.RunProtected(ATestCase: TTest; protect: TProtect);
var
  func, source: shortstring;
  line: longint;
begin
  func := '';
  source := '';
  line := 0;
  try
    protect(ATestCase, Self);
  except
    on E: EIgnoredTest do
      AddFailure(ATestCase, E, FIgnoredTests);
    on E: EAssertionFailedError do
      AddFailure(ATestCase, E, FFailures);
    on E: Exception do
      begin
      {$ifdef SHOWLINEINFO}
        GetLineInfo(LongWord(ExceptAddr), func, source, line);
      {$endif}
        AddError(ATestCase, E, source, func, line);
      end;
  end;
end;


procedure TTestResult.StartTest(ATest: TTest);
var
  count: integer;
  i: integer;
begin
  count := ATest.CountTestCases;
  //lock mutex
  FRunTests := FRunTests + count;
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).StartTest(ATest);
  //unlock mutex
end;


function TTestResult.WasSuccessful: boolean;
begin
//lock mutex
  Result := (FErrors.Count = 0) and (FFailures.Count = 0);
//unlock mutex
end;

function TTestResult.SkipTest(ATestCase: TTestCase): Boolean;
var
  i: integer;
begin
  Result := false;
  if FSkippedTests.Count = 0 then
  begin
    result := false;
    Exit;
  end
  else
    for i := 0 to FSkippedTests.Count - 1 do
    begin
      if PtrInt(FSkippedTests[i]) = PtrInt(ATestCase) then
      begin
        Result := true;
        Exit;
      end;
    end;
end;


procedure TTestResult.AddToSkipList(ATestCase: TTestCase);
begin
  FSkippedTests.Add(ATestCase);
end;

procedure TTestResult.RemoveFromSkipList(ATestCase: TTestCase);
begin
  FSkippedTests.Remove(ATestCase);
end;

procedure TTestResult.StartTestSuite(ATestSuite: TTestSuite);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).StartTestSuite(ATestSuite);
end;

procedure TTestResult.EndTestSuite(ATestSuite: TTestSuite);
var
  i: integer;
begin
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).EndTestSuite(ATestSuite);
end;

end.

