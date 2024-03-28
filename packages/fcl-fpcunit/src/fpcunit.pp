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
{$IFNDEF FPC_DOTTEDUNITS}
unit fpcunit;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$h+}
{$modeswitch functionreferences}

interface

{ Uncomment this define to remove the DUnit compatibility interface. }
{$DEFINE DUnit}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils
  ,System.Classes
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils
  ,Classes
  ;
{$ENDIF FPC_DOTTEDUNITS}


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
  TRunLocalMethod = reference to procedure;

  TTestResult = class;
  TTestSuite = class;

  {$M+}

  { TTest }

  TTest = class(TObject)
  private
  protected
    FLastStep: TTestStep;
    function GetTestName: string; virtual;
    function GetTestSuiteName: string; virtual;
    function GetEnableIgnores: boolean; virtual;
    procedure SetTestSuiteName(const aName: string); virtual; abstract;
    procedure SetEnableIgnores(Value: boolean); virtual; abstract;
  public
    function CountTestCases: integer; virtual;
    Function GetChildTestCount : Integer; virtual;
    Function GetChildTest(AIndex : Integer) : TTest; virtual;
    function FindChildTest(const AName: String): TTest;
    Function FindTest(Const AName : String) : TTest;
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
  protected
    Class var AssertCount : Integer;
  public
    type
      TStatusHook = Procedure(const msg : string);
      TStatusEvent = Procedure(const msg : string) of object;
    class var StatusHook : TStatusHook;
    class var StatusEvent : TStatusEvent;
  public
    class procedure Status(const aMsg: String); inline;
    class procedure Status(const aMsg: String; const aArgs: array of const); inline;
    class procedure Fail(const AMessage: string; AErrorAddrs: Pointer = nil);
    class procedure Fail(const AFmt: string; Args : Array of const;  AErrorAddrs: Pointer = nil);
    class procedure FailEquals(const expected, actual: string; const ErrorMsg: string = ''; AErrorAddrs: Pointer = nil);
    class procedure FailNotEquals(const expected, actual: string; const ErrorMsg: string = ''; AErrorAddrs: Pointer = nil);

    class procedure AssertTrue(const AMessage: string; ACondition: boolean; AErrorAddrs: Pointer = nil); overload;
    class procedure AssertTrue(ACondition: boolean); overload;
    class procedure AssertFalse(const AMessage: string; ACondition: boolean; AErrorAddrs: Pointer = nil); overload;
    class procedure AssertFalse(ACondition: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected: AnsiString; Actual: UnicodeString); overload;
    class procedure AssertEquals(const AMessage: string; Expected: UnicodeString; Actual: AnsiString); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: Ansistring); overload;
    class procedure AssertEquals(Expected, Actual: Ansistring); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: UnicodeString); overload;
    class procedure AssertEquals(Expected, Actual: UnicodeString); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: integer); overload;
    class procedure AssertEquals(Expected, Actual: integer); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: int64); overload;
    class procedure AssertEquals(Expected, Actual: int64); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: QWord); overload;
    class procedure AssertEquals(Expected, Actual: QWord); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: currency); overload;
    class procedure AssertEquals(Expected, Actual: currency); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(Expected, Actual, Delta: double); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: boolean); overload;
    class procedure AssertEquals(Expected, Actual: boolean); overload;
    class procedure AssertEquals(const AMessage: string; Expected, Actual: AnsiChar); overload;
    class procedure AssertEquals(Expected, Actual: AnsiChar); overload;
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
    class procedure AssertException(const AMessage: string; AExceptionClass: ExceptClass; AMethod: TRunMethod; const AExceptionMessage : String = ''; AExceptionContext : Integer = 0; AErrorAddr : Pointer = Nil); overload;
    class procedure AssertException(AExceptionClass: ExceptClass; AMethod: TRunMethod;const AExceptionMessage : String = ''; AExceptionContext : Integer = 0); overload;
    class procedure AssertNoException(const AMessage : string; AMethod: TRunMethod); overload;
    class procedure AssertNoException(AMethod: TRunMethod); overload;
    class procedure AssertException(const AMessage: string; AExceptionClass: ExceptClass; AMethod: TRunLocalMethod; const AExceptionMessage : String = ''; AExceptionContext : Integer = 0; AErrorAddr : Pointer = Nil); overload;
    class procedure AssertException(AExceptionClass: ExceptClass; AMethod: TRunLocalMethod;const AExceptionMessage : String = ''; AExceptionContext : Integer = 0); overload;
    class procedure AssertNoException(const AMessage : string; AMethod: TRunLocalMethod); overload;
    class procedure AssertNoException(AMethod: TRunLocalMethod); overload;

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
    FThrownExceptionAddress: Pointer;
    FTestLastStep: TTestStep;
    function GetAsString: string;
    function GetExceptionMessage: string;
    function GetIsFailure: boolean;
    function GetIsIgnoredTest: boolean;
    function GetExceptionClassName: string;
    function GetLocationInfo: string;
    procedure SetTestLastStep(const Value: TTestStep);
  public
    constructor CreateFailure(ATest: TTest; E: Exception; LastStep: TTestStep; ThrownExceptionAddrs: pointer = nil);
    property ExceptionClass: TClass read FRaisedExceptionClass;
  published
    property AsString: string read GetAsString;
    property IsFailure: boolean read GetIsFailure;
    property IsIgnoredTest: boolean read GetIsIgnoredTest;
    property ExceptionMessage: string read GetExceptionMessage;
    property ExceptionClassName: string read GetExceptionClassName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property LineNumber: longint read FLineNumber write FLineNumber;
    property LocationInfo: string read GetLocationInfo;
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

  { TTestCase }

  TTestCase = class(TAssert)
  private
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
    FExpectedExceptionFailMessage : String;
    FExpectedException : TClass;
    FExpectedExceptionMessage: String;
    FExpectedExceptionContext: Integer;
    FExpectedExceptionCaller : Pointer;
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
    Class function SingleInstanceForSuite : Boolean; virtual;
  Public
    Class Var CheckAssertCalled : Boolean;
  public
    constructor Create; virtual;
    constructor CreateWith(const ATestName: string; const ATestSuiteName: string); virtual;
    constructor CreateWithName(const AName: string); virtual;
    procedure ExpectException(AExceptionClass: TClass; const AExceptionMessage: string=''; AExceptionHelpContext: Integer=0);
    procedure ExpectException(const Msg: String; AExceptionClass: TClass; const AExceptionMessage: string=''; AExceptionHelpContext: Integer=0);
    function CountTestCases: integer; override;
    function CreateResultAndRun: TTestResult; virtual;
    procedure Run(AResult: TTestResult); override;
    function AsString: string;
    class function Suite : TTestSuite;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    Property ExpectedExceptionFailMessage  : String Read FExpectedExceptionFailMessage;
    Property ExpectedException : TClass Read FExpectedException;
    Property ExpectedExceptionMessage : String Read FExpectedExceptionMessage;
    Property ExpectedExceptionContext: Integer Read FExpectedExceptionContext;
  published
    property TestName: string read GetTestName write SetTestName;
  end;

  TTestCaseClass = class of TTestCase;

  { TTestSuite }

  TTestSuite = class(TTest)
  private
    FOwnsTests: Boolean;
    FTests: TFPList;
    FName: string;
    FTestSuiteName: string;
    FEnableIgnores: boolean;
    procedure SetOwnsTests(AValue: Boolean);
  protected
    Procedure SetOwnTestOnTests(AValue: Boolean);
    Function DoAddTest(ATest : TTest) : Integer;
    function GetTestName: string; override;
    function GetTestSuiteName: string; override;
    function GetEnableIgnores: boolean; override;
    procedure SetTestSuiteName(const aName: string); override;
    procedure SetTestName(const Value: string); virtual;
    procedure SetEnableIgnores(Value: boolean); override;
    property OwnsTests : Boolean Read FOwnsTests Write SetOwnsTests;
  public
    constructor Create(AClass: TClass; const AName: string); reintroduce; overload; virtual;
    constructor Create(AClass: TClass); reintroduce; overload; virtual;
    constructor Create(AClassArray: Array of TClass); reintroduce; overload; virtual;
    constructor Create(const AName: string); reintroduce; overload; virtual;
    constructor Create; reintroduce; overload; virtual;
    destructor Destroy; override;
    function CountTestCases: integer; override;
    Function GetChildTestCount : Integer; override;
    Function GetChildTest(AIndex : Integer) : TTest; override;
    procedure Run(AResult: TTestResult); override;
    procedure RunTest(ATest: TTest; AResult: TTestResult); virtual;
    procedure AddTest(ATest: TTest); overload; virtual;
    procedure AddTestSuiteFromClass(ATestClass: TClass); virtual;
    class function Warning(const aMessage: string): TTestCase;
    property Test[Index: integer]: TTest read GetChildTest; default;
    Property ChildTestCount : Integer Read GetChildTestCount;
    property TestSuiteName: string read GetTestSuiteName write SetTestSuiteName;
    property TestName: string read GetTestName write SetTestName;
    // Only for backwards compatibility. Use Test and ChildTestCount.
    property Tests: TFPList read FTests; deprecated;
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
    procedure AddFailure(ATest: TTest; E: EAssertionFailedError; aFailureList: TFPList; AThrownExceptionAdrs: Pointer);
    procedure AddError(ATest: TTest; E: Exception; AThrownExceptionAdrs: Pointer);
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

  function ComparisonMsg(const aExpected: AnsiString; const aActual: AnsiString; const aCheckEqual: boolean=true): AnsiString; overload;
  function ComparisonMsg(const aExpected: UnicodeString; const aActual: UnicodeString; const aCheckEqual: boolean=true): Unicodestring; overload;
  function ComparisonMsg(const aMsg: string; const aExpected: string; const aActual: string; const aCheckEqual: boolean=true): string; overload;

  // Made public for 3rd party developers extending TTestCase with new AssertXXX methods
  function CallerAddr: Pointer;

  
Resourcestring

  SCompare = ' expected: <%s> but was: <%s>';
  SCompareNotEqual = ' expected: not equal to <%s> but was: <%s>';
  SExpectedNotSame = 'expected not same';
  SExceptionCompare = 'Exception %s expected but %s was raised';
  SExceptionMessageCompare = 'Exception raised but exception property Message differs: ';
  SExceptionHelpContextCompare = 'Exception raised but exception property HelpContext differs: ';
  SErrUnexpectedException = 'No exception expected but exception %s was raised with message: %s';
  SMethodNotFound = 'Method <%s> not found';
  SNoValidInheritance = ' does not inherit from TTestCase';
  SNoValidTests = 'No valid tests found in ';
  SNoException = 'no exception';
  SAssertNotCalled = 'Assert not called during test.';
  
implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  FpcUnit.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses
  testutils;
{$ENDIF FPC_DOTTEDUNITS}

Const
  sExpectedButWasFmt = 'Expected:' + LineEnding + '"%s"' + LineEnding + 'But was:' + LineEnding + '"%s"';
  sExpectedButWasAndMessageFmt = '%s' + LineEnding + sExpectedButWasFmt;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}


function CallerAddr: Pointer;

Var
  address: CodePointer;
  nframes: sizeint;
begin
  nframes:=CaptureBacktrace(2,1,@address);
  if nframes=1 then
    result:=address
  else
    result:=nil;
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

// Get the ClassName of C
function GetN(C : TClass) : string;
begin
  if C=Nil then
    Result:='<NIL>'
  else
    Result:=C.ClassName;
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


function ComparisonMsg(const aExpected: Ansistring; const aActual: AnsiString; const aCheckEqual: boolean=true): AnsiString;
// aCheckEqual=false gives the error message if the test does *not* expect the results to be the same.
begin
  if aCheckEqual then
    Result := format(SCompare, [aExpected, aActual])
  else {check unequal requires opposite error message}
    Result := format(SCompareNotEqual, [aExpected, aActual]);
end;

function ComparisonMsg(const aExpected: Unicodestring; const aActual: Unicodestring; const aCheckEqual: boolean=true): Unicodestring;
// aCheckEqual=false gives the error message if the test does *not* expect the results to be the same.
begin
  if aCheckEqual then
    Result := unicodeformat(SCompare, [aExpected, aActual])
  else {check unequal requires opposite error message}
    Result := unicodeformat(SCompareNotEqual, [aExpected, aActual]);
end;


function ComparisonMsg(const aMsg: string; const aExpected: string; const aActual: string; const aCheckEqual: boolean): string;
begin
  Result := '"' + aMsg + '"' + ComparisonMsg(aExpected, aActual, aCheckEqual);
end;


constructor EAssertionFailedError.Create;
begin
  inherited Create('');
end;


constructor EAssertionFailedError.Create(const msg: string);
begin
  inherited Create(msg);
end;


constructor TTestFailure.CreateFailure(ATest: TTest; E: Exception; LastStep: TTestStep; ThrownExceptionAddrs: pointer);
begin
  inherited Create;
  FTestName := ATest.GetTestName;
  FTestSuiteName := ATest.GetTestSuiteName;
  FRaisedExceptionClass := E.ClassType;
  FRaisedExceptionMessage := E.Message;
  FThrownExceptionAddress := ThrownExceptionAddrs;
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

function TTestFailure.GetLocationInfo: string;
begin
  Result := PointerToLocationInfo(FThrownExceptionAddress);
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

function TTest.GetChildTestCount: Integer;
begin
  Result:=0;
end;

function TTest.GetChildTest(AIndex: Integer): TTest;
begin
  Result:=Nil;
end;

function TTest.FindChildTest(const AName: String): TTest;

Var
  I : Integer;

begin
  Result:=Nil;
  I:=GetChildTestCount-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=GetChildTest(I);
    if CompareText(Result.TestName,AName)<>0 then
      Result:=Nil;
    Dec(I);
    end;
end;

function TTest.FindTest(const AName: String): TTest;

Var
  S : String;
  I,P : Integer;

begin
  Result:=Nil;
  S:=AName;
  if S='' then exit;
  P:=Pos('.',S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=FindChildTest(Copy(S,1,P-1));
  if (Result<>Nil) then
    begin
    Delete(S,1,P);
    If (S<>'') then
      Result:=Result.FindTest(S);
    end
  else
    begin
    P:=GetChildTestCount;
    I:=0;
    While (Result=Nil) and (I<P) do
      begin
      Result:=GetChildTest(I).FindTest(Aname);
      Inc(I);
      end;
    end;
end;

function TTest.GetEnableIgnores: boolean;
begin
  Result := True;
end;

procedure TTest.Run(AResult: TTestResult);
begin
  { do nothing }
end;

procedure TTest.Ignore(const AMessage: string);
begin
  if EnableIgnores then raise EIgnoredTest.Create(AMessage);
end;

{ TAssert }

class procedure TAssert.Status(const aMsg: String);
begin
  If Assigned(StatusHook) then
    StatusHook(aMsg);
  if Assigned(StatusEvent) then
    StatusEvent(aMsg);
end;

class procedure TAssert.Status(const aMsg: String; const aArgs: array of const);
begin
  Status(SafeFormat(aMsg,aArgs));
end;

class procedure TAssert.Fail(const AMessage: string; AErrorAddrs: Pointer);
begin
  Inc(AssertCount);
  if AErrorAddrs = nil then
    raise EAssertionFailedError.Create(AMessage) at CallerAddr
  else
    raise EAssertionFailedError.Create(AMessage) at AErrorAddrs;
end;

class procedure TAssert.Fail(const AFmt: string; Args: array of const; AErrorAddrs: Pointer = nil);
begin
  Inc(AssertCount);
  if AErrorAddrs = nil then
    raise EAssertionFailedError.CreateFmt(AFmt,Args) at CallerAddr
  else    
    raise EAssertionFailedError.CreateFmt(AFmt,Args) at AErrorAddrs;
end;

class procedure TAssert.FailEquals(const expected, actual: string; const ErrorMsg: string; AErrorAddrs: Pointer);
begin
  Fail(EqualsErrorMessage(expected, actual, ErrorMsg), AErrorAddrs);
end;

class procedure TAssert.FailNotEquals(const expected, actual: string; const ErrorMsg: string; AErrorAddrs: Pointer);
begin
  Fail(NotEqualsErrorMessage(expected, actual, ErrorMsg), AErrorAddrs);
end;

class procedure TAssert.AssertTrue(const AMessage: string; ACondition: boolean; AErrorAddrs: Pointer = nil);
begin
  if AErrorAddrs=Nil then
    AErrorAddrs:=CallerAddr;
  if (not ACondition) then
    Fail(AMessage,AErrorAddrs)
  else
    Inc(AssertCount); // Fail will increae AssertCount
end;


class procedure TAssert.AssertTrue(ACondition: boolean);

begin
  AssertTrue('', ACondition,CallerAddr);
end;


class procedure TAssert.AssertFalse(const AMessage: string; ACondition: boolean; AErrorAddrs: Pointer = nil
  );
begin
  if AErrorAddrs=Nil then
    AErrorAddrs:=CallerAddr;
  AssertTrue(AMessage, not ACondition,AErrorAddrs);
end;


class procedure TAssert.AssertFalse(ACondition: boolean);
begin
  AssertFalse('', ACondition,CallerAddr);
end;

class procedure TAssert.AssertEquals(const AMessage: string;
  Expected: AnsiString; Actual: UnicodeString);
begin
  AssertTrue(ComparisonMsg(AMessage ,UnicodeString(Expected), Actual), UnicodeString(Expected)=Actual,CallerAddr);
end;

class procedure TAssert.AssertEquals(const AMessage: string;
  Expected: UnicodeString; Actual: AnsiString);
begin
  AssertTrue(ComparisonMsg(AMessage ,Expected, UnicodeString(Actual)), Expected=UnicodeString(Actual),CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: Ansistring);
begin
  AssertTrue(ComparisonMsg(AMessage ,Expected, Actual), Expected=Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: Ansistring);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), Expected=Actual,CallerAddr);
end;

class procedure TAssert.AssertEquals(const AMessage: string; Expected,
  Actual: UnicodeString);
begin
  AssertTrue(ComparisonMsg(AMessage ,Expected, Actual), Expected=Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: UnicodeString);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), Expected=Actual,CallerAddr);
end;



class procedure TAssert.AssertNotNull(const AString: string);
begin
  AssertNotNull('', AString);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: integer);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: integer);
begin
  AssertTrue(ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: int64);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: int64);
begin
  AssertTrue(ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: QWord);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: QWord);
begin
  AssertTrue(ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: currency);
begin
  AssertTrue(ComparisonMsg(AMessage,FloatToStr(Expected), FloatToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: currency);
begin
  AssertTrue(ComparisonMsg(FloatToStr(Expected), FloatToStr(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual, Delta: double);
begin
  AssertTrue(ComparisonMsg(AMessage,FloatToStr(Expected),FloatToStr(Actual)),
    (Abs(Expected - Actual) <= Delta),CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual, Delta: double);
begin
  AssertTrue(ComparisonMsg(FloatToStr(Expected),FloatToStr(Actual)),
    (Abs(Expected - Actual) <= Delta),CallerAddr);
end;


class procedure TAssert.AssertNotNull(const AMessage, AString: string);
begin
  AssertTrue(AMessage, AString <> '',CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: boolean);
begin
  AssertTrue(ComparisonMsg(AMessage,BoolToStr(Expected, true), BoolToStr(Actual, true)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: boolean);
begin
  AssertTrue(ComparisonMsg(BoolToStr(Expected, true), BoolToStr(Actual, true)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: AnsiChar);
begin
  AssertTrue(ComparisonMsg(AMessage,Expected, Actual), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: AnsiChar);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(const AMessage: string; Expected, Actual: TClass);

begin
  AssertTrue(ComparisonMsg(AMessage,GetN(Expected), GetN(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertEquals(Expected, Actual: TClass);
begin
  AssertTrue(ComparisonMsg(GetN(Expected), GetN(Actual)), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertSame(const AMessage: string; Expected, Actual: TObject);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertSame(Expected, Actual: TObject);
begin
  AssertTrue(ComparisonMsg(IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertSame(const AMessage: string; Expected, Actual: Pointer);
begin
  AssertTrue(ComparisonMsg(AMessage,IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertSame(Expected, Actual: Pointer);
begin
  AssertTrue(ComparisonMsg(IntToStr(PtrInt(Expected)), IntToStr(PtrInt(Actual))), Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertNotSame(const AMessage: string; Expected, Actual: TObject);
begin
  AssertFalse('"' + aMessage + '"' + SExpectedNotSame, Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertNotSame(Expected, Actual: TObject);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual);
end;


class procedure TAssert.AssertNotSame(const AMessage: string; Expected, Actual: Pointer);
begin
  AssertFalse('"' + aMessage + '"' + SExpectedNotSame, Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertNotSame(Expected, Actual: Pointer);
begin
  AssertFalse(SExpectedNotSame, Expected = Actual,CallerAddr);
end;


class procedure TAssert.AssertNotNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject <> nil),CallerAddr);
end;


class procedure TAssert.AssertNotNull(AObject: TObject);
begin
  AssertTrue('',(AObject <> nil),CallerAddr);
end;


class procedure TAssert.AssertNotNullIntf(const AMessage: string; AInterface: IInterface);
begin
  AssertTrue(AMessage, (AInterface <> nil),CallerAddr);
end;


class procedure TAssert.AssertNotNullIntf(AInterface: IInterface);
begin
  AssertTrue('', (AInterface <> nil),CallerAddr);
end;


class procedure TAssert.AssertNotNull(const AMessage: string; APointer: Pointer);
begin
  AssertTrue(AMessage, (APointer <> nil),callerAddr);
end;


class procedure TAssert.AssertNotNull(APointer: Pointer);
begin
  AssertTrue('', (APointer <> nil),callerAddr);
end;


class procedure TAssert.AssertNull(const AMessage: string; AObject: TObject);
begin
  AssertTrue(AMessage, (AObject = nil),CallerAddr);
end;


class procedure TAssert.AssertNull(AObject: TObject);
begin
  AssertTrue('',(AObject = nil),CallerAddr);
end;


class procedure TAssert.AssertNullIntf(const AMessage: string; AInterface: IInterface);
begin
  AssertTrue(AMessage, (AInterface = nil),CallerAddr);
end;


class procedure TAssert.AssertNullIntf(AInterface: IInterface);
begin
  AssertTrue('', (AInterface = nil),CallerAddr);
end;


class procedure TAssert.AssertNull(const AMessage: string; APointer: Pointer);
begin
  AssertTrue(AMessage, (APointer = nil),CallerAddr);
end;


class procedure TAssert.AssertNull(APointer: Pointer);
begin
  AssertTrue('', (APointer = nil),CallerAddr);
end;


class procedure TAssert.AssertException(const AMessage: string; AExceptionClass: ExceptClass;
  AMethod: TRunMethod;const AExceptionMessage : String = ''; AExceptionContext : Integer = 0; AErrorAddr : Pointer = Nil);

  Function MisMatch (const AClassName : String) : String;

  begin
    Result:=Format(SExceptionCompare,[AExceptionClass.ClassName, AClassName])
  end;

var
  Msg,FailMsg : string;
begin
  If AErrorAddr=Nil then
    AErrorAddr:=CallerAddr;
  FailMsg:='';
  try
    AMethod;
    FailMsg:=MisMatch(SNoException);
  except
    on E: Exception do
      begin
      if Not E.ClassType.InheritsFrom(AExceptionClass) then
        FailMsg:=MisMatch(E.ClassName)
      else if not (AExceptionClass.ClassName = E.ClassName) then
        FailMsg:=MisMatch(E.ClassName)
      else if (AExceptionMessage<>'') and (AExceptionMessage<>E.Message) then
        FailMsg:=ComparisonMsg(SExceptionMessageCompare,AExceptionMessage,E.Message)
      else if (AExceptionContext<>0) and (AExceptionContext<>E.HelpContext) then
        FailMsg:=ComparisonMsg(SExceptionHelpContextCompare,IntToStr(AExceptionContext),IntToStr(E.HelpContext))
      end;
  end;
  Msg:=FailMsg;
  if aMessage<>'' then
    Msg:=AMessage + ': '+Msg;
  AssertTrue(Msg, FailMsg='', AErrorAddr);
end;


class procedure TAssert.AssertException(AExceptionClass: ExceptClass;
  AMethod: TRunMethod;const AExceptionMessage : String = ''; AExceptionContext : Integer = 0);

begin
  AssertException('', AExceptionClass, AMethod, AExceptionMessage, AExceptionContext, CallerAddr);
end;


class procedure TAssert.AssertNoException(const AMessage: string; AMethod: TRunMethod);

var
  Msg,aClass,aExceptionMessage : String;

begin
  aClass:='';
  aExceptionMessage:='';
  Try
    aMethod;
  Except
    On E : Exception do
      begin
      aClass:=E.ClassName;
      aExceptionMessage:=E.Message;
      end;
  end;
  Msg:=Format(SErrUnexpectedException,[aClass,aExceptionMessage]);
  if aMessage<>'' then
    Msg:=aMessage+': '+Msg;
  AssertTrue(Msg,aClass='');
end;

class procedure TAssert.AssertNoException(AMethod: TRunMethod);
begin
  AssertNoException('',aMethod);
end;

class procedure TAssert.AssertException(const AMessage: string; AExceptionClass: ExceptClass; AMethod: TRunLocalMethod;
  const AExceptionMessage: String; AExceptionContext: Integer; AErrorAddr: Pointer);

  Function MisMatch (const AClassName : String) : String;

  begin
    Result:=Format(SExceptionCompare,[AExceptionClass.ClassName, AClassName])
  end;

var
  Msg,FailMsg : string;

begin
  If AErrorAddr=Nil then
    AErrorAddr:=CallerAddr;
  FailMsg:='';
  try
    AMethod;
    FailMsg:=MisMatch(SNoException);
  except
    on E: Exception do
      begin
      if Not E.ClassType.InheritsFrom(AExceptionClass) then
        FailMsg:=MisMatch(E.ClassName)
      else if not (AExceptionClass.ClassName = E.ClassName) then
        FailMsg:=MisMatch(E.ClassName)
      else if (AExceptionMessage<>'') and (AExceptionMessage<>E.Message) then
        FailMsg:=ComparisonMsg(SExceptionMessageCompare,AExceptionMessage,E.Message)
      else if (AExceptionContext<>0) and (AExceptionContext<>E.HelpContext) then
        FailMsg:=ComparisonMsg(SExceptionHelpContextCompare,IntToStr(AExceptionContext),IntToStr(E.HelpContext))
      end;
  end;
  Msg:=FailMsg;
  if aMessage<>'' then
    Msg:=AMessage + ': '+Msg;
  AssertTrue(Msg, FailMsg='', AErrorAddr);
end;

class procedure TAssert.AssertException(AExceptionClass: ExceptClass; AMethod: TRunLocalMethod; const AExceptionMessage: String;
  AExceptionContext: Integer);
begin
  AssertException('', AExceptionClass, AMethod, AExceptionMessage, AExceptionContext, CallerAddr);
end;

class procedure TAssert.AssertNoException(const AMessage: string; AMethod: TRunLocalMethod);

var
  Msg,aClass,aExceptionMessage : String;

begin
  aClass:='';
  aExceptionMessage:='';
  Try
    aMethod;
  Except
    On E : Exception do
      begin
      aClass:=E.ClassName;
      aExceptionMessage:=E.Message;
      end;
  end;
  Msg:=Format(SErrUnexpectedException,[aClass,aExceptionMessage]);
  if aMessage<>'' then
    Msg:=aMessage+': '+Msg;
  AssertTrue(Msg,aClass='');
end;

class procedure TAssert.AssertNoException(AMethod: TRunLocalMethod);
begin
  AssertNoException('',aMethod);
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

class function TTestCase.Suite: TTestSuite;
begin
  Result:=TTestSuite.Create(Self.ClassType);
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

class function TTestCase.SingleInstanceForSuite: Boolean;
begin
  Result:=False;
end;


procedure TTestCase.RunTest;
var
  m: TMethod;
  RunMethod: TRunMethod;
  pMethod : Pointer;
  FailMessage : String;

begin
  AssertNotNull('name of the test not assigned', FName);
  pMethod := Self.MethodAddress(FName);
  if (Assigned(pMethod)) then
  begin
    m.Code := pMethod;
    m.Data := self;
    RunMethod := TRunMethod(m);
    ExpectException('',Nil,'',0);
    try
      AssertCount:=0;
      FailMessage:='';
      RunMethod;
      if (FExpectedException<>Nil) then
        FailMessage:=Format(SExceptionCompare, [FExpectedException.ClassName, SNoException]);
      if CheckAssertCalled and (AssertCount=0) then  
        FailMessage:=SAssertNotCalled;
    except
      On E : Exception do
        begin
        if FExpectedException=Nil then
          Raise;
        If not (E is FExpectedException) then
          FailMessage:=Format(SExceptionCompare, [FExpectedException.ClassName, E.ClassName]);
        if (FExpectedExceptionMessage<>'') then
          if (FExpectedExceptionMessage<>E.Message) then
            FailMessage:=Format(SExceptionmessageCompare+SCompare, [FExpectedExceptionMessage,E.Message]);
        if (FExpectedExceptionContext<>0) then
          if (FExpectedExceptionContext<>E.HelpContext) then
            FailMessage:=Format(SExceptionHelpContextCompare+SCompare, [IntToStr(FExpectedExceptionContext),IntToStr(E.HelpContext)])
        end;
    end;
    if (FailMessage<>'') then
      begin
      if (FExpectedExceptionFailMessage<>'') then
        FailMessage:=' : '+FailMessage;
      Fail(FExpectedExceptionFailMessage+FailMessage,FExpectedExceptionCaller);
      end;
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

Type

  { TTestItem }

  TTestItem = Class(TObject)
  private
    FName: String;
    FOwnsTest: Boolean;
    FTest: TTest;
  public
    Constructor Create(T : TTest);
    Destructor Destroy; override;
    Property Test : TTest Read FTest;
    Property TestName : String Read FName;
    Property OwnsTest : Boolean Read FOwnsTest Write FOwnstest;
  end;

{ TTestItem }

constructor TTestItem.Create(T: TTest);
begin
  FTest:=T;
  FName:=T.TestName;
  FOwnsTest:=True;
end;

destructor TTestItem.Destroy;
begin
  if FOwnsTest then
    FreeAndNil(FTest);
  inherited Destroy;
end;

constructor TTestSuite.Create(AClass: TClass; const AName: string);
begin
  Create(AClass);
  FName := AName;
end;


constructor TTestSuite.Create(AClass: TClass);
var
  ml: TStringList;
  i,j: integer;
  tc: TTestCaseClass;
  C : TTestCase;
  SN : String;

begin
  TAssert.AssertNotNull(AClass);
  Create(AClass.ClassName);
  if AClass.InheritsFrom(TTestCase) then
  begin
    tc := TTestCaseClass(AClass);
    ml := TStringList.Create;
    try
      GetMethodList(AClass, ml);
      SN:=tc.ClassName;
      if tc.SingleInstanceForSuite then
        begin
        c:=tc.CreateWith('',SN);
        for i := 0 to ml.Count -1 do
          begin
          C.TestName:=ml[i];
          J:=DoAddTest(C);
          TTestItem(FTests[J]).OwnsTest:=(I=0);
          end;
        end
      else
        for i := 0 to ml.Count -1 do
          AddTest(tc.CreateWith(ml.Strings[i], SN));
    finally
      ml.Free;
    end;
  end
  else
    AddTest(Warning(AClass.ClassName + SNoValidInheritance));
  if FTests.Count = 0 then
    AddTest(Warning(SNoValidTests + AClass.ClassName));
end;


constructor TTestSuite.Create(AClassArray: array of TClass);
var
  i: integer;
begin
  Create;
  for i := Low(AClassArray) to High(AClassArray) do
    if Assigned(AClassArray[i]) then
      AddTest(TTestSuite.Create(AClassArray[i]));
end;


constructor TTestSuite.Create(const AName: string);
begin
  Create();
  FName := AName;
end;


constructor TTestSuite.Create;
begin
  inherited Create;
  FTests := TFPList.Create;
  FOwnsTests:=True;
  FEnableIgnores := True;
end;


destructor TTestSuite.Destroy;
begin
  FreeObjects(FTests);
  FTests.Free;
  inherited Destroy;
end;


function TTestSuite.GetChildTest(AIndex: integer): TTest;
begin
  Result := TTestItem(FTests[AIndex]).Test;
end;

function TTestSuite.GetChildTestCount: Integer;
begin
  Result:=FTests.Count;
end;

procedure TTestSuite.SetOwnsTests(AValue: Boolean);
begin
  if FOwnsTests=AValue then Exit;
  FOwnsTests:=AValue;
  SetOwnTestOnTests(AValue);
end;

procedure TTestSuite.SetOwnTestOnTests(AValue: Boolean);
Var
  I : Integer;

begin
  For I:=0 to FTests.Count-1 do
    TTestItem(FTests[i]).OwnsTest:=AValue;
end;

function TTestSuite.DoAddTest(ATest: TTest): Integer;

Var
  I : TTestItem;

begin
  I:=TTestItem.Create(ATest);
  I.OwnsTest:=OwnsTests;
  Result:=FTests.Add(I);
  if ATest.TestSuiteName = '' then
    ATest.TestSuiteName := Self.TestName;
  ATest.EnableIgnores := Self.EnableIgnores;
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
      TTestItem(FTests[i]).Test.EnableIgnores := Value;
  end
end;

function TTestSuite.CountTestCases: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FTests.Count - 1 do
  begin
    Result := Result + TTestItem(FTests[i]).Test.CountTestCases;
  end;
end;

procedure TTestCase.ExpectException(const Msg: String;
  AExceptionClass: TClass; const AExceptionMessage: string = '';
  AExceptionHelpContext: Integer =0 );
begin
  FExpectedExceptionFailMessage:=Msg;
  FExpectedException:=AExceptionClass;
  FExpectedExceptionMessage:=AExceptionMessage;
  FExpectedExceptionContext:=AExceptionHelpContext;
  FExpectedExceptionCaller:=CallerAddr;
end;

procedure TTestCase.ExpectException(AExceptionClass: TClass;
  const AExceptionMessage: string = ''; AExceptionHelpContext: Integer = 0);
begin
  FExpectedExceptionFailMessage:='';
  FExpectedException:=AExceptionClass;
  FExpectedExceptionMessage:=AExceptionMessage;
  FExpectedExceptionContext:=AExceptionHelpContext;
  FExpectedExceptionCaller:=CallerAddr;
end;

procedure TTestSuite.Run(AResult: TTestResult);
var
  i: integer;
  ti : TTestItem;

begin
  if FTests.Count > 0 then
    AResult.StartTestSuite(self);
    
  for i := 0 to FTests.Count - 1 do
    begin
    ti:=TTestItem(FTests[i]);
    if Ti.Test.InheritsFrom(TTestCase) and TTestCase(Ti.Test).SingleInstanceForSuite then
      TTestCase(Ti.Test).SetTestName(Ti.TestName);
    RunTest(TI.Test, AResult);
    end;

  if FTests.Count > 0 then
    AResult.EndTestSuite(self);
end;


procedure TTestSuite.RunTest(ATest: TTest; AResult: TTestResult);
begin
  ATest.Run(AResult);
end;


procedure TTestSuite.AddTest(ATest: TTest);
begin
  DoAddTest(ATest);
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


procedure TTestResult.AddFailure(ATest: TTest; E: EAssertionFailedError; aFailureList: TFPList; AThrownExceptionAdrs: Pointer);
var
  i: integer;
  f: TTestFailure;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep, AThrownExceptionAdrs);
  aFailureList.Add(f);
  for i := 0 to FListeners.Count - 1 do
    ITestListener(FListeners[i]).AddFailure(ATest, f);
  //unlock mutex
end;


procedure TTestResult.AddError(ATest: TTest; E: Exception; AThrownExceptionAdrs: Pointer);
var
  i: integer;
  f: TTestFailure;
begin
  //lock mutex
  f := TTestFailure.CreateFailure(ATest, E, ATest.LastStep, AThrownExceptionAdrs);
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
begin
  try
    protect(ATestCase, Self);
  except
    on E: EIgnoredTest do
      AddFailure(ATestCase, E, FIgnoredTests, ExceptAddr);
    on E: EAssertionFailedError do
      AddFailure(ATestCase, E, FFailures, ExceptAddr);
    on E: Exception do
      begin
        AddError(ATestCase, E, ExceptAddr);
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

function TTestResult.SkipTest(ATestCase: TTestCase): boolean;
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
      if PtrUInt(FSkippedTests[i]) = PtrUInt(ATestCase) then
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

initialization
  TTestCase.CheckAssertCalled:=False;
end.

