{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Michael Van Canneyt,
    member of the Free Pascal development team.

    Complete Test unit framework relying only on system unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc} // needed for exceptions
{ $DEFINE USEUNICODE} // define this if you want to use unicode.

unit punit;

interface
Type
{$IFDEF USEUNICODE}
   TTestString = UnicodeString;
{$ELSE}
   TTestString = AnsiString;
{$ENDIF}

{ ---------------------------------------------------------------------
  Some configuration
  ---------------------------------------------------------------------}

Var
  InitListLength : Integer = 10; // Initial length of test/suite lists
  GrowFactor : Double = 3/2; // Grow factor when list needs to be grown.
  DefaultSuiteName : Shortstring = 'Globals'; // Default name to use
  RequirePassed : Boolean = False; // If set to true, tests must explicitly call passed.
  DefaultDoubleDelta : Double = 1E-14;
  DefaultSingleDelta : Single = 1E-9;
  DefaultExtendedDelta : Extended = 1E-14;

{ ---------------------------------------------------------------------
  Some test structures
  ---------------------------------------------------------------------}

Type
  // General status code
  TTestError = (
    teOK,                  // All ok
    teTestsRunning,        // Tests already running
    teRegistryEmpty,       // no tests in registry
    teNoMemory,            // Could not allocate memory
    teNoSuite,             // No suite specified
    teNoSuiteName,         // No suite name specified
    teSuiteSetupFailed,    // Suite setup failed
    teSuiteTeardownFailed, // Suite teardown failed
    teDuplicateSuite,      // Duplicate suite name
    teSuiteInactive,       // Attempt to run inactive suite
    teNoTest,              // No test specified
    teNoTestName,          // No test name specified
    teDuplicateTest,       // Duplicate test name specified
    teTestNotInSuite,      // Given Test not member of given suite
    teTestInactive,        // Attempt to run inactive test
    teRunStartHandler,      // An error occurred during the run start handler;
    teRunCompleteHandler   // An error occurred during the run complete handler;
  );

  // What to do if an error occurs during test suite
  TErrorAction = (
    eaIgnore, // Ignore errors, continue testing
    eaFail,   // Fail current test run.
    eaAbort   // Abort all (Halt(1));
  );

  // Test prototypes. Empty result inficate succes, nonempty means failure

  // test suite setup.
  TTestSetup = Function : TTestString;
  // test suite teardown.
  TTestTearDown = Function : TTestString;
  // test runn.
  TTestRun = Function : TTestString;

  // A single test
  TTestOption = (toInactive);
  TTestOptions = Set of TTestOption;
  PTest = ^TTest;
  TTest = Record
    Run : TTestRun;           // Function to execute when test is run.
    Name : ShortString;       // Name of the test. Must not be empty.
    Options : TTestOptions;   // True if the test is active (default). Inactive tests are not run.
    Data : Pointer;           // Data to be associated with the test.
  end;
  TTestArray = Array of PTest;
  // List of tests structure.
  TTestList = Record
    Items : TTestArray;  // Array of Test pointers. Can be oversized. Initialized to InitListLength
    Count : Integer;     // Actual number of tests in list.
  end;
  PTestList = ^TTestList;

  // A testsuite.
  TSuiteOption = (soInactive,soSetupTearDownPerTest);
  TSuiteOptions = Set of TSuiteOption;

  PSuiteList = ^TSuiteList;
  PSuite = ^TSuite;
  TSuiteArray = Array of PSuite;
  TSuiteList = Record
    Items : TSuiteArray; // Array of Suite pointers. Can be oversized. Initialized to InitListLength
    Count : Integer;      // Actual number of suites in list.
  end;

  TSuite = Record
    Suites : TSuiteList;      // Test Suites inside this suite
    Tests : TTestList;        // Tests in this suite
    Setup : TTestSetup;       // Setup function, executed once at start of suite.
    Teardown : TTestTearDown; // Teardown function. Executed once at end of suite (regardless of errors)
    Name : Shortstring;       // Name of the suite, must not be empty.
    Options: TSuiteOptions;   // True if the suite is active (
    ParentSuite : PSuite;     // Parent suites of this suite
    Data : Pointer;           // Data to be associated with the suite
  end;



  TTestResult = (
    trEmpty,               // test didn't call any assert or didn't report error.
    trOK,                  // Test ran OK
    trSuiteInactive,       // Suite was inactive (only on suite records)
    trSuiteSetupFailed,    // Suite setup failed (only on suite records)
    trSuiteTearDownFailed, // Suite setup failed (only on suite records)
    trTestInactive,        // test was inactive
    trTestIgnore,          // test was ignored
    trAssertFailed,        // An assertion failed.
    trTestError,           // An error happened during the test (exception)
    trHandlerError         // An error occurred during the global test hooks (exception)
    );

  {
    Results are stored in a tree. At each level if the
  }

  PResultRecord = ^TResultRecord;
  TResultRecord = record
    Suite : PSuite;             // Testsuite from which this is a result. If the global run handlers have an error, a result record is created with Suite=Nil.
    Test : PTest;               // Test from which this is the result. If this is nil, then the result is the global testsuite result.
    ElapsedTime : Integer;      // time spent in test, in milliseconds. Only calculated if time hook is set
    TestResult : TTestResult;   // Result.
    TestMessage : TTestString;  // Error message, if any. If an exception is expected, then the message is set PRIOR to the error.
    ExpectException : TClass;   // Exception to expect.
    ParentResult,               // Parent result (suite result)
    ChildResults,               // Child result (tests or suites)
    NextResult : PResultRecord; // Next result at this level.
  end;

  // Statistics about a suite test run.
  TSuiteStats = Record
    Suites,                       // Number of sub suites (recursively)
    TestsFailed,                  // Number of failed tests.
    TestsInactive,                // Number of inactive tests.
    TestsIgnored,                 // Number of ignored tests.
    TestsRun,                     // Number of run tests.
    TestsError,                   // Number of tests with an error.
    TestsUnimplemented : Integer; // Number of unimplemented tests (no result or no Assert calls)
  end;

  TRunSummary = Record
    SuitesRun: Integer;       // Number of suites that were run.
    SuitesFailed : Integer;   // Number of suites that failed in setup/teardown).
    SuitesInactive : Integer; // Number of inactive suites in the test run
    TestsRun : Integer;       // Number of tests that were run.
    TestsFailed : Integer;    // Number of tests that failed.
    TestsIgnored : Integer;   // Number of tests that were ignored.
    TestsUnimplemented : Integer;    // Number of unimplemented tests.
    TestsInactive : Integer;  // Number of inactive tests.
    AssertCount : Integer;    // Number of times assert was called.
    Results : TResultRecord;   // Detailed results for all tests/suites
    ElapsedTime : Integer;    // time spent in test run, in milliseconds; Only calculated if time hook is set
  end;
  PRunSummary = ^TRunSummary;

  { EFail }

  // This exception can be raised to exit from a test. When caught, it will mark the test as failed, and copy the message to the testresult.
  EFail = Class(TObject)
  Private
    FMessage : AnsiString;
  Public
    Constructor Create(Const AMessage : AnsiString);
    Function ToString : AnsiString; override;
  end;
  // This exception can be raised to exit from a test. When caught, it will mark the test as ignored, and copy the message to the testresult.
  EIgnore = Class(EFail);

{ ---------------------------------------------------------------------
  Test Registry management
  ---------------------------------------------------------------------}


// Initialize the testregistry. The registry is implicitly initialized by the management functions
Function SetupTestRegistry : TTestError;
// Clean up the testregistry. The registry is implicitly cleaned up on program exit.
Function TearDownTestRegistry : TTestError;
// Check if the testregistry was initialized.
Function TestRegistryOK : Boolean;

// Suite management

// Add a suite with name AName (Required, duplicates forbidden) and Setup/TearDown functions. Returns the new suite or nil on error.
Function AddSuite(Const AName : ShortString; ASetup : TTestSetup = nil; ATearDown : TTestTearDown = Nil; AParent : PSuite = nil) : PSuite;
Function AddSuite(Const AName : ShortString; AParent : PSuite) : PSuite;
// Number of currently registered suites. If Recurse is false, only top-level suites are counted.
Function GetSuiteCount(Recurse : Boolean = True) : Integer;
// Number of currently registered nested suites. If Recurse is false, only directly nested suites are counted.
Function GetSuiteCount(ASuite : PSuite; Recurse : Boolean = True) : Integer;
// Return the 0-based index of the suite names AName (case sensitive). -1 if not found.
Function GetSuiteIndex(Const AName : ShortString) : Integer;
// Return the 0-based index of the nested suite names AName (case sensitive). -1 if not found.
Function GetSuiteIndex(ASuite : PSuite; Const AName : ShortString) : Integer;
// Return the suite at position AIndex. Nil on error.
Function GetSuite(Const AIndex : Integer) : PSuite;overload;
// Return the suite named AName, starting at AParent. Nil if it is not found. Suite can be named Suite1.Sub2.Sub3
Function GetSuite(Const AName : ShortString; AParent : PSuite = Nil) : PSuite;overload;


// Test management

// Register a test with name ATestName in suite ASuiteName. The test function is ARun.
// If Suitename is empty, DefaultSuiteName is used, and registered if it does not exist yet.
// Returns the newly created test.
// It is allowed to register the same function with different names
Function AddTest(Const ATestName : ShortString; ARun : TTestRun; Const ASuiteName : ShortString = '') : PTest;
// Same as above, only the suite is explitly given. It may not be nil.
Function AddTest(Const ATestName : ShortString; ARUn : TTestRun; Const ASuite : PSuite) : PTest;
// Return the 0-Based index of ATestName in suite ASuitename. Returns -1 on error or if nor found.
Function GetTestIndex(Const ASuiteName : TTestString; Const ATestName : ShortString) : Integer;
// Return the 0-Based index of ATestName in suite ASuit. Returns -1 on error or if nor found.
Function GetTestIndex(Const ASuite : PSuite; Const ATestName : ShortString) : Integer;
// Return 0-Based index of ATest in ASuite. Returns -1 if not found or on error.
Function GetTestIndex(Const ASuite : PSuite; Const ATest : PTest) : Integer;
// Return the number of tests in testsuite. On Error -1 is returned.
Function GetTestCount(Const ASuiteName : TTestString) : Integer;
// Return the number of tests in testsuite. On Error -1 is returned.
Function GetTestCount(Const ASuite : PSuite) : Integer;
// Return the test named ATestName in ASuiteName. Returns Nil if not found.
Function GetTest(Const ASuiteName : TTestString; Const ATestName : ShortString) : PTest;
// Return the test named ATestName in ASuite. Returns Nil if not found.
Function GetTest(Const ASuite : PSuite; Const ATestName : ShortString) : PTest;
// Return the test with index ATestindex in ASuite. Returns Nil if not found.
Function GetTest(Const ASuite : PSuite; Const ATestIndex : Integer) : PTest;
// Return True if ATest is part of ASuite. False otherwise or on error.
Function TestIsInSuite(Const ASuite : PSuite; Const ATest : PTest) : Boolean;

{ ---------------------------------------------------------------------
  Running tests
  ---------------------------------------------------------------------}

// The following are always complete test runs.
// Results from previous runs are cleared when one of these functions is called.

// Run all tests. Returns teOK if all tests were run without problems (failure is not a problem)
Function RunAllTests : TTestError;
// Run suite AName from the testsuite. Results can be viewed in GetCurrentRun.
Function RunSuite(Const ASuiteName : ShortString) : TTestError;
// Run suite ASuiteIndex in the testsuite. Results can be viewed in GetCurrentRun.
Function RunSuite(ASuiteIndex : Integer) : TTestError;
// Run suite ASuite (need not be registeredà. Results can be viewed in GetCurrentRun.
Function RunSuite(ASuite : PSuite) : TTestError;

// Running a test
// Run test ATestName from Suite ASuiteName in the testsuite. Results can be viewed in GetCurrentRun.
Function RunTest(Const ASuiteName : ShortString; Const ATestName: ShortString) : TTestError;
// Run test ATestName from Suite ASuite. ASuite need not be registered. Results can be viewed in GetCurrentRun.
Function RunTest(ASuite : PSuite; Const ATestName : ShortString) : TTestError;
// Run test ATest from Suite ASuite. ASuite need not be registered. Results can be viewed in GetCurrentRun.
Function RunTest(ASuite : PSuite; ATest : PTest) : TTestError;

// Special function: will register a default test and runs all tests.
// Intended for fast test suite creation and execution.
// It will halt the program with the exit codes of RunAllSysTests
// Additionally, an exit code of 2 may result if there was no test
// Doing this will disable -t and -s command-line options..
Procedure RunTest(ARun : TTestRun);

// Get run summary of the current test run. Remains available after run is finished, before a new run is started.
Function GetCurrentRun : TRunSummary;
// Get currently running suite, may be nil.
Function GetCurrentSuite : PSuite;
// Get currently running test, may be nil.
Function GetCurrentTest : PTest;
// Get currently test result record, may be nil.
Function GetCurrentResult : PResultRecord;
// Get result stats for a suite result record.
Procedure GetSuiteStats(AResults : PResultRecord; Out Stats : TSuiteStats);

{ ---------------------------------------------------------------------
  Test Result management
  ---------------------------------------------------------------------}

Function CountResults(Results : PResultRecord) : Integer;

{ ---------------------------------------------------------------------
  Assertions
  ---------------------------------------------------------------------}

// Mark test as ignored with given message. Always returns false.
Function Ignore(Const Msg : TTestString) : Boolean;
// Mark test as failed with given message.
Function Fail(Const Msg : TTestString) : Boolean;
// Mark test as failed with given message, raising EFail.
// will not return, but for symmetry has the same call signature
Function FailExit(Const Msg : TTestString) : Boolean;
// Mark test as ignored with given message, raising EIgnore.
// will not return, but for symmetry has the same call signature
Function IgnoreExit(Const Msg : TTestString) : Boolean;
// If RequirePassed = True, then this must be called to mark a test as passed.
// Otherwise it is marked 'unimplemented' if assert was never called during execution of the test.
Function AssertPassed (AMessage : TTestString=''): Boolean;
 // Mark test as passed if ACondition = true, failed if False. Returns ACondition
Function AssertTrue(const AMessage : TTestString; ACondition : Boolean): Boolean;
// Mark test as passed if ACondition = false, failed if true. Returns Not ACondition
Function AssertFalse(const AMessage : TTestString; ACondition : Boolean): Boolean;
// Check if 2 strings are equal. Mark test as failed if not.
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : ShortString): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : AnsiString): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : UnicodeString): Boolean;
// Check if 2 ordinals are equal. Mark test as failed if not.
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Boolean): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Shortint): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Byte): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Smallint): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Word): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Longint): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Cardinal): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Int64): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : QWord): Boolean;
// Floating point types
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Currency): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual: Double; ADelta : Double = 0): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual: Single; ADelta : Single = 0): Boolean;
Function AssertEquals(AMessage : TTestString; const AExpected, AActual: Extended; ADelta : Extended = 0): Boolean;
// Assert null
Function AssertNull(AMessage : TTestString; const AValue : Pointer): Boolean;
Function AssertNotNull(AMessage : TTestString; const AValue : Pointer): Boolean;

// Check if 2 pointers are equal. Mark test as failed if not.
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : Pointer): Boolean;
Function AssertDiffers(AMessage : TTestString; const AExpected, AActual : Pointer): Boolean;
// Check if 2 class types are equal. Mark test as failed if not.
Function AssertEquals(AMessage : TTestString; const AExpected, AActual : TClass): Boolean;
// Check if 2 class types are equal. Mark test as failed if not.
Function AssertInheritsFrom(AMessage : TTestString; const AChild, AParent : TObject): Boolean;
Function AssertInheritsFrom(AMessage : TTestString; const AChild, AParent : TClass): Boolean;
// Check if 2 object instances are equal. Mark test as failed if not.
Function AssertSame(AMessage : TTestString; const AExpected, AActual : TObject): Boolean;
// Check if 2 object instances are different. Mark test as failed if they are equal.
Function AssertNotSame(AMessage : TTestString; const AExpected, AActual : TObject): Boolean;
// Run procedure ARun. Expect an exception. If none is raised, or the class differs, mark the test as failed.
Function AssertException(const AMessage: string; AExceptionClass: TClass; ARun: TProcedure) : boolean;
Function AssertException(const AMessage: string; AExceptionClass: TClass; ARun: TTestRun) : boolean;

// Tell the testsuite that the test will raise an exception of class ACLass.
// If the test does not raise an exception, or the exception class differs, the test is marked as failed.
Function ExpectException(AMessage : TTestString; AClass : TClass) : Boolean;

{ ---------------------------------------------------------------------
  Test Hooks (Decorators)
  ---------------------------------------------------------------------}

Type
  // All arguments must be considered read-only.

  // Test Run hooks.
  // Handler called at the start of a testrun. Suites is an array of suites that will be run.
  TRunStartHandler = Procedure(Const Suites : TSuiteArray);
  // Handler called at the completion of a testrun. ARunResult is the run summary.
  TRunCompleteHandler = Procedure(Const ARunResult : TRunSummary);

  // Test suite hooks.

  // Handler called at the start of a suite, before suite setup. ASuite is the result.
  TSuiteStartHandler = Procedure(ASuite : PSuite);
  // Handler called at the end of a suite, after suite teardown. SuiteResults is an array of test results. The first record is a global suite result.
  TSuiteCompleteHandler = Procedure(ASuite : PSuite; Const SuiteResults : PResultRecord);
  // Handler called if the suite setup function failed (non-empty string returned or exception)
  TSuiteSetupFailureHandler = Procedure(ASuite : PSuite; Const AError : TTestString);
  // Handler called if the suite teardown function failed.(non-empty string returned or exception)
  TSuiteTearDownFailureHandler = Procedure(ASuite : PSuite; Const AError : TTestString);

  // Test hooks.

  // Handler called at the start of a test run.
  TTestStartHandler = Procedure(ATest : PTest; ASuite : PSuite);
  // Handler called at the end of a test run (regardless of pass/fail);
  TTestCompleteHandler = Procedure(ATest: PTest; ASuite : PSuite; Const TestResult : PResultRecord);

  // Other hooks

  // The sysutils or dos unit are not available to get the time. Support for getting time
  // can be enabled by including a function with the correct signature. For example:
  // using sysutils:  SetTimeHook(@SysUtils.Now);

  TTimeHook = Function : TDateTime;

// These functions set the various hooks. The old value is returned, so hooks can be chained.
Function SetRunStartHandler(AHandler : TRunStartHandler) : TRunStartHandler;
Function SetRunCompleteHandler(AHandler : TRunCompleteHandler) : TRunCompleteHandler;
Function SetSuiteStartHandler(AHandler : TSuiteStartHandler) : TSuiteStartHandler;
Function SetSuiteCompleteHandler(AHandler : TSuiteCompleteHandler) : TSuiteCompleteHandler;
Function SetSuitesetupFailureHandler(AHandler : TSuitesetupFailureHandler) : TSuitesetupFailureHandler;
Function SetSuiteTearDownFailureHandler(AHandler : TSuiteTearDownFailureHandler) : TSuiteTearDownFailureHandler;
Function SetTestStartHandler(AHandler : TTestStartHandler) : TTestStartHandler;
Function SetTestCompleteHandler(AHandler : TTestCompleteHandler) : TTestCompleteHandler;

// These functions get the current value of the various hooks.
Function GetSuiteStartHandler : TSuiteStartHandler;
Function GetTestStartHandler : TTestStartHandler;
Function GetTestCompleteHandler : TTestCompleteHandler;
Function GetSuiteCompleteHandler : TSuiteCompleteHandler;
Function GetRunStartHandler : TRunStartHandler;
Function GetRunCompleteHandler : TRunCompleteHandler;
Function GetSuitesetupFailureHandler : TSuitesetupFailureHandler;
Function GetSuiteTearDownFailureHandler : TSuiteTearDownFailureHandler;
// Clear all handlers
Procedure ClearTestHooks;

// Time hook management:

Function GetTimeHook : TTimeHook;
Function SetTimeHook(AHook : TTimeHook) : TTimeHook;
// Is the time hook set ?
Function HaveTimeHook : Boolean; inline;

// Current time as returned by hook, or 0 if hook not set.
Function GetTimeFromHook : TDateTime;
// Timespan in milliseconds between Current time as returned by hook and From. 0 if hook not set.
Function GetTimeDiff(From : TDateTime) : Integer;
// Convert timespan in milliseconds to human readable string hh:nn:ss.ZZZ (z milliseconds)
Function SysTimeString(MSec : Integer) :  ShortString;

{ ---------------------------------------------------------------------
  Errors
  ---------------------------------------------------------------------}

// Get the current error status.
Function GetTestError : TTestError;
// Get a message corresponding to the error.
Function GetTestErrorMessage : TTestString;
// Set test error. If the current is not teOK, it cannot be overwritten except by teOK.
Function SetTestError(AError : TTestError) : TTestError;
// What to do if a non-teOK value is set ?
Function GetErrorAction : TErrorAction;
// Set the error action, returns the old value.
Function SetErrorAction(AError : TErrorAction) : TErrorAction;



// System hooks
Type
  // Verbosity of system hooks
  TSysRunVerbosity = (
    rvQuiet,  // No messages at all
    rvFailures, // only output failures
    rvNormal, // normal messages
    rvVerbose // Lots of messages
  );

// Setup system hooks. Called by unit initialization.
Procedure SetupSysHandlers;
// Teardown system hooks. Called by unit finalization.
Procedure TearDownSysHandlers;
// Get current run mode.
Function GetSysRunVerbosity: TSysRunVerbosity;
/// Set currentsystem run mode
Function SetSysRunVerbosity(AMode : TSysRunVerbosity) : TSysRunVerbosity;
// Set system hook variables based on commandline.
// -v --verbose: verbose messaging
// -f --failures: only output failures
// -q --quiet: no messaging
// -n --normal: no messaging
// -o --output=Name: write to named file.
// -s --suite=Suite:  name of suite to run
// -t --test=name of test to run. If no suite is specified, DefaultSuiteName is assumed.
// -l --list simply list all tests.
// -h --help show help
Procedure ProcessSysCommandline;
// Process command line, run tests based on variables set.
// Program exit codes:
// 0 : All tests completed succesfully
// 1 : tests completed with failures.
// 3 : Suite not found (-s --suite)
// 4 : Testsuite didn't run correct.
// 6 : Test not found (-t --test)
Procedure RunAllSysTests;
// Return the OS for which the system was compiled, as a lowercase string.
// This can help when registering tests.
Function GetSysTestOS : ShortString;
// Get test setting. The settings file is by default a file called punit.cfg
// Format is Name = Value. Comment lines start with ; or #
Function SysGetSetting(Const AName : ShortString) : ShortString;

implementation

Const
  SExpected = 'Expected';
  SActual = 'Actual';
//  SIsNotNil = 'Is not nil';
//  SIsNil = 'Is nil';

Var
  CurrentError : TTestError;
  CurrentErrorAction : TErrorAction;
  CurrentTimeHook : TTimeHook;

  TestRegistry : TSuiteList;

  GlobalSuiteStartHandler : TSuiteStartHandler;
  GlobalTestStartHandler : TTestStartHandler;
  GlobalTestCompleteHandler : TTestCompleteHandler;
  GlobalSuiteCompleteHandler : TSuiteCompleteHandler;
  GlobalRunStartHandler : TRunStartHandler;
  GlobalRunCompleteHandler : TRunCompleteHandler;
  GlobalSuitesetupFailureHandler : TSuitesetupFailureHandler;
  GlobalSuiteTearDownFailureHandler : TSuiteTearDownFailureHandler;
  CurrentSuite : PSuite;
  CurrentTest : PTest;
  CurrentRun : TRunSummary;
  CurrentSuiteResult,
  CurrentResult : PResultRecord;

{ ---------------------------------------------------------------------
  Handler management
  ---------------------------------------------------------------------}

function GetTimeHook: TTimeHook;

begin
  Result:=CurrentTimeHook;
end;

function SetTimeHook(AHook: TTimeHook): TTimeHook;

begin
  Result:=CurrentTimeHook;
  CurrentTimeHook:=AHook;
end;

function HaveTimeHook: Boolean;
begin
  Result:=Assigned(CurrentTimeHook);
end;

function GetTimeFromHook: TDateTime;

begin
  if HaveTimeHook then
    Result:=CurrentTimeHook()
  else
    Result:=0;
end;

function GetTimeDiff(From: TDateTime): Integer;

const // Copied from sysutils;
   HoursPerDay = 24;
   MinsPerHour = 60;
   SecsPerMin  = 60;
   MSecsPerSec = 1000;
   MinsPerDay  = HoursPerDay * MinsPerHour;
   SecsPerDay  = MinsPerDay * SecsPerMin;
   MSecsPerDay = SecsPerDay * MSecsPerSec;

Var
  T : TDateTime;

begin
  T:=GetTimeFromHook;
  if (T=0) or (From=0) then
    Result:=0
  else
    Result:=Round((T-From)*MSecsPerDay);
end;

function SetSuiteStartHandler(AHandler: TSuiteStartHandler): TSuiteStartHandler;
begin
  Result:=GlobalSuiteStartHandler;
  GlobalSuiteStartHandler:=AHandler;
end;

function SetTestStartHandler(AHandler: TTestStartHandler): TTestStartHandler;

begin
  Result:=GlobalTestStartHandler;
  GlobalTestStartHandler:=AHandler;
end;

function SetTestCompleteHandler(AHandler: TTestCompleteHandler
  ): TTestCompleteHandler;

begin
  Result:=GlobalTestCompleteHandler;
  GlobalTestCompleteHandler:=AHandler;
end;

function SetSuiteCompleteHandler(AHandler: TSuiteCompleteHandler
  ): TSuiteCompleteHandler;

begin
  Result:=GlobalSuiteCompleteHandler;
  GlobalSuiteCompleteHandler:=AHandler;
end;

function SetRunCompleteHandler(AHandler: TRunCompleteHandler
  ): TRunCompleteHandler;

begin
  Result:=GlobalRunCompleteHandler;
  GlobalRunCompleteHandler:=AHandler;
end;

function SetRunStartHandler(AHandler: TRunStartHandler): TRunStartHandler;

begin
  Result:=GlobalRunStartHandler;
  GlobalRunStartHandler:=AHandler;
end;

function SetSuitesetupFailureHandler(AHandler: TSuitesetupFailureHandler
  ): TSuitesetupFailureHandler;

begin
  Result:=GlobalSuitesetupFailureHandler;
  GlobalSuitesetupFailureHandler:=AHandler;
end;

function SetSuiteTearDownFailureHandler(AHandler: TSuiteTearDownFailureHandler
  ): TSuiteTearDownFailureHandler;

begin
  Result:=GlobalSuiteTearDownFailureHandler;
  GlobalSuiteTearDownFailureHandler:=AHandler;
end;


function GetSuiteStartHandler: TSuiteStartHandler;
begin
  Result:=GlobalSuiteStartHandler;
end;

function GetTestStartHandler: TTestStartHandler;
begin
  Result:=GlobalTestStartHandler;
end;

function GetTestCompleteHandler: TTestCompleteHandler;
begin
  Result:=GlobalTestCompleteHandler;
end;

function GetSuiteCompleteHandler: TSuiteCompleteHandler;
begin
  Result:=GlobalSuiteCompleteHandler;
end;

function GetRunStartHandler: TRunStartHandler;
begin
  Result:=GlobalRunStartHandler;
end;

function GetRunCompleteHandler: TRunCompleteHandler;
begin
  Result:=GlobalRunCompleteHandler;
end;

function GetSuitesetupFailureHandler: TSuitesetupFailureHandler;

begin
  Result:=GlobalSuiteSetupFailureHandler;
end;

function GetSuiteTearDownFailureHandler: TSuiteTearDownFailureHandler;

begin
  Result:=GlobalSuiteTearDownFailureHandler;
end;

procedure ClearTestHooks;
begin
  SetSuiteStartHandler(Nil);
  SetTestStartHandler(Nil);
  SetTestCompleteHandler(Nil);
  SetSuiteCompleteHandler(Nil);
  SetRunStartHandler(Nil);
  SetRunCompleteHandler(Nil);
  SetSuiteSetupFailureHandler(Nil);
  SetSuiteTearDownFailureHandler(Nil);
end;

{ ---------------------------------------------------------------------
  Error management
  ---------------------------------------------------------------------}

Const
   SErrUnknown             = 'Unknown error';
   SErrOK                  = 'OK';
   SErrTestsRunning        = 'Tests already running';
   SErrRegistryEmpty       = 'Testregistry emmpty';
   SErrNoMemory            = 'No memory available';
   SErrNoSuite             = 'No suite available';
   SErrNoSuiteName         = 'No suite name specified';
   SErrSuiteSetupFailed    = 'Suite setup failed';
   SerrSuiteTeardownFailed = 'Suite teardown failed';
   SErrDuplicateSuite      = 'Duplicate suite name';
   SErrSuiteInactive       = 'Attempt to run inactive suite';
   SErrNoTest              = 'No test specified';
   SErrNoTestName          = 'No test name specified';
   SErrDuplicateTest       = 'Duplicate test name specified';
   SErrTestNotInSuite      = 'Test not member of suite';
   SErrTestInactive        = 'Attempt to run inactive test';

function GetTestError: TTestError;
begin
  Result:=CurrentError;
end;

function GetTestErrorMessage: TTestString;

begin
  Case GetTestError of
    teOK:                  Result:=SErrOK;
    teTestsRunning:        Result:=SErrTestsRunning;
    teRegistryEmpty:       Result:=SErrRegistryEmpty;
    teNoMemory:            Result:=SErrNoMemory;
    teNoSuite:             Result:=SErrNoSuite;
    teNoSuiteName:         Result:=SErrNoSuiteName;
    teSuiteSetupFailed:    Result:=SErrSuiteSetupFailed;
    teSuiteTeardownFailed: Result:=SErrSuiteTeardownFailed;
    teDuplicateSuite:      Result:=SErrDuplicateSuite;
    teSuiteInactive:       Result:=SErrSuiteInactive;
    teNoTest:              Result:=SErrNoTest;
    teNoTestName:          Result:=SErrNoTestName;
    teDuplicateTest:       Result:=SErrDuplicateTest;
    teTestNotInSuite:      Result:=SErrTestNotInSuite;
    teTestInactive:        Result:=SErrTestInactive;
  else
    Result:=SErrUnknown;
  end;
end;

function SetTestError(AError: TTestError): TTestError;

begin
  // Forces us to reset the error at all points
  if (AError=teOK) or (CurrentError=teOK)  then
    CurrentError:=AError;
  Result:=CurrentError;
  If (AError<>teOK) and (CurrentErrorAction=eaAbort) then
    Halt(1);
end;

Function CombineError(Original, Additional : TTestError) : TTestError;

begin
  if (teOK=Original) then
    Result:=Additional
  else
    Result:=Original;
end;

function GetErrorAction: TErrorAction;

begin
  Result:=CurrentErrorAction;
end;

function SetErrorAction(AError: TErrorAction): TErrorAction;

begin
  Result:=CurrentErrorAction;
  CurrentErrorAction:=AError;
end;

{ ---------------------------------------------------------------------
  List management.
  ---------------------------------------------------------------------}

Procedure CheckGrowSuiteList(AList : PSuiteList);

Var
  L : Integer;

begin
  L:=Length(AList^.Items);
  if (AList^.Count=L) then
    begin
    if (L=0) then
      L:=InitListLength
    else
      L:=Round(L*3/2);
    SetLength(AList^.Items,L);
    end;
end;

Procedure DoneSuiteList(Var Suites : TSuiteList); forward;

Function DoneSuite(ASuite : PSuite) : TTestError;

Var
  I : Integer;

begin
  SetTestError(teOK);
  With ASuite^ do
    begin
    DoneSuiteList(Suites);
    For I:=0 to Tests.Count-1 do
      Dispose(Tests.Items[i]);
    SetLength(Tests.Items,0);
    Tests.Count:=0;
    end;
  Dispose(ASuite);
  Result:=GetTestError;
end;


Procedure DoneSuiteList(Var Suites : TSuiteList);

Var
  I : Integer;

begin
  For I:=0 to Suites.Count-1 do
    DoneSuite(Suites.Items[i]);
  SetLength(Suites.Items,0);
  Suites.Count:=0;
end;


Procedure CheckGrowTests(Var AList : TTestList);

Var
  L : Integer;

begin
  L:=Length(AList.Items);
  if (L=AList.Count) then
    begin
    if L=0 then
      L:=InitListLength
    else
      L:=Round(L*GrowFactor);
    SetLength(AList.Items,L);
    end;
end;

{ ---------------------------------------------------------------------
  Testsuite Registry management
  ---------------------------------------------------------------------}

function TestRegistryOK: Boolean;
begin
  Result:=Length(TestRegistry.Items)<>0;
end;


Procedure InitSuiteList(Var Suites: TSuiteList);

begin
  Suites.Count:=0;
  CheckGrowSuiteList(@Suites);
end;

Procedure DoSetupTestRegistry;

begin
  if TestRegistry.Count<>0 then exit;
  InitSuiteList(TestRegistry);
end;

function SetupTestRegistry: TTestError;

begin
  Result:=SetTestError(teOK);
  Result:=TearDownTestRegistry;
  if Result=teOK then
    DoSetupTestRegistry;
end;

function TearDownTestRegistry: TTestError;


begin
  SetTestError(teOK);
  DoneSuiteList(TestRegistry);
  Result:=GetTestError;
end;

{ ---------------------------------------------------------------------
  Suite management
  ---------------------------------------------------------------------}


Function CheckInactive : Boolean;

begin
  Result:=(CurrentSuite=Nil);
  If Not Result then
    SetTestError(teTestsRunning);
end;

function AddSuite(const AName: ShortString; AParent: PSuite): PSuite;
begin
  Result:=AddSuite(AName,Nil,Nil,AParent);
end;

function GetSuiteCount(ASuite: PSuiteList; Recurse: Boolean): Integer;

Var
  I : Integer;

begin
  Result:=ASuite^.Count;
  if Recurse then
    For I:=0 to ASuite^.Count-1 do
      Result:=Result+GetSuiteCount(ASuite^.Items[i],True);
end;

function GetSuiteCount(Recurse: Boolean): Integer;
begin
  Result:=GetSuiteCount(PsuiteList(@TestRegistry),Recurse);
end;

Function GetSuiteCount(ASuite : PSuite; Recurse : Boolean = True) : Integer;

begin
  if (ASuite=Nil) then
    Result:=0
  else
    Result:=GetSuiteCount(PSuiteList(@ASuite^.Suites),Recurse);
end;

Function GetSuiteIndex(Const AList : PSuiteList; Const AName: ShortString): Integer;

begin
  Result:=-1;
  if (AList=Nil) then
    begin
    SetTestError(teNoSuite);
    Exit;
    end;
  SetTestError(teOK);
  Result:=AList^.Count-1;
  While (Result>=0) and (AList^.Items[Result]^.Name<>AName) do
    Dec(Result);
end;

function GetSuiteIndex(ASuite : PSuite; const AName: ShortString): Integer;

begin
  if ASuite=Nil then
    Result:=0
  else
    Result:=GetSuiteIndex(PSuiteList(@ASuite^.Suites),AName);
end;

function GetSuiteIndex(const AName: ShortString): Integer;

begin
  Result:=GetSuiteIndex(PSuiteList(@TestRegistry),AName);
end;

Function GetSuite(const AList : PSuiteList; Const AIndex: Integer): PSuite;

begin
  If (AIndex>=0) And (AIndex<AList^.Count) then
    Result:=Alist^.Items[AIndex]
  else
    Result:=Nil;
end;

Function GetSuite(AList : PSuiteList; Const AName : ShortString) : PSuite;

Var
  I,P : Integer;
  N : ShortString;
  L : PSuiteList;

begin
  if AList=Nil then
    Result:=Nil
  else
    begin
    N:=AName;
    L:=AList;
    P:=0;
    For I:=1 to Length(N) do
      if N[i]='.' then
        P:=I;
    if (P>0) then
      begin
      Result:=GetSuite(L,Copy(N,1,P-1));
      if (Result<>Nil) then
        L:=@Result^.Suites
      else
        L:=Nil;
      Delete(N,1,P);
      end;
    I:=GetSuiteIndex(L,N);
    If I<0 then
      Result:=Nil
    else
      Result:=L^.Items[I];
    end;
end;

Function GetSuite(Const AName : ShortString; AParent : PSuite = Nil) : PSuite;

Var
  L : PSuiteList;

begin
  Result:=Nil;
  if (AParent<>Nil) then
    L:=@AParent^.Suites
  else
    L:=@TestRegistry;
  if L<>Nil then
    Result:=GetSuite(L,AName);
end;

function GetSuite(const AIndex: Integer): PSuite;
begin
  Result:=GetSuite(@TestRegistry,AIndex);
end;

function AddSuite(const AName: ShortString; ASetup: TTestSetup;
  ATearDown: TTestTearDown; AParent: PSuite): PSuite;

Var
  S : PSuite;
  L : PSuiteList;
begin
  Result:=Nil;
  SetTestError(teOK);
  If not CheckInactive then
    exit;
  DoSetupTestRegistry;
  if AName='' then
    begin
    SetTestError(teNoSuiteName);
    Exit;
    end;
  S:=GetSuite(AName,AParent);
  if (S<>Nil) then
    begin
    SetTestError(teDuplicateSuite);
    Exit;
    end;
  if AParent<>Nil then
    L:=@AParent^.Suites
  else
    L:=@TestRegistry;
  CheckGrowSuiteList(L);
  New(Result);
  if (Result=Nil) then
    SetTestError(teNoMemory)
  else
    begin
    L^.Items[L^.Count]:=Result;
    FillChar(Result^,Sizeof(TSuite),0);
    Result^.Name:=AName;
    Result^.Setup:=ASetup;
    Result^.Teardown:=ATearDown;
    Result^.Options:=[];
    Result^.Tests.Count:=0;
    Result^.ParentSuite:=AParent;
    CheckGrowTests(Result^.Tests);
    Inc(L^.Count);
    end;
  end;

{ ---------------------------------------------------------------------
  Test management
  ---------------------------------------------------------------------}

function AddTest(const ATestName: ShortString; ARUn: TTestRun;
  const ASuite: PSuite): PTest;
Var
  I : Integer;
  List : PTestList;

begin
  Result:=Nil;
  SetTestError(teOK);
  if not CheckInactive then
    Exit;
  if (ASuite=Nil) then
    SetTestError(teNoSuite)
  else If (ATestName='') then
    SetTestError(teNoTestName)
  else
    begin
    I:=GetTestIndex(ASuite,ATestName);
    if (I<>-1) then
      SetTestError(teDuplicateTest)
    else
      begin
      List:=@ASuite^.Tests;
      CheckGrowTests(List^);
      New(Result);
      if (Result=Nil) then
        SetTestError(teNoMemory)
      else
        begin
        FillChar(Result^,SizeOf(TTest),0);
        Result^.Name:=ATestName;
        Result^.Options:=[];
        Result^.Run:=ARun;
        List^.Items[List^.Count]:=Result;
        Inc(List^.Count);
        end;
      end;
    end;
end;

// Easy access function
function AddTest(const ATestName: ShortString; ARun: TTestRun;
  const ASuiteName: ShortString): PTest;

Var
  S : PSuite;
  SN : ShortString;

begin
  Result:=Nil;
  SetTestError(teOK);
  SN:=ASuiteName;
  if (SN='') then
    SN:=DefaultSuiteName;
  S:=GetSuite(SN);
  if (S=Nil) and (ASuiteName<>'') then
    SetTestError(teNoSuite)
  else
    begin
    If (S=Nil) then
      S:=AddSuite(SN,Nil,Nil);
    If (S<>Nil) then
      Result:=AddTest(ATestName,ARun,S);
    end;
end;

Function GetTestIndex(Const ASuiteIndex: Integer; Const ATestName: ShortString): Integer;
begin
  Result:=GetTestIndex(GetSuite(ASuiteIndex),ATestName);
end;

function GetTestIndex(const ASuiteName: TTestString;
  const ATestName: ShortString): Integer;
begin
  Result:=GetTestIndex(GetSuite(ASuiteName),ATestName);
end;

function GetTestIndex(const ASuite: PSuite; const ATestName: ShortString
  ): Integer;

Var
  A  : TTestArray;

begin
  Result:=-1;
  SetTestError(teOK);
  if (ASuite=Nil) then
    SetTestError(teNoSuite)
  else
    begin
    Result:=ASuite^.Tests.Count-1;
    A:=ASuite^.Tests.Items;
    While (Result>=0) and (A[Result]^.Name<>ATestName) do
      Dec(Result);
    end;
end;

Function GetTest(Const ASuiteIndex: Integer; Const ATestName: ShortString
  ): PTest;
begin
  Result:=GetTest(GetSuite(ASuiteIndex),ATestName);
end;

function GetTestCount(const ASuiteName: TTestString): Integer;
begin
  Result:=GetTestCount(GetSuite(ASuiteName));
end;

function GetTestCount(const ASuite: PSuite): Integer;
begin
  SetTestError(teOK);
  Result:=-1;
  if (ASuite=Nil) then
    SetTestError(teNoSuite)
  else
    Result:=ASuite^.Tests.Count;
end;

function GetTest(const ASuiteName: TTestString; const ATestName: ShortString
  ): PTest;
begin
  Result:=GetTest(GetSuite(ASuiteName),ATestName);
end;

function GetTest(const ASuite: PSuite; const ATestName: ShortString): PTest;

Var
  I,P : Integer;
  N : ShortString;
  S : PSuite;

begin
  N:=ATestName;
  S:=ASuite;
  P:=0;
  For I:=1 to Length(N) do
    if ATestName[I]='.' then
      P:=i;
  If (P>0) then
    begin
    S:=GetSuite(Copy(N,1,P-1),S);
    Delete(N,1,P);
    end;
  if (S=Nil) then
    begin
    SetTestError(teNoSuite);
    Exit;
    end;
  I:=GetTestIndex(S,N);
  If (I=-1) then
    Result:=Nil
  else
    Result:=S^.Tests.items[i];
end;

function GetTest(const ASuite: PSuite; const ATestIndex: Integer): PTest;
begin
  SetTestError(teOK);
  Result:=Nil;
  if (ASuite=Nil) then
    SetTestError(teNoSuite)
  else If (ATestIndex>=0) and (ATestIndex<GetTestCount(ASuite)) then
    Result:=ASuite^.Tests.Items[ATestindex]
end;

function GetTestIndex(const ASuite: PSuite; const ATest: PTest): Integer;

Var
  T : TTestArray;

begin
  SetTestError(teOK);
  Result:=-1;
  if (ASuite=Nil) then
    SetTestError(teNoSuite)
  else if (ATest=Nil) then
    SetTestError(teNoTest)
  else
    begin
    Result:=GetTestCount(ASuite)-1;
    T:=ASuite^.Tests.Items;
    While (Result>=0) and (ATest<>T[Result]) do
      Dec(Result);
    end;
end;

function TestIsInSuite(const ASuite: PSuite; const ATest: PTest): Boolean;

begin
  Result:=GetTestIndex(ASuite,ATest)<>-1;
end;

{ ---------------------------------------------------------------------
  Test result management
  ---------------------------------------------------------------------}

Procedure SetTestResult(Var AResult : TResultRecord; AResultType : TTestResult; AMessage : TTestString; Force : Boolean = False);

Var
  Prev : TTestResult;

begin
  if  Not ((AResult.TestResult=trEmpty) or Force) then
    Exit;
  Prev:=AResult.TestResult;
  AResult.TestResult:=AResultType;
  AResult.TestMessage:=AMessage;
  // Only increas in case of switch from non-error -> error
  if (Prev In [trEmpty,trOK]) and not (AResult.TestResult In [trEmpty,trOK])  And (AResult.Test<>Nil) then
    if AResult.TestResult=trTestIgnore then
      Inc(CurrentRun.TestsIgnored)
    else
      Inc(CurrentRun.TestsFailed);
end;

Procedure SetTestResult(AResultType : TTestResult; AMessage : TTestString; Force : Boolean = False);

begin
  if Assigned(CurrentResult) then
    SetTestResult(CurrentResult^,AResultType,AMessage,Force);
end;

Function DoAssert(AResult : Boolean; ACondition : TTestString) : Boolean;

begin
  Inc(CurrentRun.AssertCount);
  Result:=AResult;
  if (Not Result) and (Assigned(CurrentResult)) then
    SetTestResult(CurrentResult^,trAssertFailed,ACondition);
end;

function CountResults(Results: PResultRecord): Integer;
begin
  Result:=0;
  While Results<>Nil do
    begin
    Inc(Result);
    Results:=Results^.NextResult;
    end;
end;

function Ignore(const Msg: TTestString): Boolean;

begin
  SetTestResult(CurrentResult^,trTestIgnore,Msg);
  Result:=False;
end;

function Fail(const Msg: TTestString): Boolean;

begin
  Result:=DoAssert(False,Msg);
end;

function FailExit(const Msg: TTestString): Boolean;
begin
  Result:=False;
  Raise EFail.Create(Msg);
end;

function IgnoreExit(const Msg: TTestString): Boolean;
begin
  Result:=False;
  Raise EIgnore.Create(Msg);
end;

function AssertPassed(AMessage: TTestString): Boolean;
begin
  Result:=DoAssert(True,'');
  if Assigned(CurrentResult) then
    SetTestResult(CurrentResult^,trOK,AMessage);
end;

function AssertTrue(const AMessage: TTestString; ACondition: Boolean): Boolean;
begin
  DoAssert(ACondition,AMessage);
  Result:=ACondition;
end;

Function ExpectMessage(AExpect,AActual : TTestString; Quote : Boolean = False) : TTestString;

begin
  if Quote then
    begin
    AExpect:='"'+AExpect+'"';
    AActual:='"'+AActual+'"';
    end;
  Result:=SExpected+': '+AExpect+' '+SActual+': '+AActual;
end;

Function ExpectMessage(AExpect,AActual : UnicodeString; Quote : Boolean = False) : UnicodeString;

begin
  if Quote then
    begin
    AExpect:='"'+AExpect+'"';
    AActual:='"'+AActual+'"';
    end;
  Result:=SExpected+': '+AExpect+' '+SActual+': '+AActual;
end;

function AssertFalse(const AMessage: TTestString; ACondition: Boolean): Boolean;
begin
  Result:=AssertTrue(AMessage,Not ACondition);
end;

function AssertEquals(AMessage: TTestString; const AExpected,
  AActual: ShortString): Boolean;
begin
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(AExpected,AActual,True),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected,
  AActual: AnsiString): Boolean;
begin
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(AExpected,AActual,True),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected,
  AActual: UnicodeString): Boolean;
begin
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(AExpected,AActual,True),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Boolean
  ): Boolean;

Const
   BStrs : Array[Boolean] of TTestString = ('False','True');

begin
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(BStrs[AExpected],BStrs[AActual],False),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Shortint
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Byte
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Smallint
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Word
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Longint
  ): Boolean;

Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Cardinal
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Int64
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: QWord
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Currency
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Double;
  ADelta: Double): Boolean;

Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  If ADelta=0 then
    ADelta:=DefaultDoubleDelta;
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),Abs(AExpected-AActual)<ADelta);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Single;
  ADelta: Single): Boolean;

Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  If ADelta=0 then
    ADelta:=DefaultSingleDelta;
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),Abs(AExpected-AActual)<ADelta);
end;

function AssertEquals(AMessage: TTestString; const AExpected,
  AActual: Extended; ADelta: Extended): Boolean;

Var
  SE,SA : TTestString;

begin
  Str(AExpected,SE);
  Str(AActual,SA);
  If ADelta=0 then
    ADelta:=DefaultExtendedDelta;
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),Abs(AExpected-AActual)<ADelta);
end;

function AssertNull(AMessage: TTestString; const AValue: Pointer): Boolean;
begin
  Result:=AssertEquals(AMessage,Nil,AValue);
end;

function AssertNotNull(AMessage: TTestString; const AValue: Pointer): Boolean;
begin
  Result:=AssertDiffers(AMessage,Nil,AValue);
end;

Function PointerToStr(P : Pointer) : TTestString;

begin
  if P=Nil then
    Result:='Nil'
  else
    Result:=HexStr(P);
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: Pointer
  ): Boolean;
Var
  SE,SA : TTestString;

begin
  SE:=PointerToStr(AExpected);
  SA:=PointerToStr(AActual);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertDiffers(AMessage: TTestString; const AExpected, AActual: Pointer
  ): Boolean;

Var
  SE,SA : TTestString;

begin
  SE:=PointerToStr(AExpected);
  SA:=PointerToStr(AActual);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected<>AActual)
end;

function AssertEquals(AMessage: TTestString; const AExpected, AActual: TClass
  ): Boolean;

  Function CN (AClass : TClass) : ShortString;

  begin
    If Assigned(AClass) then
      Result:=AClass.ClassName
    else
      Result:='Nil'
  end;

Var
  SE,SA : ShortString;

begin
  SE:=CN(AExpected);
  SA:=CN(AActual);
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertInheritsFrom(AMessage: TTestString; const AChild,
  AParent: TObject): Boolean;

Var
  CC,CP : TClass;

begin
  if Assigned(AParent) then
    CP:=AParent.ClassType
  else
    CP:=Nil;
  if Assigned(AChild) then
    CC:=AChild.ClassType
  else
    CC:=Nil;
  Result:=AssertInheritsFrom(AMessage,CC,CP)
end;

function AssertInheritsFrom(AMessage: TTestString; const AChild, AParent: TClass
  ): Boolean;
begin
  Result:=AssertNotNull(AMessage,AChild);
  if Result then
    begin
    Result:=AssertNotNull(AMessage,AParent);
    if Result then
      Result:=AssertTrue(AMessage,AChild.InheritsFrom(AParent));
    end;
end;

function AssertSame(AMessage: TTestString; const AExpected, AActual: TObject
  ): Boolean;

  Function CN (AClass : TObject) : ShortString;

  begin
    If Assigned(ACLass) then
      Result:=AClass.ClassName
    else
      Result:='Nil'
  end;

Var
  SE,SA : ShortString;

begin
  SE:=CN(AExpected);
  if AExpected<>Nil then
    SE:=SE+'('+HexStr(AExpected)+')';
  SA:=CN(AActual);
  if AActual<>Nil then
    SA:=SA+'('+HexStr(AActual)+')';
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected=AActual);
end;

function AssertNotSame(AMessage: TTestString; const AExpected, AActual: TObject
  ): Boolean;

  Function CN (AClass : TObject) : ShortString;

  begin
    If Assigned(ACLass) then
      Result:=AClass.ClassName
    else
      Result:='Nil'
  end;

Var
  SE,SA : ShortString;

begin
  SE:=CN(AExpected);
  if AExpected<>Nil then
    SE:=SE+'('+HexStr(AExpected)+')';
  SA:=CN(AActual);
  if AActual<>Nil then
    SA:=SA+'('+HexStr(AActual)+')';
  Result:=AssertTrue(AMessage+'. '+ExpectMessage(SE,SA),AExpected<>AActual);
end;

function AssertException(const AMessage: string; AExceptionClass: TClass;
  ARun: TProcedure): boolean;

Var
  EC : TClass;

begin
  EC:=Nil;
  Try
    ARun();
  except
    On E : TObject do
      EC:=E.ClassType;
  end;
  Result:=AssertNotNull(AMessage,EC);
  if Result then
    Result:=AssertEquals(AMessage,AExceptionClass,EC);
end;

function AssertException(const AMessage: string; AExceptionClass: TClass;
  ARun: TTestRun): boolean;
Var
  EC : TClass;
  S : TTestString;
begin
  EC:=Nil;
  S:='';
  Try
    S:=ARun();
  except
    On E : TObject do
      EC:=E.ClassType;
  end;
  Result:=AssertNotNull(AMessage,EC) and AssertEquals(AMessage,'',S);
  if Result then
    Result:=AssertEquals(AMessage,AExceptionClass,EC);
end;

function ExpectException(AMessage: TTestString; AClass: TClass): Boolean;
begin
  Result:=SetTestError(teOK)=teOK;
  Result:=AssertTrue(AMessage,Result and Assigned(CurrentResult) and (CurrentResult^.TestResult in [trEmpty,trOK]));
  If Result then
    begin
    CurrentResult^.ExpectException:=AClass;
    CurrentResult^.TestMessage:=AMessage;
    end;
end;

{ ---------------------------------------------------------------------
  Auxiliary test run routines
  ---------------------------------------------------------------------}

// Reset run summary results
Procedure FreeResultRecord(P : PResultRecord; Recurse : Boolean);

Var
  N : PResultRecord;

begin
  if Not Assigned(P) then
    exit;
  Repeat
    Finalize(p^);
    N:=P^.NextResult;
    If Recurse then
      FreeResultRecord(P^.ChildResults,Recurse);
    FreeMem(P);
    P:=N;
  Until (P=Nil);
end;

Procedure ResetRun(Var ARun : TRunSummary);

begin
  FreeResultRecord(ARun.Results.ChildResults,True);
  ARun:=Default(TRunSummary);
  ARun.Results:=Default(TResultRecord);
  CurrentSuiteResult:=@ARun.Results;
  CurrentResult:=@ARun.Results;
end;

// Check if the test run must be continued ?
Function ContinueTest(AResult : TTestError) : Boolean;

begin
  Result:=(AResult=teOK) or (CurrentErrorAction=eaIgnore);;
end;

// Set current test result

Function AllocateCurrentSuiteResult(ASuite: PSuite; IsChild : Boolean): TTestError;

Var
  P : PResultRecord;

begin
  Result:=SetTestError(teOK);
  New(P);
  If (P=Nil) then
    SetTestError(teNoMemory)
  else
    begin
    P^:=Default(TResultRecord);
    p^.Suite:=ASuite;
    If IsChild then
      begin
      CurrentSuiteResult^.ChildResults:=P;
      P^.ParentResult:=CurrentSuiteResult;
      end
    else
      Begin
      CurrentSuiteResult^.NextResult:=P;
      P^.ParentResult:=CurrentSuiteResult^.ParentResult;
      end;
    CurrentSuiteResult:=P;
    CurrentResult:=CurrentSuiteResult;
    end;
end;

Function AllocateCurrentResult(ASuite: PSuite; ATest: PTest): TTestError;

Var
  N,P : PResultRecord;


begin
  Result:=SetTestError(teOK);
  New(P);
  If (P=Nil) then
    SetTestError(teNoMemory)
  else
    begin
    P^:=Default(TResultRecord);
    P^.TestResult:=trEmpty;
    P^.Suite:=ASuite;
    P^.Test:=ATest;
    P^.ExpectException:=Nil;
    // Hook up in tree.
    N:=CurrentSuiteResult^.ChildResults;
    if N=Nil then
      begin
      CurrentSuiteResult^.ChildResults:=P;
      P^.ParentResult:=CurrentSuiteResult;
      end
    else
      begin
      While (N^.NextResult<>Nil) do
        N:=N^.NextResult;
      N^.NextResult:=P;
      P^.ParentResult:=N^.ParentResult;
      end;
    CurrentResult:=P;
    end;
end;

{ ---------------------------------------------------------------------
  Protected run of hook handlers. Catch exceptions and report them.
  ---------------------------------------------------------------------}

// Run start hook
Function RunGLobalRunStartHandler(Suites : TSuiteArray) : TTestError;

begin
  Result:=SetTestError(teOK);
  Try
    GlobalRunStartHandler(Suites);
  except
    On E : TObject do
      begin
      CurrentResult:=@CurrentRun.Results;
      SetTestResult(trHandlerError,E.ToString,True);
      Result:=(SetTestError(teRunStartHandler));
      end;
  end;
end;

// Run complete hook
Function RunGLobalRunCompleteHandler(Run : TRunSummary) : TTestError;

begin
  Result:=SetTestError(teOK);
  if Assigned(GlobalRunCompleteHandler) then
    Try
      GlobalRunCompleteHandler(Run);
    except
      On E : TObject do
        begin
        CurrentResult:=@CurrentRun.Results;
        SetTestResult(trHandlerError,E.ToString,False);
        Result:=(SetTestError(teRunCompleteHandler));
        end;
    end;
end;

// Run suite start hook
Function RunGlobalSuiteStartHandler(ASuite : PSuite) : TTestError;

begin
  Result:=SetTestError(teOK);
  If Assigned(GlobalSuiteStartHandler) then
    try
      GlobalSuiteStartHandler(ASuite);
    except
      On E : EIgnore do
       SetTestResult(trTestIgnore,E.ToString);
      On E : EFail do
       SetTestResult(trAssertFailed,E.ToString);
      On E : TObject do
       SetTestResult(trHandlerError,E.ToString);
    end;
end;

// Run suite complete hook
Function RunGlobalSuiteCompleteHandler(ASuite : PSuite; SuiteResult : PResultRecord) : TTestError;

Var
  C : PresultRecord;

begin
  Result:=SetTestError(teOK);
  If Assigned(GlobalSuiteCompleteHandler) then
    begin
    C:=CurrentResult;
    CurrentResult:=SuiteResult;
    try
      GlobalSuiteCompleteHandler(ASuite,SuiteResult);
    except
      On E : EIgnore do
       SetTestResult(trTestIgnore,E.ToString);
      On E : EFail do
        SetTestResult(SuiteResult^,trAssertFailed,E.ToString);
      On E : TObject do
        SetTestResult(SuiteResult^,trHandlerError,E.ToString);
    end;
    CurrentResult:=C;
    end;
end;

// Run suite setup
Function RunSuiteSetup(ASuite : PSuite; SuiteResult : PResultRecord) : TTestError;

var
  S : TTestString;

begin
  Result:=SetTestError(teOK);
  if Not Assigned(ASuite^.Setup) then
     exit;
  S:='';
  try
    S:=ASuite^.Setup();
  Except
    On E : TObject Do
      S:=E.ToString;
  end;
  if (S<>'') then
    begin
    SetTestResult(SuiteResult^,trSuiteSetupFailed,S,True);
    Result:=SetTestError(teSuiteSetupFailed);
    Inc(CurrentRun.SuitesFailed);
    If Assigned(GlobalSuiteSetupFailureHandler) then
      GlobalSuiteSetupFailureHandler(ASuite,S);
    end
end;

// Run suite teardown
Function RunSuiteTearDown(ASuite : PSuite; SuiteResult : PResultRecord) : TTestError;

var
  S : TTestString;
  C : PresultRecord;

begin
  Result:=SetTestError(teOK);
  if Not Assigned(ASuite^.Teardown) then
     exit;
  C:=CurrentResult;
  CurrentResult:=SuiteResult;
  S:='';
  try
    S:=ASuite^.TearDown();
  Except
    On E : TObject Do
      S:=E.ToString;
  end;
  if (S<>'') then
    begin
    SetTestResult(SuiteResult^,trSuiteTearDownFailed,S,True);
    Result:=SetTestError(teSuiteTeardownFailed);
    Inc(CurrentRun.SuitesFailed);
    If Assigned(GlobalSuiteTearDownFailureHandler) then
      GlobalSuiteTearDownFailureHandler(ASuite,S);
    end;
  CurrentResult:=C;
end;

// Run test handler
Function RunTestHandler(R : TTestRun) : String;

Var
  EC : TClass;
  EM : TTestString;


begin
  Result:='';
  EC:=Nil;
  EM:='';
  try
    Result:=R();
  except
    On E : TObject do
      begin
      EC:=E.ClassType;
      EM:=E.TOString;
      end;
  end;
  // Treat exception.
  if (EC<>CurrentResult^.ExpectException) then
    begin
    if (CurrentResult^.ExpectException=Nil) then
      Result:=EM
    else
      With CurrentResult^ do
        if (EC=Nil) then
          Result:=TestMessage+' '+ExpectMessage(ExpectException.ClassName,'Nil')
        else
          Result:=TestMessage+' '+ExpectMessage(ExpectException.ClassName,EC.ClassName);
    end;
end;

{ ---------------------------------------------------------------------
  Running tests
  ---------------------------------------------------------------------}

Function RunSingleTest(T : PTest) : TTestError;

Type
  TTestStage = (tsStartHandler,tsSetup,tsRun,tsTearDown,tsCompleteHandler);

Const
  Prefixes : Array[TTestStage] of TTestString =
     ('Test start handler','Test Setup','','Test TearDown','Test complete handler');
  Errors : Array[TTestStage] of TTestResult =
     (trHandlerError,trSuiteSetupFailed, trTestError,trSuiteTearDownFailed,trHandlerError);

Var
  S : TTestString;
  Stage : TTestStage;
  StartTime : TDateTime;
  CurrentAsserts : Integer;

begin
  SetTestError(teOK);
  Assert(CurrentSuite<>Nil);
  CurrentTest:=T;
  try
    CurrentAsserts:=CurrentRun.AssertCount;
    Result:=AllocateCurrentResult(CurrentSuite,T);
    if (Result<>teOK) then
      Exit;
    Stage:=tsStartHandler;
    // We don't use a protected method. We use 1 try/except block that keeps track of the 'stage'.
    If Assigned(GlobalTestStartHandler) then
      GlobalTestStartHandler(T,CurrentSuite);
    if (soSetupTearDownPerTest in CurrentSuite^.Options) then
      begin
      Stage:=tsSetup;
      Result:=RunSuiteSetup(CurrentSuite,CurrentResult);
      end;
    if (Result=teOK) then
      If Not (toInactive in T^.Options) then
        begin
        StartTime:=GetTimeFromHook;
        try
          Stage:=tsRun;
          S:=RunTestHandler(T^.Run); // Protect and handle exception.
          if (S<>'') then
            Fail(S)
          else if (CurrentResult^.TestResult=trEmpty) then
            if (CurrentAsserts=CurrentRun.AssertCount) and RequirePassed then
              Inc(CurrentRun.TestsUnimplemented)
            else
              SetTestResult(trOK,'');
        Finally
          CurrentResult^.ElapsedTime:=GetTimeDiff(StartTime);
          Inc(CurrentRun.TestsRun);
        end;
        if (soSetupTearDownPerTest in CurrentSuite^.Options) then
          begin
          Stage:=tsTearDown;
          Result:=RunSuiteTearDown(CurrentSuite,CurrentResult);
          end;
        Stage:=tsCompleteHandler;
        end
      else
        begin
        Inc(CurrentRun.TestsInactive);
        SetTestResult(trTestInactive,'',True);
        Result:=SetTestError(teTestInactive);
        end;
    if Assigned(GlobalTestCompleteHandler) then
      GlobalTestCompleteHandler(T,CurrentSuite,CurrentResult);
  except
    On E : TObject do
      begin
      S:=Prefixes[Stage];
      if (S<>'') then S:='['+S+'] ';
      S:=S+E.Tostring;
      SetTestResult(CurrentResult^,Errors[Stage],S,True);
      end;
  end;
  CurrentTest:=Nil;
end;


// Internal, run a single suite, collect results in RunSummary.
Function RunSingleSuite(ASuite : PSuite; isChild : Boolean) : TTestError;

Type
  TSuiteStage = (ssStartHandler,ssSetup,ssRun,ssTearDown,ssEndHandler);

Const
  Prefixes : Array [TSuiteStage] of TTestString =
       ('Start handler','Setup','','Teardown','End Handler');
  StageErrors :  Array [TSuiteStage] of TTestResult =
       (trHandlerError,trSuiteSetupFailed,trTestError,trSuiteTearDownFailed,trHandlerError);

Var
  S : TTestString;
  T : PTest;
  Stage : TSuiteStage;
  I : Integer;
  StartTime : TDateTime;
  R2 : TTestError;
  OldCurrentSuite : PSuite;
  SuiteResult : PResultRecord;

begin
  if AllocateCurrentSuiteResult(ASuite,IsChild)<>teOK then
    exit;
  SetTestError(teOK);
  OldCurrentSuite:=CurrentSuite;
  SuiteResult:=CurrentSuiteResult;
  CurrentSuite:=ASuite;
  try
    Result:=teOK;
    Stage:=ssStartHandler;
    RunGlobalSuiteStartHandler(ASuite);
    // First, run all sub suites.
    If (soInactive in ASuite^.Options) then
      Inc(CurrentRun.SuitesInactive)
    else
      begin
      StartTime:=GetTimeFromHook;
      S:='';
      try
        if not (soSetupTearDownPerTest in ASuite^.Options) then
          begin
          Stage:=ssSetup;
          Result:=RunSuiteSetup(ASuite,SuiteResult);
          end;
        Stage:=ssRun;
        For I:=0 to Asuite^.Suites.Count-1 do
          If (Result=teOK) or (CurrentErrorAction=eaIgnore) then
            Result:=RunSingleSuite(ASuite^.Suites.Items[i],I=0);
        // Reset current result
        CurrentSuiteResult:=SuiteResult;
        CurrentResult:=SuiteResult;
        For I:=0 to Asuite^.Tests.Count-1 do
          If (Result=teOK) or (CurrentErrorAction=eaIgnore) then
            begin
            T:=Asuite^.Tests.Items[i];
            if Not (toInactive in T^.Options) then
              Result:=RunSingleTest(T)
            else
              Inc(CurrentRun.TestsInactive)
            end;
        Stage:=ssTeardown;
        Result:=RunSuiteTearDown(ASuite,SuiteResult);
      Finally
         Inc(CurrentRun.SuitesRun);
         SuiteResult^.ElapsedTime:=GetTimeDiff(StartTime);
      end;
      Stage:=ssEndHandler;
      R2:=RunGLobalSuiteCompleteHandler(ASuite,SuiteResult);
      if (Result=teOK) and (R2<>teOK) then
        Result:=R2;
      SetTestResult(SuiteResult^,trOK,'',False);
      end;
  except
    On E : TObject do
      begin
      S:=Prefixes[Stage];
      if (S<>'') then S:='['+S+'] ';
      S:=S+E.Tostring;
      SetTestResult(SuiteResult^,StageErrors[Stage],S,True);
      end;
  end;
  CurrentSuite:=OldCurrentSuite;
end;


// Internal. At this point, ASuite and ATest are valid.
Function DoRunTest(ASuite: PSuite; ATest: PTest): TTestError;

Var
  A : TSuiteArray;
  StartTime : TDateTime;
  SuiteResult : PResultRecord;
begin
  A:=[];
  ResetRun(CurrentRun);
  if AllocateCurrentSuiteResult(ASuite,True)<>teOK then
    exit;
  Result:=SetTestError(teOK);
  SuiteResult:=CurrentResult;
  If Assigned(GlobalRunStartHandler) then
    begin
    SetLength(A,1);
    A[0]:=ASuite;
    Result:=RunGlobalRunStartHandler(A);
    SetLength(A,0);
    If not ContinueTest(Result) then
      exit;
    end;
  if (soInactive in ASuite^.Options) then
    begin
    SetTestResult(trSuiteInactive,'',True);
    Inc(CurrentRun.SuitesInactive);
    Inc(CurrentRun.SuitesFailed);
    RunGlobalRunCompleteHandler(CurrentRun); // Ignore status
    Exit(SetTestError(teSuiteInactive));
    end;
  StartTime:=GetTimeFromHook;
  if Not ContinueTest(Result)then
    begin
    Result:=CombineError(Result,RunGlobalRunCompleteHandler(CurrentRun));
    exit;
    end;
  CurrentSuite:=ASuite;
  try
    Result:=RunGlobalSuiteStartHandler(ASuite);
    if ContinueTest(Result) then
      begin
      Result:=RunSuiteSetup(ASuite,SuiteResult);
      if ContinueTest(Result) then
        begin
        Result:=CombineError(Result,RunSingleTest(ATest));
        Result:=CombineError(Result,RunSuiteTearDown(ASuite,SuiteResult));
        end;
      end;
  finally
     SetTestResult(SuiteResult^,trOK,'');
     Inc(CurrentRun.SuitesRun);
     CurrentSuite:=Nil;
  end;
  Result:=CombineError(Result,RunGlobalSuiteCompleteHandler(ASuite,SuiteResult));
  CurrentRun.ElapsedTime:=GetTimeDiff(StartTime);
  Result:=CombineError(Result,RunGlobalRunCompleteHandler(CurrentRun));
end;


// Internal. At this point, ASuite is valid.
Function DoRunSuite(ASuite: PSuite): TTestError;

Var
  A : TSuiteArray;
  StartTime : TDateTime;
begin
  A:=[];
  SetTestError(teOK);
  ResetRun(CurrentRun);
  If Assigned(GlobalRunStartHandler) then
    begin
    SetLength(A,1);
    A[0]:=ASuite;
    Result:=RunGlobalRunStartHandler(A);
    SetLength(A,0);
    if not ContinueTest(Result) then
      Exit;
    end;
  SetTestError(teOK);
  StartTime:=GetTimeFromHook;
  Result:=teOK;
  Result:=RunSingleSuite(ASuite,True);
  CurrentRun.ElapsedTime:=GetTimeDiff(StartTime);
  Result:=CombineError(Result,RunGlobalRunCompleteHandler(CurrentRun));
end;

function RunSuite(ASuite: PSuite): TTestError;

begin
  SetTestError(teOK);
  if (ASuite=Nil) then
    Result:=SetTestError(teNoSuite)
  else
    Result:=DoRunSuite(ASuite);
end;

function RunSuite(const ASuiteName: ShortString): TTestError;
begin
  Result:=RunSuite(GetSuite(ASuiteName));
end;

function RunSuite(ASuiteIndex: Integer): TTestError;
begin
  Result:=RunSuite(GetSuite(ASuiteIndex))
end;

Function RunTest(ASuiteIndex: Integer; Const ATestName: TTestString): TTestError;

Var
  S : PSuite;

begin
  S:=GetSuite(ASuiteIndex);
  Result:=RunTest(S,GetTest(S,ATestName));
end;

Function RunTest(ASuite: PSuite; ATestIndex: Integer): TTestError;
begin
  Result:=RunTest(ASuite,GetTest(ASuite,ATestIndex));
end;

function RunTest(ASuite: PSuite; const ATestName: ShortString): TTestError;
begin
  Result:=RunTest(ASuite,GetTest(ASuite,ATestName));
end;

function RunTest(const ASuiteName: ShortString; const ATestName: ShortString
  ): TTestError;

Var
  S : PSuite;
begin
  S:=GetSuite(ASuiteName);
  Result:=RunTest(S,GetTest(S,ATestName));
end;


function RunTest(ASuite: PSuite; ATest: PTest): TTestError;
begin
  Result:=SetTestError(teOK);
  ProcessSysCommandline;
  if (ASuite=Nil) then
    Result:=SetTestError(teNoSuite)
  else if (ATest=Nil) then
    Result:=SetTestError(teNoTest)
  else if not TestIsInSuite(ASuite,ATest) then
    Result:=SetTestError(teTestNotInSuite)
  else
    Result:=DoRunTest(ASuite,ATest);
end;

Procedure SysHalt;

begin
  if CurrentRun.TestsFailed<>0 then
    Halt(1)
  else
    Halt(0);
end;

Procedure DoRunSysTests(S : PSuite; T : PTest); forward;

procedure RunTest(ARun: TTestRun);

begin
  ProcessSysCommandLine;
  if ARun=Nil then
    Halt(2);
  if (AddTest('Global',ARun)=Nil) then
    Halt(3);
  DoRunSysTests(Nil,Nil);
end;

function GetCurrentRun: TRunSummary;
begin
  Result:=CurrentRun;
end;

function GetCurrentSuite: PSuite;
begin
  Result:=CurrentSuite;
end;

function GetCurrentTest: PTest;
begin
  Result:=CurrentTest;
end;

function GetCurrentResult: PResultRecord;
begin
  Result:=CurrentResult;
end;


function RunAllTests: TTestError;

Var
  I : Integer;
  A : TSuiteArray;
  StartTime : TDateTime;
begin
  A:=[];
  Result:=SetTestError(teOK);
  ResetRun(CurrentRun);
  StartTime:=GetTimeFromHook;
  If Assigned(GlobalRunStartHandler) then
    begin
    // Array of actual size.
    SetLength(A,TestRegistry.Count);
    For I:=0 to TestRegistry.Count-1 do
      A[I]:=TestRegistry.Items[i];
    GlobalRunStartHandler(A);
    SetLength(A,0);
    end;
  If (TestRegistry.Count=0) then
    Result:=SetTestError(teRegistryEmpty)
  else
    begin
    I:=0;
    While (I<TestRegistry.Count) and ContinueTest(Result) do
      begin
      Result:=CombineError(Result,RunSingleSuite(TestRegistry.Items[I],I=0));
      Inc(I);
      end;
    end;
  CurrentRun.ElapsedTime:=GetTimeDiff(StartTime);
  Result:=CombineError(Result,RunGlobalRunCompleteHandler(CurrentRun));
end;

{ ---------------------------------------------------------------------
  Systemm hooks
  ---------------------------------------------------------------------}

Const

  // Run
  STestRun = 'Test run';
  SRunSummary   = 'Run summary';

  // Suites
  SSuites = 'Suites';
  SSuite = 'Suite';
  SSummary = 'summary';
  SSuitesSummary  = 'Suites summary';

  // Tests
  STests = 'Tests';
  STest  = 'Test';
  STestsSummary   = 'Tests summary';

  // Counts
  SInactiveCount = 'Inactive';
  SIgnoredCount  = 'Ignored';
  SRunCount      = 'Run';
  SFailedCount   = 'Failed';
  SUnimplementedCount = 'Unimplemented';

  // test Status/Result
  SPassed = 'Passed';
  SIgnored = 'Ignored';
  SFailed = 'Failed';
  SError = 'Error';
  SInactive = 'Inactive';
  SNotImplemented = 'Not implemented';
  SUnknown = 'Unknown';
  SErrorMessage = 'Error message';
  SSuiteSetupFailed = 'Suite setup failed';
  SSuiteTearDownFailed = 'Suite setup failed';
  // Elapsed time
  STime = 'Time';

  SUsage = 'Usage:';
  SHelpL = '-l --list         list all tests (observes -s)';
  SHelpF = '-f --failures     only show names and errors of failed tests';
  SHelpH = '-h --help         this help message';
  SHelpN = '-n --normal       normal log level';
  SHelpO = '-o --output=file  log output file name (default is standard output)';
  SHelpQ = '-q --quiet        Do not display messages ';
  SHelpS = '-s --suite=name   Only run/list tests in given suite';
  SHelpT = '-t --test=name    Only run/list tests matching given test (requires -s)';
  SHelpV = '-v --verbose      Verbose output logging';
  SHelpHasTime = 'This binary has support for displaying time';
  SHelpNoTime = 'This binary has no support for displaying time';
  SHelpExitCodes = 'Possible exit codes:';
  SHelpExit0 = '0 - All actions (tests) completed successfully';
  SHelpExit1 = '1 - All tests were run, but some tests failed.';
  SHelpExit2 = '2 - An empty test function was given to runtest';
  SHelpExit3 = '3 - The requested suite was not found';
  SHelpExit4 = '4 - The requested test was not found';
  SHelpExit5 = '5 - An unexpected error occurred in the testsuite';

Type
  TRunMode = (rmHelp,rmList,rmTest);

Var
  FSys : Text;
  CurrentRunMode : TSysRunVerbosity;
  SysSuite : PSuite;
  SysOutputFileName : ShortString;
  SysTestName : ShortString;
  SysSuiteName : ShortString;
  SysRunMode : TRunMode;
  SysSuiteIndent : String;

Procedure SysSuiteStartHandler (ASuite : PSuite);

begin
  if (ASuite<>SysSuite) then
    begin
    SysSuiteIndent:=SysSuiteIndent+'  ';
    Write(FSys,SysSuiteIndent,SSuite,' ',ASuite^.Name,':');
    if CurrentRunMode=rvVerbose then
      Writeln(FSys,' (',ASuite^.Tests.Count,' ',STests+')')
    else
      Writeln(FSys);
    SysSuite:=ASuite;
    end;
end;

Procedure SysTestStartHandler (ATest : PTest; ASuite : PSuite);

begin
  if CurrentRunMode in [rvQuiet,rvFailures] then
    Exit;
  Write(FSys,SysSuiteIndent+'  ',STest,' ',ATest^.Name,': ');
end;

function SysTimeString(MSec: Integer): ShortString;

Var
  S : ShortString;

begin
  S:='';
  Str(Msec mod 1000,Result);
  MSec:=Msec div 1000;
  If (Msec=0) then
    Result:='0.'+Result
  else
    begin
    Str(Msec mod 60,S);
    Result:=S+'.'+Result;
    Msec:=Msec div 60;
    If (Msec<>0) then
      begin
      Str(Msec mod 60,S);
      Result:=S+':'+Result;
      Msec:=Msec div 60;
      If (Msec<>0) then
        begin
        Str(Msec,S);
        Result:=S+':'+Result;
        end;
      end;
    end;
  Result:=STime+': '+Result;
end;

Procedure SysTestCompleteHandler (ATest: PTest; ASuite : PSuite; Const AResultRecord : PResultRecord);

Var
  S : TTestString;
  F,O : Boolean;
  TR : TTestResult;

begin
  if (CurrentRunMode=rvQuiet) then exit;
  F:=CurrentRunMode=rvFailures;
  O:=False;
  S:=AResultRecord^.TestMessage;
  TR:=AResultRecord^.TestResult;
  Case TR of
    trEmpty :
      if not F then
        Write(FSys,SNotImplemented);
    trOK :
      if not F then
        Write(FSys,SPassed);
    trTestIgnore:
       Write(FSys,SIgnored,' (',S,')');
    trSuiteSetupFailed,
    trSuiteTearDownFailed,
    trAssertFailed,
    trTestError,
    trHandlerError:
      begin
      if F then
        Write(FSys,STest,' ',ASuite^.Name,'.',ATest^.Name,': ');
      if TR in [trTestError,trHandlerError] then
        Write(FSys,SError)
      else
        Write(FSys,SFailed);
      Write(FSys,' (',SErrorMessage,': ',S,')');
      O:=True;
      end;
    trTestInactive:
      if not F then
        Write(FSys,SInactive);
  else
    if not F then
      Write(FSys,SUnknown,' : ',AResultRecord^.TestMessage);
  end;
  if (not F) and HaveTimeHook then
    Writeln(FSys,' ',SysTimeString(AResultRecord^.ElapsedTime))
  else if (O or Not F) then
    Writeln(FSys);
end;

Procedure GetResultStats(AResults : PResultRecord; Var Stats : TSuiteStats);

begin
  If AResults^.Test<>Nil then
    begin
    Inc(Stats.TestsRun);
    Case AResults^.TestResult of
      trEmpty : Inc(Stats.TestsUnimplemented);
      trAssertFailed : Inc(Stats.TestsFailed);
      trTestInactive : Inc(Stats.TestsInactive);
      trTestIgnore : Inc(Stats.TestsIgnored);
      trTestError : Inc(Stats.TestsError);
    else
      // Do nothing, silence compiler warning
    end;
    end;
end;

Procedure DoGetSuiteStats(AResults : PResultRecord; Var Stats : TSuiteStats);

Var
  R : PResultRecord;

begin
  if AResults^.Test<>Nil then
     Exit;
  Inc(Stats.Suites);
  R:=AResults^.ChildResults;
  While R<>Nil do
    begin
    if R^.Test=Nil then
      DoGetSuiteStats(R,Stats)
    else
      GetResultStats(R,Stats);
    R:=R^.NextResult;
    end;
end;

Procedure GetSuiteStats(AResults : PResultRecord; Out Stats : TSuiteStats);

begin
  Stats:=Default(TSuiteStats);
  DoGetSuiteStats(AResults,Stats);
end;

Procedure SysSuiteCompleteHandler (ASuite : PSuite; Const AResults : PResultRecord);

Var
  Stats : TSuiteStats;

begin
  if CurrentRunMode in [rvQuiet,rvFailures] then
    exit;
  Write(FSys,SysSuiteIndent,SSuite,' ',ASuite^.Name,' ',SSummary,': ');
  GetSuiteStats(AResults,Stats);
  With Stats do
    begin
    Write(FSys,SRunCount,': ',TestsRun,' ',SFailedCount,': ',TestsFailed,' ',SInactiveCount,': ',TestsInactive,' ',SIgnoredCount,': ',TestsIgnored);
    if RequirePassed then
      Write(FSys,' ',SUnimplementedCount,': ',TestsUnimplemented);
    end;
  if HaveTimeHook then
    Writeln(FSys,' ',SysTimeString(AResults^.ElapsedTime))
  else
    Writeln(FSys);
  Flush(FSys);
  Delete(SysSuiteIndent,1,2);
end;


Procedure SysRunStartHandler (Const Suites: TSuiteArray);

Var
  I,TC : Integer;

begin
  if (CurrentRunmode in [rvQuiet,rvFailures]) then
    exit;
  TC:=0;
  For I:=0 to Length(Suites)-1 do
    Inc(TC,Suites[i]^.Tests.Count);
  Write(FSys,STestRun,':');
  If (CurrentRunMode<>rvVerbose) then
    Writeln(FSys)
  else
    Writeln(FSys,' ',Length(Suites),' ',SSuites,', ',TC,' ',STests);
end;

Procedure SysRunCompleteHandler (Const AResult : TRunSummary);


begin
  if (CurrentRunMode=rvQuiet) then exit;
  if (CurrentRunMode=rvFailures) then
    begin
    Writeln(FSys,SFailedCount,': ',AResult.TestsFailed);
    exit;
    end;
  Writeln(FSys);
  Write(FSys,SRunSummary,':');
  if HaveTimeHook then
    Writeln(Fsys,' ',SysTimeString(AResult.ElapsedTime))
  else
    Writeln(Fsys);
  Write(FSys,'  ',SSuitesSummary,':');
  With AResult do
    if CurrentRunMode=rvVerbose then
      begin
      Writeln(FSys);
      Writeln(FSys,'    ',SRunCount,': ',SuitesRun);
      Writeln(FSys,'    ',SFailedCount,': ',SuitesFailed);
      Writeln(FSys,'    ',SInactiveCount,': ',SuitesInactive);
      end
    else
      Writeln(FSys,' ',SRunCount,': ',SuitesRun,' ',SFailedCount,': ',SuitesFailed,' ',SInactiveCount,': ',SuitesInactive);
  Write(FSys,'  ',STestsSummary,':');
  With AResult do
    if CurrentRunMode=rvVerbose then
      begin
      Writeln(FSys);
      Writeln(FSys,'    ',SRunCount,': ',TestsRun);
      Writeln(FSys,'    ',SInactiveCount,': ',TestsInactive);
      Writeln(FSys,'    ',SFailedCount,': ',TestsFailed);
      Writeln(FSys,'    ',SIgnoredCount,': ',TestsIgnored);
      if RequirePassed then
        Writeln(FSys,'    ',SUnimplementedCount,': ',TestsUnimplemented);
      end
    else
      begin
      Write(FSys,' ',SRunCount,': ',TestsRun,' ',SFailedCount,': ',TestsFailed,' ',SInactiveCount,': ',TestsInactive,' ',SIgnoredCount,': ',TestsIgnored);
      if RequirePassed then
        Writeln(FSys,' ',SUnimplementedCount,': ',TestsUnimplemented)
      else
        Writeln(FSys);
      end;
  Flush(FSys);
end;

Procedure SysSuiteSetupFailedHandler (ASuite : PSuite; Const AError : TTestString);

begin
  If (CurrentRunMode=rvVerbose) then
    Writeln(FSys,SSuiteSetupFailed,' : ',ASuite^.Name,' : ',AError)
end;

Procedure SysSuiteTearDownFailedHandler (ASuite : PSuite; Const AError : TTestString);

begin
  If (CurrentRunMode=rvVerbose) then
    Writeln(FSys,SSuiteTeardownFailed,' : ',ASuite^.Name,' : ',AError)
end;

procedure SetupSysHandlers;

begin
{$i-}
  Close(FSys);
{$i+}
  Assign(FSys,'');
{$i-}
  Rewrite(FSys);
  if (IOResult<>0) then
    CurrentRunMode:=rvQuiet;
{$i+}
  CurrentRunmode:=rvNormal;
  // Run
  SetRunStartHandler(@SysRunStartHandler);
  SetRunCompleteHandler(@SysRunCompleteHandler);
  // Suite
  SetSuiteCompleteHandler(@SysSuiteCompleteHandler);
  SetSuiteStartHandler(@SysSuiteStartHandler);
  SetSuiteSetupFailureHandler(@SysSuiteSetupFailedHandler);
  SetSuiteTearDownFailureHandler(@SysSuiteTearDownFailedHandler);
  // Test
  SetTestStartHandler(@SysTestStartHandler);
  SetTestCompleteHandler(@SysTestCompleteHandler);
end;

procedure TearDownSysHandlers;

begin
{$I-}
  Close(FSys);
{$I+}
  ClearTestHooks;
end;

function GetSysRunVerbosity: TSysRunVerbosity;
begin
  Result:=CurrentRunMode;
end;

function SetSysRunVerbosity(AMode: TSysRunVerbosity): TSysRunVerbosity;
begin
  Result:=CurrentRunMode;
  CurrentRunMode:=AMode;
end;


Function FullSuiteName(ASuite : PSuite) : AnsiString;

begin
  Result:='';
  While (ASuite<>Nil) do
    begin
    If (Result<>'') then
      Result:='.'+Result;
    Result:=ASuite^.Name+Result;
    ASuite:=ASuite^.ParentSuite;
    end;
end;

Procedure SysListTests(ASuiteList : PSuiteList; ASuite : Psuite; ATest : PTest);

Var
  I,J : Integer;
  S : PSuite;
  T : PTest;

begin
  if ASuiteList=Nil then
    exit;
  For i:=0 to ASuiteList^.Count-1 do
    begin
    S:=ASuiteList^.Items[I];
    If (ASuite=Nil) or (ASuite=S) then
      Begin
      if (CurrentRunMode=rvVerbose) then
        Writeln(FSys,SSuite,': ',FullSuiteName(S));
      // First, list all sub suites.
      SysListTests(@S^.Suites,ASuite,ATest);
      For J:=0 to S^.Tests.Count-1 do
        begin
        T:=S^.Tests.Items[J];
        If (ATest=Nil) or (ATest=T) then
          begin
          if (CurrentRunMode=rvVerbose) then
            Write(FSys,'  ',STest,': ');
          Writeln(FSys,FullSuiteName(S),'.',T^.Name);
          end;
        end;
      end;
    end;
end;

procedure ProcessSysCommandline;

Var
  i: Integer;
  S: TTestString;

  Function TestO(Const Short,Long: ShortString) : Boolean;

  Var
    L : Integer;
    LO  : String;

  begin
    Result:=(S='-'+Short);
    if Result then
      begin
      Inc(I);
      S:=Paramstr(i);
      end
    else
      begin
      Lo:='--'+Long+'=';
      L:=Length(Lo);
      Result:=(Copy(S,1,L)=LO);
      if Result then
        Delete(S,1,L);
      end;
  end;

begin
  SysRunMode:=rmTest;
  I:=1;
  While I<=ParamCount do
    begin
    S:=ParamStr(i);
    if (S='-v') or (S='--verbose') then
      SetSysRunVerbosity(rvverbose)
    else if (S='-q') or (S='--quiet') then
      SetSysRunVerbosity(rvQuiet)
    else if (S='-n') or (S='--normal') then
      SetSysRunVerbosity(rvNormal)
    else if (S='-f') or (S='--failures') then
      SetSysRunVerbosity(rvFailures)
    else if (S='-l') or (S='--list') then
      SysRunMode:=rmList
    else if (S='-h') or (S='--help') then
      SysRunMode:=rmHelp
    else if TestO('o','output') then
      begin
      If (S='') then
        begin
        S:=ParamStr(0)
        end;
      SysOutputFileName:=S;
      end
    else if TestO('s','suite') then
      SysSuiteName:=S
    else if TestO('t','test') then
      SysTestName:=S;
    Inc(i);
    end;
 // if (SysOutputFileName<>'') then
    begin
    Assign(FSys,SysOutputFileName);
    {$i-}
    Rewrite(FSys);
    if (IOResult<>0) then
      CurrentRunMode:=rvQuiet;
    {$i+}
    end;
end;



Procedure SysShowHelp;

begin
  Writeln(SUsage);
  Writeln(SHelpF);
  Writeln(SHelpH);
  Writeln(SHelpL);
  Writeln(SHelpN);
  Writeln(SHelpO);
  Writeln(SHelpQ);
  Writeln(SHelpS);
  Writeln(SHelpT);
  Writeln(SHelpV);
  If HaveTimeHook then
    Writeln(SHelpHasTime)
  else
    Writeln(SHelpNoTime);
  Writeln(SHelpExitCodes);
  Writeln(SHelpExit0);
  Writeln(SHelpExit1);
  Writeln(SHelpExit2);
  Writeln(SHelpExit3);
  Writeln(SHelpExit4);
  Writeln(SHelpExit5);
end;

procedure DoRunSysTests(S: PSuite; T: PTest);

Var
  r : TTestError;

begin
  Case SysRunMode of
    rmHelp:
      begin
      SysShowHelp;
      Halt(0);
      end;
    rmList:
      begin
      SysListTests(@TestRegistry,S,T);
      Halt(0);
      end;
    rmTest:
      begin
      if Assigned(T) then
        R:=RunTest(S,T)
      else if Assigned(S) then
        R:=RunSuite(S)
      else
        R:=RunAllTests;
      If (R<>teOK) then
        Halt(5)
      else
        SysHalt;
      end;
  end;
end;

procedure RunAllSysTests;

Var
  S : PSuite;
  T : PTest;
  P : Integer;

begin
  S:=Nil;
  T:=Nil;
  ProcessSysCommandline;
  P:=Pos('.',SysTestName);
  if (P>0) then
    begin
    SysSuiteName:=Copy(SysTestName,1,P-1);
    Delete(SysTestName,1,P);
    P:=Pos('.',SysTestName);
    While P<>0 do
      begin
      SysSuiteName:=SysSuiteName+'.'+Copy(SysTestName,1,P-1);
      Delete(SysTestName,1,P);
      P:=Pos('.',SysTestName);
      end;
    end;
  if (SysSuiteName<>'') then
    begin
    S:=GetSuite(SysSuiteName);
    if (S=Nil) then
      Halt(3);
    end;
  if (SysTestName<>'') then
    begin
    if (S=Nil) then
      begin
      S:=GetSuite(DefaultSuiteName);
      if (S=Nil) then
        Halt(3);
      end;
    T:=GetTest(S,SysTestName);
    if (T=Nil) then
      Halt(4);
    end;
  DoRunSysTests(S,T);
end;

{ EFail }

Constructor EFail.Create(Const AMessage: AnsiString);
begin
  FMessage:=AMessage;
end;

Function EFail.ToString: AnsiString;
begin
  Result:=FMessage;
end;

function GetSysTestOS: ShortString;

begin
  GetSysTestOS := lowercase({$I %FPCTARGETOS%});
end;

function SysGetSetting(const AName: ShortString): ShortString;

  Procedure Trim(Var S : ShortString);

  begin
    While (S<>'') and (S[1] in [' ',#9]) do Delete(S,1,1);
    While (S<>'') and (S[Length(S)] in [' ',#9]) do S:=Copy(S,1,Length(S)-1);
  end;

Var
  F : Text;
  I: Integer;
  FN,N,V : String;

begin
  Result:='';
  FN:=paramstr(0);
  I:=Length(Fn);
  While (I>0) and (FN[I]<>DirectorySeparator) do
    Dec(I);
  FN:=Copy(FN,1,I);
  Assign(f,FN+'punit.cfg');
  {$i-}
  Reset(f);
  if IOResult<>0 then
    exit;
  {$i+}
  While (Result='') and not EOF(F) do
    begin
    ReadLn(F,V);
    N:='';
    I:=Pos('=',V);
    if I>0 then
      begin
      N:=Copy(V,1,I-1);
      Delete(V,1,I);
      end;
    if (N<>'') and (Pos(';',N)<>1) and (Pos('#',N)<>1) then
      begin
      Trim(N);
      If upcase(N)=upcase(AName) then
        begin
        Result:=V;
        Trim(Result);
        end;
      end;
    end;
  Close(F);
end;

initialization
  SetupTestRegistry;
  SetupSysHandlers;

finalization

  TearDownSysHandlers;
  TearDownTestRegistry;
  ResetRun(CurrentRun);
end.

