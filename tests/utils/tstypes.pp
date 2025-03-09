{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2007 by the Free Pascal development team.

    This unit contains the different possible outcome
    of a single test.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$modeswitch advancedrecords}
{$h+}

unit tstypes;

interface

uses
  tsstring;


Type
  TTestStatus = (
  stInvalid,
  stFailedToCompile,
  stSuccessCompilationFailed,
  stFailedCompilationsuccessful,
  stSuccessfullyCompiled,
  stFailedToRun,
  stKnownRunProblem,
  stSuccessFullyRun,
  stSkippingGraphTest,
  stSkippingInteractiveTest,
  stSkippingKnownBug,
  stSkippingCompilerVersionTooLow,
  stSkippingCompilerVersionTooHigh,
  stSkippingOtherCpu,
  stSkippingOtherTarget,
  stskippingRunUnit,
  stskippingRunTest
  );


Const
  FirstStatus = stFailedToCompile;
  LastStatus = stskippingRunTest;

  TestOK : Array[TTestStatus] of Boolean = (
    False, // stInvalid
    False, // stFailedToCompile,
    True,  // stSuccessCompilationFailed,
    False, // stFailedCompilationsuccessful,
    True,  // stSuccessfullyCompiled,
    False, // stFailedToRun,
    True,  // stKnownRunProblem,
    True,  // stSuccessFullyRun,
    False, // stSkippingGraphTest,
    False, // stSkippingInteractiveTest,
    False, // stSkippingKnownBug,
    False, // stSkippingCompilerVersionTooLow,
    False, // stSkippingCompilerVersionTooHigh,
    False, // stSkippingOtherCpu,
    False, // stSkippingOtherTarget,
    False, // stskippingRunUnit,
    False  // stskippingRunTest
  );

  TestFailed : Array[TTestStatus] of Boolean = (
      False, // stInvalid
      True,  // stFailedToCompile,
      False, // stSuccessCompilationFailed,
      True,  // stFailedCompilationsuccessful,
      False, // stSuccessfullyCompiled,
      True,  // stFailedToRun,
      False, // stKnownRunProblem,
      False, // stSuccessFullyRun,
      False, // stSkippingGraphTest,
      False, // stSkippingInteractiveTest,
      False, // stSkippingKnownBug,
      False, // stSkippingCompilerVersionTooLow,
      False, // stSkippingCompilerVersionTooHigh,
      False, // stSkippingOtherCpu,
      False, // stSkippingOtherTarget,
      False, // stSkippingRunUnit,
      False  // stskippingRunTest
   );


  TestSkipped : Array[TTestStatus] of Boolean = (
    False,  // stInvalid
    False,  // stFailedToCompile,
    False,  // stSuccessCompilationFailed,
    False,  // stFailedCompilationsuccessful,
    False,  // stSuccessfullyCompiled,
    False,  // stFailedToRun,
    False,  // stKnownRunProblem,
    False,  // stSuccessFullyRun,
    True,   // stSkippingGraphTest,
    True,   // stSkippingInteractiveTest,
    True,   // stSkippingKnownBug,
    True,   // stSkippingCompilerVersionTooLow,
    True,   // stSkippingCompilerVersionTooHigh,
    True,   // stSkippingOtherCpu,
    True,   // stSkippingOtherTarget,
    True,   // stskippingRunUnit,
    True    // stskippingRunTest
  );

  ExpectRun : Array[TTestStatus] of Boolean = (
    False,  // stInvalid
    False,  // stFailedToCompile,
    False,  // stSuccessCompilationFailed,
    False,  // stFailedCompilationsuccessful,
    True ,  // stSuccessfullyCompiled,
    False,  // stFailedToRun,
    False,  // stKnownRunProblem,
    False,  // stSuccessFullyRun,
    False,  // stSkippingGraphTest,
    False,  // stSkippingInteractiveTest,
    False,  // stSkippingKnownBug,
    False,  // stSkippingCompilerVersionTooLow,
    False,  // stSkippingCompilerVersionTooHigh,
    False,  // stSkippingOtherCpu,
    False,  // stSkippingOtherTarget,
    False,  // stskippingRunUnit,
    False   // stskippingRunTest
   );

  StatusText : Array[TTestStatus] of String = (
    invalid_status,
    failed_to_compile,
    success_compilation_failed,
    failed_compilation_successful ,
    successfully_compiled ,
    failed_to_run ,
    known_problem ,
    successfully_run ,
    skipping_graph_test ,
    skipping_interactive_test ,
    skipping_known_bug ,
    skipping_compiler_version_too_low,
    skipping_compiler_version_too_high,
    skipping_other_cpu ,
    skipping_other_target ,
    skipping_run_unit ,
    skipping_run_test
  );

  SQLField : Array[TTestStatus] of String = (
    '',
    'TU_FAILEDTOCOMPILE',
    'TU_SUCCESSFULLYFAILED',
    'TU_FAILEDTOFAIL',
    'TU_SUCCESFULLYCOMPILED',
    'TU_FAILEDTORUN',
    'TU_KNOWNPROBLEM',
    'TU_SUCCESSFULLYRUN',
    'TU_SKIPPEDGRAPHTEST',
    'TU_SKIPPEDINTERACTIVETEST',
    'TU_KNOWNBUG',
    'TU_COMPILERVERIONTOOLOW',
    'TU_COMPILERVERIONTOOHIGH',
    'TU_OTHERCPU',
    'TU_OTHERTARGET',
    'TU_UNIT',
    'TU_SKIPPINGRUNTEST'
  );


  UseGit = True;

  faction_show_overview = 0;
  faction_show_run_results = 1;
  faction_show_run_pie = 2;
  faction_show_one_test = 3;
  faction_show_history = 4;
  faction_compare_with_previous = 5;
  faction_compare_with_next = 6;
  faction_compare2_with_previous = 7;
  faction_compare2_with_next = 8;
  faction_compare_both_with_previous = 9;
  faction_compare_both_with_next = 10;

Type

  TValidTestStatus = FirstStatus .. LastStatus;
  TCharSet = set of char;

  TVerboseLevel=(V_Abort,V_Error,V_Warning,V_Normal,V_Debug,V_SQL);

  // This record contains exactly the fields of the database.

  TTestInfo = record
    Name : String;
    CPU : String;
    OS : string;
    Version : string;
    AddDate : TDateTime;
    Graph : boolean;
    Interactive : boolean;
    Result :  integer;
    Fail : boolean;
    ReCompile : boolean;
    NoRun : boolean;
    NeedLibrary : boolean;
    KnownRunError : Integer;
    Known : boolean;
    Note : String;
    Description : String;
    Source : String;
    Opts : String;
    DelOptions,
    SkipCPU,
    SkipEmu,
    NeedTarget,
    SkipTarget,
    MaxVersion,
    KnownRunNote,
    KnownCompileNote,
    RecompileOpt: string;
    KnownCompileError : longint;
    NeededAfter   : boolean;
    IsKnownRunError : Boolean;
    Timeout       : longint;
    Category      : string;
    Files         : string;
    ConfigFileSrc : string;
    ConfigFileDst : string;
    WpoParas      : string;
    WpoPasses     : longint;
    DelFiles      : string;
    ExpectMsgs    : array of longint;
    Property NeedCPU : String Read CPU Write CPU;
    Property MinVersion : String Read Version Write Version;
    Property UsesGraph : boolean read Graph Write Graph;
    Property IsInteractive : boolean Read Interactive write INTERACTIVE;
    Property ResultCode : Integer Read RESULT Write RESULT;
    Property ShouldFail : Boolean Read FAIL Write Fail;
    Property NeedRecompile : Boolean Read Recompile Write Recompile;
    Property IsKnownCompileError : Boolean read KNOWN Write KNown;
    Property NeedOptions : String Read OPTS Write OPTS;
  end;
  TConfig = TTestInfo;

  TRunStats = Record
    OKCount,
    FailedCount,
    SkipCount : Integer;
  end;

  // Test run data

  { TTestRunData }

  TTestRunData = Record
    logfile: string;
    longlogfile : string;
    os: string;
    cpu: string;
    category: string;
    version: string;
    submitter: string;
    machine: string;
    config : string;
    description : string;
    Date : TDateTime;
    CompilerDate,
    CompilerFullVersion,
    CompilerRevision,
    TestsRevision,
    RTLRevision,
    PackagesRevision : String;
    CPUID : Integer;
    OSID  : Integer;
    VersionID  : Integer;
    CategoryID : Integer;
    RunID : Int64;
    //ConfigID : Integer;
    PlatformID : Integer;
    StatusCount : Array[TTestStatus] of Integer;
    Function GetField(const aField : String) : String;
    function FailedCount: Integer;
    function SkippedCount : Integer;
    function OKCount: Integer;
    function TotalCount: Integer;
  end;

  { TTestResultData }

  TTestResultData = record
    PlatformID : Integer;
    TestID : Integer;
    ID : Int64;
    RunID : Int64;
    TestResult : TTestStatus;
    Log : String;
    Date : TDateTime;
    function ResultDiffers(aResult : TTestResultData; CompareLog : Boolean = False) : Boolean;
  end;

implementation

uses sysutils;

{ TTestRunData }

function TTestRunData.OKCount : Integer;

var
  TS : TTestStatus;

begin
  Result:=0;
  for TS:=FirstStatus to LastStatus do
    if TestOK[TS] then
      Inc(Result,StatusCount[TS]);
end;

function TTestRunData.FailedCount : Integer;

var
  TS : TTestStatus;
begin
  Result:=0;
  for TS:=FirstStatus to LastStatus do
    if TestFailed[TS] then
      Inc(Result,StatusCount[TS]);
end;

function TTestRunData.SkippedCount: Integer;
var
  TS : TTestStatus;
begin
  Result:=0;
  for TS:=FirstStatus to LastStatus do
    if TestSkipped[TS] then
      Inc(Result,StatusCount[TS]);
end;

function TTestRunData.TotalCount: Integer;
var
  TS : TTestStatus;
begin
  Result:=0;
  for TS:=FirstStatus to LastStatus do
    Result:=Result+StatusCount[TS];
end;


function TTestRunData.GetField(const aField: String): String;
begin
  case lowercase(aField) of
  'logfile' : Result:=logfile;
  'longlogfile ' : Result:=longlogfile ;
  'os' : Result:=os;
  'cpu' : Result:=cpu;
  'category' : Result:=category;
  'version' : Result:=version;
  'submitter' : Result:=submitter;
  'machine' : Result:=machine;
  'comment',
  'config' : Result:=config ;
  'description' : Result:=description ;
  'date' : Result:=DateToStr(Date);
  'compilerdate': Result:=CompilerDate;
  'compilerfullversion': Result:=CompilerFullVersion;
  'compilerrevision':  Result:=CompilerRevision;
  'restsrevision': Result:=TestsRevision;
  'rtlrevision': Result:=RTLRevision;
  'packagesrevision' : Result:=PackagesRevision ;
  'cpuid' : Result:=IntToStr(CPUID);
  'osid' : Result:=IntToStr(OSID);
  'versionid' : Result:=IntToStr(VersionID);
  'categoryid' : Result:=IntToStr(CategoryID);
  'runid' : Result:=IntToStr(RunID);
  'platformid': Result:=IntToStr(PlatformID);
  'stfailedtocompile' : Result:=IntToStr(StatusCount[stfailedtocompile]);
  'stsuccesscompilationfailed' : Result:=IntToStr(StatusCount[stsuccesscompilationfailed]);
  'stfailedcompilationsuccessful' : Result:=IntToStr(StatusCount[stfailedcompilationsuccessful]);
  'stsuccessfullycompiled' : Result:=IntToStr(StatusCount[stsuccessfullycompiled]);
  'stfailedtorun' : Result:=IntToStr(StatusCount[stfailedtorun]);
  'stknownrunproblem' : Result:=IntToStr(StatusCount[stknownrunproblem]);
  'stsuccessfullyrun' : Result:=IntToStr(StatusCount[stsuccessfullyrun]);
  'stskippinggraphtest' : Result:=IntToStr(StatusCount[stskippinggraphtest]);
  'stskippinginteractivetest' : Result:=IntToStr(StatusCount[stskippinginteractivetest]);
  'stskippingknownbug' : Result:=IntToStr(StatusCount[stskippingknownbug]);
  'stskippingcompilerversiontoolow' : Result:=IntToStr(StatusCount[stskippingcompilerversiontoolow]);
  'stskippingcompilerversiontoohigh' : Result:=IntToStr(StatusCount[stskippingcompilerversiontoohigh]);
  'stskippingothercpu' : Result:=IntToStr(StatusCount[stskippingothercpu]);
  'stskippingothertarget' : Result:=IntToStr(StatusCount[stskippingothertarget]);
  'stskippingrununit' : Result:=IntToStr(StatusCount[stskippingrununit]);
  'stskippingruntest' : Result:=IntToStr(StatusCount[stskippingruntest]);
  'failed' :  Result:=IntToStr(FailedCount);
  'ok' : Result:=IntToStr(OKCount);
  'total' : Result:=IntToStr(TotalCount);
  'rev' : Result:=CompilerRevision+'/'+RTLRevision+'/'+PackagesRevision+'/'+TestsRevision;
  end;
end;

{ TTestResultData }

function TTestResultData.ResultDiffers(aResult: TTestResultData; CompareLog: Boolean): Boolean;
begin
  Result:=(PlatformID<>aResult.PlatFormID);
  Result:=Result or (TestID<>aResult.TestID);
  Result:=Result or (TestResult<>aResult.TestResult);
  if CompareLog and Not Result then
    Result:=Log<>aResult.Log;
end;

end.

