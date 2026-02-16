{ This unit contains the TTestRunner class, a base class for the console test
  runner for fpcunit.

  Copyright (C) 2006 Vincent Snijders

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit consoletestrunner;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Fcl.CustApp, System.Classes, System.SysUtils, System.IniFiles, FpcUnit.Test,
  FpcUnit.Registry, FpcUnit.Utils, FpcUnit.Reports, FpcUnit.Reports.LaTeX,
  FpcUnit.Reports.XMLTest, FpcUnit.Reports.Plain, FpcUnit.Reports.JUnit, Xml.Dom;
{$ELSE FPC_DOTTEDUNITS}
uses
  custapp, Classes, SysUtils, inifiles, fpcunit, testregistry, testutils,
  fpcunitreport, latextestreport, xmltestreport, plaintestreport,
  junittestreport, dom;
{$ENDIF FPC_DOTTEDUNITS}

const
  Version = '0.5';

type
  TFormat = (fPlain, fLatex, fXML, fPlainNoTiming, fJUnit);
  TRunMode = (rmUnknown,rmHelp,rmList,rmSuite,rmAll);

var
  DefaultFormat : TFormat = fXML;
  DefaultRunAllTests : Boolean = False;

type
  { TTestRunner }

  TTestRunner = class(TCustomApplication)
  private
    FShowProgress: boolean;
    FFileName: string;
    FStyleSheet: string;
    FLongOpts: TStrings;
    FFormatParam: TFormat;
    FSkipTiming : Boolean;
    FSParse: Boolean;
    FSkipAddressInfo : Boolean;
    FSuite: String;
    FRunMode : TRunMode;
    FNoExitCodeOnError : boolean;
    procedure DoStatus(const msg: string);
  protected
    const
      CDefaultsFileNameConst = 'testdefaults.ini';
      CDefaultsFileNameEnvVar = 'FPCUNITCONFIG';
      CDefaultsFileIniSection = 'defaults';
    function DefaultsFileName: String; virtual;
    procedure ReadCustomDefaults(Ini: TMemIniFile; Section: string); virtual;
    procedure ReadDefaults; virtual;
  protected
    Class function StrToFormat(const S: String): TFormat;
    procedure RunSuite; virtual;
    procedure ShowTestList; virtual;
    procedure Usage; virtual;
    property FileName: string read FFileName write FFileName;
    property LongOpts: TStrings read FLongOpts write FLongOpts;
    property ShowProgress: boolean read FShowProgress write FShowProgress;
    property StyleSheet: string read FStyleSheet write FStyleSheet;
    property FormatParam: TFormat read FFormatParam write FFormatParam;
    property NoExitCodeOnError : boolean read FNoExitCodeOnError Write FNoExitCodeOnError;
    procedure DoRun; override;
    procedure DoTestRun(ATest: TTest); virtual;
    function GetShortOpts: string; virtual;
    procedure AppendLongOpts; virtual;
    procedure WriteCustomHelp; virtual;
    function ParseOptions: Boolean; virtual;
    procedure ExtendXmlDocument(Doc: TXMLDocument); virtual;
    function GetResultsWriter: TCustomResultsWriter; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpcUnit.Decorator;
{$ELSE FPC_DOTTEDUNITS}
uses testdecorator;
{$ENDIF FPC_DOTTEDUNITS}

const
  ArgHelp = 'help';
  ArgList = 'list';
  ArgAll = 'all';
  ArgSuite = 'suite';
  ArgFormat = 'format';
  ArgSkipTiming = 'skiptiming';
  ArgSparse = 'sparse';
  ArgNoAddresses = 'no-addresses';
  ArgStyleSheet = 'stylesheet';
  ArgProgress = 'progress';
  ArgStatus = 'status';
  ArgNoExitCode = 'no-exitcode';
  ArgFile = 'file';
  ArgNoConfig = 'no-config';

const
  ShortOpts =
    'h'+ // ArgHelp
    'l'+ // ArgList
    'a'+ // ArgAll
    's:'+ // ArgSuite (requires value)
    't'+ // ArgSkipTiming
    'q'+ // ArgSparse
    'd'+ // ArgNoAddresses
    'y:'+ // ArgStyleSheet (requires value)
    'p'+ // ArgProgress
    'u'+ // ArgStatus
    'x'+ // ArgNoExitCode
    'f:'+ // ArgFile (requires value)
    'n'; // ArgNoConfig
  DefaultLongOpts: array[1..14] of string = (
    ArgHelp,
    ArgList,
    ArgAll,
    ArgSuite+':', // requires value
    ArgFormat+':', // requires value
    ArgSkipTiming,
    ArgSparse,
    ArgNoAddresses,
    ArgStyleSheet+':', // requires value
    ArgProgress,
    ArgStatus,
    ArgNoExitCode,
    ArgFile+':', // requires value
    ArgNoConfig
  );

const
  ValXML = 'xml';
  ValPlain = 'plain';
  ValPlainNoTiming = 'plainnotiming';
  ValLatex = 'latex';
  ValJUnit = 'junit';

Type
  TTestDecoratorClass = Class of TTestDecorator;

  { TDecoratorTestSuite }

  TDecoratorTestSuite = Class(TTestSuite)
  public
    Destructor Destroy; override;
  end;

  { TProgressWriter }

  TProgressWriter= class(TNoRefCountObject, ITestListener)
  private
    FTotal : Integer;
    FFailed: Integer;
    FIgnored : Integer;
    FErrors : Integer;
    FQuiet : Boolean;
    FSuccess : Boolean;
    procedure WriteChar(c: AnsiChar);
  public
    Constructor Create(AQuiet : Boolean);
    destructor Destroy; override;
    Function GetExitCode : Integer;
    { ITestListener interface requirements }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    Property Total : Integer Read FTotal;
    Property Failed : Integer Read FFailed;
    Property Errors : Integer Read FErrors;
    Property Ignored : Integer Read FIgnored;
    Property Quiet : Boolean Read FQuiet;
  end;

{ ---------------------------------------------------------------------
  TProgressWriter
  ---------------------------------------------------------------------}

procedure TProgressWriter.WriteChar(c: AnsiChar);
begin
  write(c);
  // flush output, so that we see the AnsiChar immediately, even it is written to file
  Flush(output);
end;

constructor TProgressWriter.Create(AQuiet: Boolean);

begin
  FQuiet:=AQuiet;
end;

destructor TProgressWriter.Destroy;
begin
  // on destruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

function TProgressWriter.GetExitCode: Integer;

begin
  Result:=Ord(Failed<>0); // Bit 0 indicates fails
  if Errors<>0 then
    Result:=Result or 2;  // Bit 1 indicates errors.
end;

procedure TProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess:=False;
  If AFailure.IsIgnoredTest then
    Inc(FIgnored)
  else
    Inc(FFailed);
  If Not Quiet then
    writechar('F');
end;

procedure TProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess:=False;
  Inc(FErrors);
  if not Quiet then
    writechar('E');
end;

procedure TProgressWriter.StartTest(ATest: TTest);
begin
  FSuccess := true; // assume success, until proven otherwise
end;

procedure TProgressWriter.EndTest(ATest: TTest);
begin
  if FSuccess and not Quiet then
    writechar('.');
end;

procedure TProgressWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

procedure TProgressWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

{ ---------------------------------------------------------------------
  TDecoratorTestSuite
  ---------------------------------------------------------------------}

destructor TDecoratorTestSuite.Destroy;

begin
  OwnsTests:=False;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------
  TTestRunner
  ---------------------------------------------------------------------}

constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLongOpts := TStringList.Create;
  AppendLongOpts;
  StopOnException:=True;
  NoExitCodeOnError:=false;
end;

destructor TTestRunner.Destroy;
begin
  FLongOpts.Free;
  inherited Destroy;
end;

procedure TTestRunner.DoStatus(const msg: string);
begin
  Writeln(stderr,msg);
end;

class function TTestRunner.StrToFormat(const S: String): TFormat;
begin
  Case lowercase(S) of
    ValXML:           Result := fXML;
    ValPlain:         Result := fPlain;
    ValPlainNoTiming: Result := fPlainNoTiming;
    ValLatex:         Result := fLatex;
    ValJUnit:         Result := fJUnit;
  else
    Raise EConvertError.CreateFmt('Not a valid output format : "%s"',[S]);
  end;
end;

function TTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  case FormatParam of
    fLatex:         Result := TLatexResultsWriter.Create(nil);
    fPlain:         Result := TPlainResultsWriter.Create(nil);
    fPlainNotiming: Result := TPlainResultsWriter.Create(nil);
    fJUnit:         Result := TJUnitResultsWriter.Create(nil)
  else
    begin
      Result := TXmlResultsWriter.Create(nil);
      ExtendXmlDocument(TXMLResultsWriter(Result).Document);
    end;
  end;
  Result.SkipTiming:=FSkipTiming or (formatParam=fPlainNoTiming);
  Result.Sparse:=FSparse;
  Result.SkipAddressInfo:=FSkipAddressInfo;
end;

procedure TTestRunner.DoTestRun(ATest: TTest);

var
  ResultsWriter: TCustomResultsWriter;
  ProgressWriter: TProgressWriter;
  TestResult: TTestResult;

begin
  ProgressWriter:=Nil;
  ResultsWriter:=Nil;
  TestResult := TTestResult.Create;
  try
    ProgressWriter:=TProgressWriter.Create(Not ShowProgress);
    TestResult.AddListener(ProgressWriter);
    ResultsWriter:=GetResultsWriter;
    ResultsWriter.Filename := FileName;
    TestResult.AddListener(ResultsWriter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);
  finally
    if Assigned(ProgressWriter) and not NoExitCodeOnError then
      ExitCode:=ProgressWriter.GetExitCode;
    TestResult.Free;
    ResultsWriter.Free;
    ProgressWriter.Free;
  end;
end;

function TTestRunner.GetShortOpts: string;
begin
  Result := ShortOpts;
end;

procedure TTestRunner.AppendLongOpts;
var
  i: Integer;
begin
  for i := low(DefaultLongOpts) to high(DefaultLongOpts) do
    LongOpts.Add(DefaultLongOpts[i]);
end;

procedure TTestRunner.WriteCustomHelp;
begin
  // no custom help options in base class;
end;

procedure TTestRunner.Usage;

begin
    writeln(Title);
    writeln(Version);
    writeln;
    writeln('Commands:');
    writeln('  -h or --',ArgHelp,'              Show help and version');
    writeln('  -l or --',ArgList,'              Show a list of registered tests');
    writeln('  -a or --',ArgAll,'               Run all registered tests');
    writeln('  -s or --',ArgSuite,'=<name>      Run a test suite with the specified name, or all test suites');
    writeln(       '                            of the specified class (a descendant of the TTestCase)');
    writeln;
    writeln('Options:');
    writeln('  --',ArgFormat,'=<FMT>            Select output format, <FMT> is one of:');
    writeln('    ',ValXML,'                       output as XML source (default)');
    writeln('    ',ValPlain,'                     output as plain ASCII source');
    writeln('    ',ValPlainNoTiming,'             output as plain ASCII source, skip timings');
    writeln('    ',ValLatex,'                     output as latex');
    writeln('    ',ValJUnit,'                     output as JUnit compatible XML source');
    writeln('  -t or --',ArgSkipTiming,'        Do not output timings (useful for diffs of test runs)');
    writeln('  -q or --',ArgSparse,'            Produce less output (errors/failures only)');
    writeln('  -d or --',ArgNoAddresses, '      Do not display address info');
    writeln('  -y or --',ArgStyleSheet,'=<ref>  Add stylesheet reference');
    writeln('  -p or --',ArgProgress,'          Show progress');
    writeln('  -u or --',ArgStatus,'            Show status messages on stderr');
    writeln('  -x or --',ArgNoExitCode, '       Do not set exit code on errors');
    writeln('  -f or --',ArgFile,'=<filename>   Output results to file');
    writeln('  -n or --',ArgNoConfig, '         Do not read the configuration file');
    WriteCustomHelp;
    writeln;
    writeln('Configuration file:');
    writeln('  Defaults for long options can be specified in the "',CDefaultsFileNameConst,'" file in the executable folder.');
    writeln('  The path to this file can be overridden by the environment variable "',CDefaultsFileNameEnvVar,'".');
    writeln('  All values must be located in "[',CDefaultsFileIniSection,']" section, the option names specified without the "--" sign.');
    writeln('  The value of logical options indicated via "1"/"0". Example file contents:');
    writeln('    [',CDefaultsFileIniSection,']');
    writeln('    ',ArgAll,'=1');
    writeln('    ',ArgFormat,'=',ValPlain);
    writeln('    ',ArgSparse,'=1');
    writeln('  Command line options take precedence and override the values in this file.');
end;

Function TTestRunner.DefaultsFileName : String;

begin
  Result:=GetEnvironmentVariable(CDefaultsFileNameEnvVar);
  if Result='' then
    Result:=CDefaultsFileNameConst;
  Result:=ExpandFileName(Result,Location);
end;

procedure TTestRunner.ReadCustomDefaults(Ini: TMemIniFile; Section: string);
var
  s: string;
begin
  // Determine runmode (ArgHelp option may be useful in the config due to DefaultRunAllTests)
  FSuite:=Ini.ReadString(Section,ArgSuite,'');
  if Ini.ReadBool(Section,ArgHelp,false) then
    FRunMode:=rmHelp
  else if FSuite<>'' then
    FRunMode:=rmSuite
  else if Ini.ReadBool(Section,ArgAll,false) then
    FRunMode:=rmAll
  else if Ini.ReadBool(Section,ArgList,false) then
    FRunMode:=rmList;
  // Other options
  s:=Ini.ReadString(Section,ArgFormat,'');
  if s<>'' then
    FormatParam:=StrToFormat(s); // raise exception on error
  FSkipTiming:=Ini.ReadBool(Section,ArgSkipTiming,FSKipTiming);
  FSparse:=Ini.ReadBool(Section,ArgSparse,FSparse);
  FSkipAddressInfo:=Ini.ReadBool(Section,ArgNoAddresses,FSkipAddressInfo);
  StyleSheet:=Ini.ReadString(Section,ArgStyleSheet,StyleSheet);
  ShowProgress:=Ini.ReadBool(Section,ArgProgress,ShowProgress);
  if Ini.ReadBool(Section,ArgStatus,false) then
    TAssert.StatusEvent:=@DoStatus;
  NoExitCodeOnError:=Ini.ReadBool(Section,ArgNoExitCode,FNoExitCodeOnError);
  FileName:=Ini.ReadString(Section,ArgFile,FileName);
  // (there is no point in reading the ArgNoConfig option here)
end;

procedure TTestRunner.ReadDefaults;
var
  Ini: TMemIniFile;
  FN: string;
begin
  FN:=DefaultsFileName;
  if FileExists(FN) then
    begin
    Ini:=TMemIniFile.Create(FN);
    try
      Ini.Options:=Ini.Options+[ifoStripQuotes];
      Ini.SetBoolStringValues(true,['1','true','y','yes','on']);
      Ini.SetBoolStringValues(false,['0','false','n','no','off']);

      ReadCustomDefaults(Ini,CDefaultsFileIniSection);
    finally
      Ini.Free;
    end;
    end;
end;

Function TTestRunner.ParseOptions : Boolean;

begin
  Result:=True;
  // Determine runmode
  if (ParamCount = 0) and (FRunMode=rmUnknown) then // FRunMode can be set earlier in ReadDefaults
  begin
    Usage;
    ExitCode:=1;
    exit;
  end;
  if HasOption('h', ArgHelp) then
    FRunMode:=rmHelp
  else if HasOption('s',ArgSuite) then
    begin
    FSuite:=GetOptionValue('s',ArgSuite);
    FRunMode:=rmSuite;
    end
  else If HasOption('a',ArgAll) then
    FRunMode:=rmAll
  else if HasOption('l',ArgList) then
    FRunMode:=rmList;
  // Other options
  if HasOption(ArgFormat) then
    FormatParam:=StrToFormat(GetOptionValue(ArgFormat));
  if HasOption('t',ArgSkipTiming) then
    FSkipTiming:=True;
  if HasOption('q',ArgSparse) then
    FSparse:=True;
  If HasOption('d',ArgNoAddresses) then
    FSkipAddressInfo:=True;
  if HasOption('y',ArgStyleSheet) then
    StyleSheet:=GetOptionValue('y',ArgStyleSheet);
  if HasOption('p', ArgProgress) then
    ShowProgress:=True;
  If HasOption('u',ArgStatus) then
    TAssert.StatusEvent:=@DoStatus;
  if HasOption('x',ArgNoExitCode) then
    NoExitCodeOnError:=True;
  if HasOption('f',ArgFile) then
    FileName:=GetOptionValue('f',ArgFile);
end;

procedure TTestRunner.ExtendXmlDocument(Doc: TXMLDocument);

var
  n: TDOMElement;

begin
  if StyleSheet<>'' then begin
    Doc.StylesheetType := 'text/xsl';
    Doc.StylesheetHRef := StyleSheet;
  end;
  n := Doc.CreateElement('Title');
  n.AppendChild(Doc.CreateTextNode(Title));
  Doc.FirstChild.AppendChild(n);
end;


procedure TTestRunner.RunSuite;

var
  I,P : integer;
  S,TN : string;
  TS : TDecoratorTestSuite;
  T : TTest;

begin
  S := FSuite;
  if S = '' then
    for I := 0 to GetTestRegistry.ChildTestCount - 1 do
      writeln(GetTestRegistry[i].TestName)
  else
    begin
      TS:=TDecoratorTestSuite.Create('SuiteList');
      try
      while Not(S = '') Do
        begin
        P:=Pos(',',S);
        If P=0 then
          P:=Length(S)+1;
        TN:=Copy(S,1,P-1);
        Delete(S,1,P);
        if (TN<>'') then
          begin
          T:=GetTestRegistry.FindTest(TN);
          if Assigned(T) then
            TS.AddTest(T);
          end;
        end;
        if (TS.CountTestCases>1) then
          DoTestRun(TS)
        else if TS.CountTestCases=1 then
          DoTestRun(TS[0])
        else
          Writeln('No tests selected.');
      finally
        TS.Free;
      end;
    end;
end;

procedure TTestRunner.ShowTestList;

begin
  case FormatParam of
    fLatex:         Write(GetSuiteAsLatex(GetTestRegistry));
    fPlain:         Write(GetSuiteAsPlain(GetTestRegistry));
    fPlainNoTiming: Write(GetSuiteAsPlain(GetTestRegistry));
  else
    Write(GetSuiteAsXml(GetTestRegistry));
  end
end;

procedure TTestRunner.DoRun;

var
  S : string;

begin
  Terminate;
  FormatParam := DefaultFormat;
  If DefaultRunAllTests then
    FRunMode:=rmAll;
  S := CheckOptions(GetShortOpts, LongOpts);
  if (S <> '') then
    begin
    Writeln(S);
    Exit;
    end;
  if not HasOption('n',ArgNoConfig) then
    ReadDefaults;
  if Not ParseOptions then
    exit;
  Case FRunMode of
    rmHelp: Usage;
    rmList: ShowTestList;
    rmSuite: RunSuite;
    rmAll: DoTestRun(GetTestRegistry);
  else
    // rmUnknown
  end;
end;

end.

