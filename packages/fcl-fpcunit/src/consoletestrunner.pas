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
unit consoletestrunner;

{$mode objfpc}{$H+}

interface

uses
  custapp, Classes, SysUtils, fpcunit, testregistry, testutils,
  fpcunitreport, latextestreport, xmltestreport, plaintestreport,
  dom;

const
  Version = '0.3';

type
  TFormat = (fPlain, fLatex, fXML, fPlainNoTiming);
  TRunMode = (rmUnknown,rmList,rmSuite,rmAll);

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
  protected
    Class function StrToFormat(S: String): TFormat;
    function DefaultsFileName: String;
    procedure RunSuite; virtual;
    procedure ShowTestList; virtual;
    procedure ReadDefaults; virtual;
    procedure Usage; virtual;
    property FileName: string read FFileName write FFileName;
    property LongOpts: TStrings read FLongOpts write FLongOpts;
    property ShowProgress: boolean read FShowProgress write FShowProgress;
    property StyleSheet: string read FStyleSheet write FStyleSheet;
    property FormatParam: TFormat read FFormatParam write FFormatParam;
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

uses inifiles, testdecorator;

const
  ShortOpts = 'alhp';
  DefaultLongOpts: array[1..11] of string =
     ('all', 'list', 'progress', 'help', 'skiptiming',
      'suite:', 'format:', 'file:', 'stylesheet:','sparse','no-addresses');

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
    procedure WriteChar(c: char);
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

procedure TProgressWriter.WriteChar(c: char);
begin
  write(c);
  // flush output, so that we see the char immediately, even it is written to file
  Flush(output);
end;

constructor TProgressWriter.Create(AQuiet: Boolean);

begin
  FQuiet:=AQuiet;
end;

destructor TProgressWriter.Destroy;
begin
  // on descruction, just write the missing line ending
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
end;

destructor TTestRunner.Destroy;
begin
  FLongOpts.Free;
  inherited Destroy;
end;

class function TTestRunner.StrToFormat(S: String): TFormat;

begin
  Case lowercase(S) of
    'latex': Result:=fLatex;
    'plain': Result:=fPlain;
    'plainnotiming': Result:=fPlainNoTiming;
    'xml': Result:=fXML;
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
    if Assigned(ProgressWriter) then
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
    writeln('Usage: ');
    writeln('  --format=FMT        Select output format. FMT is one of:');
    writeln('    latex            output as latex');
    writeln('    plain            output as plain ASCII source');
    writeln('    plainnotiming    output as plain ASCII source, skip timings');
    writeln('    xml              output as XML source (default)');
    writeln('  --skiptiming              Do not output timings (useful for diffs of testruns)');
    writeln('  --sparse                  Produce Less output (errors/failures only)');
    writeln('  --no-addresses            Do not display address info');
    writeln('  --stylesheet=<reference>   add stylesheet reference');
    writeln('  --file=<filename>         output results to file');
    writeln;
    writeln('  -l or --list              show a list of registered tests');
    writeln('  -a or --all               run all tests');
    writeln('  -p or --progress          show progress');
    writeln('  --suite=MyTestSuiteName   run single test suite class');
    WriteCustomHelp;
    writeln;
    Writeln('Defaults for long options will be read from ini file ',DefaultsFileName);
    writeln('The results can be redirected to a file,');
    writeln('for example: ', ParamStr(0),' --all > results.xml');
end;

Function TTestRunner.DefaultsFileName : String;

begin
  Result:=GetEnvironmentVariable('FPCUNITCONFIG');
  if (Result='') then
    Result:=Location+'testdefaults.ini';
end;

procedure TTestRunner.ReadDefaults;

Const
  S = 'defaults';

Var
  Ini : TMemIniFile;
  FN,F : String;

begin
  FN:=DefaultsFileName;
  if FileExists(FN) then
    begin
    Ini:=TMemIniFile.Create(FN);
    try
      F:=Ini.ReadString(S,'format','');
      if (F<>'') then
        FormatParam:=StrToFormat(F);
      FileName:=Ini.ReadString(S,'file',FileName);
      StyleSheet:=Ini.ReadString(S,'stylesheet',StyleSheet);
      ShowProgress:=Ini.ReadBool(S,'progress',ShowProgress);
      FSkipTiming:=Ini.ReadBool(S,'skiptiming',FSKipTiming);
      FSparse:=Ini.ReadBool(S,'sparse',FSparse);
      FSkipAddressInfo:=Ini.ReadBool(S,'no-addresses',FSkipAddressInfo);
      // Determine runmode
      FSuite:=Ini.ReadString(S,'suite','');
      if (FSuite<>'') then
        FRunMode:=rmSuite
      else if Ini.ReadBool(S,'all', false) then
        FRunMode:=rmAll
      else if Ini.ReadBool(S,'list',False) then
        FRunMode:=rmList;
    finally
      Ini.Free;
    end;
    end;
end;

Function TTestRunner.ParseOptions : Boolean;

begin
  Result:=True;
  if HasOption('h', 'help') or ((ParamCount = 0) and not DefaultRunAllTests) then
    begin
    Usage;
    Exit(False);
    end;
  //get the format parameter
  if HasOption('format') then
    FormatParam:=StrToFormat(GetOptionValue('format'));
  if HasOption('file') then
    FileName:=GetOptionValue('file');
  if HasOption('stylesheet') then
    StyleSheet:=GetOptionValue('stylesheet');
  if HasOption('p', 'progress') then
    ShowProgress:=True;
  if HasOption('skiptiming') then
    FSkipTiming:=True;
  if HasOption('sparse') then
    FSparse:=True;
  If HasOption('no-addresses') then
    FSkipAddressInfo:=True;
  // Determine runmode
  if HasOption('suite') then
    begin
    FSuite:=GetOptionValue('suite');
    FRunMode:=rmSuite;
    end
  else If HasOption('a','all') then
    FRunMode:=rmAll
  else if HasOption('l','list') then
    FRunMode:=rmList;
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
  S := CheckOptions(GetShortOpts, LongOpts);
  if (S <> '') then
    begin
    Writeln(S);
    Exit;
    end;
  ReadDefaults;
  if Not ParseOptions then
    exit;
  //get a list of all registed tests
  Case FRunMode of
    rmList: ShowTestList;
    rmSuite: RunSuite;
    rmAll: DoTestRun(GetTestRegistry);
  else
    Usage
  end;
end;

end.

