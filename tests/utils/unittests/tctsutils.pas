unit tctsutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tsutils, tstypes;

Type

  { TTestUtilsCase }

  TTestUtilsCase = class(TTestCase)
  Private
    FConfig : TConfig;
    FFileName : String;
    FReadResult : Boolean;
  Public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoRead;
    procedure CreateFile(const aLines : Array of string);
    Property ReadResult : Boolean Read FReadResult;
  Published
    procedure TestNoFile;
    procedure TestEmptyFile;
    procedure TestNoPercent;
    procedure TestNoEndComment;
    procedure TestSpaces;
    procedure TestCodeAndNote;
    procedure TestConfig;
    procedure TestNeedOptions;
    procedure TestDelOptions;
    Procedure TestNeedTarget;
    Procedure TestSkipTarget;
    procedure TestNeedCPU;
    procedure TestSkipCPU;
    Procedure TestSkipEMU;
    Procedure TestMinVersion;
    Procedure TestMaxVersion;
    Procedure TestResultCode;
    Procedure TestUsesGraph;
    Procedure TestShouldFail;
    procedure TestNoRun;
    Procedure TestNeedLibrary;
    Procedure TestNeedAfter;
    procedure TestTimeout;
    procedure TestFiles;
    Procedure TestWPOParas;
    Procedure TestWPOPasses;
    Procedure TestDelFiles;
    Procedure TestInteractive;
  end;

implementation

{ TTestUtilsCase }

procedure TTestUtilsCase.SetUp;
begin
  inherited SetUp;
  FReadResult:=False;
  FFileName:=GetTempDir(False)+TestName+'.pas';
  if FileExists(FFileName) then
    if not DeleteFile(FFileName) then
      Fail('Failed to delete file '+FFileName);
end;

procedure TTestUtilsCase.TearDown;
begin
  if (FFileName<>'') and FileExists(FFileName) then
    if not DeleteFile(FFileName) then
      Fail('Failed to delete file '+FFileName);
  inherited TearDown;
end;

procedure TTestUtilsCase.DoRead;
begin
  FReadResult:=GetConfig('tc',FFileName,FConfig);
end;

procedure TTestUtilsCase.CreateFile(const aLines: array of string);
var
  l : TStrings;
begin
  L:=TStringList.Create;
  try
    l.AddStrings(aLines,True);
    l.SaveToFile(FFileName);
  finally
    l.Free;
  end;
end;

procedure TTestUtilsCase.TestNoFile;
begin
  DoRead;
  AssertFalse('Not read',ReadResult);
end;

procedure TTestUtilsCase.TestEmptyFile;
begin
  CreateFile([]);
  DoRead;
  AssertTrue('read',ReadResult);
end;

procedure TTestUtilsCase.TestNoPercent;
begin
  CreateFile(['{CPU=X}']);
  DoRead;
  AssertTrue('read',ReadResult);
  AssertEquals('No value','',FCOnfig.NeedCPU);
end;

procedure TTestUtilsCase.TestNoEndComment;
begin
  CreateFile(['{ %CPU=X']);
  DoRead;
  AssertTrue('read',ReadResult);
  AssertEquals('No value','',FConfig.NeedCPU);
end;

procedure TTestUtilsCase.TestSpaces;
begin
  CreateFile(['{ %CPU = X }']);
  DoRead;
  AssertTrue('read',ReadResult);
  AssertEquals('No value','X',FConfig.NeedCPU);
end;

procedure TTestUtilsCase.TestCodeAndNote;
begin
  CreateFile(['{%KNOWNRUNERROR=123X }']);
  DoRead;
  AssertTrue('read',ReadResult);
  AssertEquals('Runerror value',123,FConfig.KnownRunError);
  AssertEquals('RunError note','X',FConfig.KnownRunNote);
end;

procedure TTestUtilsCase.TestConfig;
begin
  CreateFile(['{%CONFIGFILE=X.CFG Y.CFG}']);
  DoRead;
  AssertTrue('read',ReadResult);
  AssertEquals('Runerror value','X.CFG',FConfig.ConfigFileSrc);
  AssertEquals('RunError note','Y.CFG',FConfig.ConfigFileDst);
end;

procedure TTestUtilsCase.TestNeedOptions;
begin
  CreateFile(['{%OPT=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.NeedOptions);;
end;

procedure TTestUtilsCase.TestDelOptions;
begin
  CreateFile(['{%DELOPT=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.DelOptions);;
end;

procedure TTestUtilsCase.TestNeedTarget;
begin
  CreateFile(['{%TARGET=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.NeedTarget);;
end;

procedure TTestUtilsCase.TestSkipTarget;
begin
  CreateFile(['{%SKIPTARGET=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.SkipTarget);;
end;

procedure TTestUtilsCase.TestNeedCPU;
begin
  CreateFile(['{%SKIPEMU=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.SkipEmu);;
end;

procedure TTestUtilsCase.TestSkipCPU;
begin
  CreateFile(['{%SKIPEMU=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.SkipEmu);;
end;

procedure TTestUtilsCase.TestSkipEMU;
begin
  CreateFile(['{%SKIPEMU=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.SkipEmu);;
end;

procedure TTestUtilsCase.TestMinVersion;
begin
  CreateFile(['{%VERSION=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.MinVersion);;
end;

procedure TTestUtilsCase.TestMaxVersion;
begin
  CreateFile(['{%MAXVERSION=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.MaxVersion);;
end;

procedure TTestUtilsCase.TestResultCode;
begin
  CreateFile(['{%RESULT=1}']);
  DoRead;
  AssertEquals('Value',1,FConfig.ResultCode);
end;

procedure TTestUtilsCase.TestUsesGraph;
begin
  CreateFile(['{%GRAPH}']);
  DoRead;
  AssertTrue('Value',FConfig.UsesGraph);
end;

procedure TTestUtilsCase.TestShouldFail;
begin
  CreateFile(['{%FAIL}']);
  DoRead;
  AssertTrue('Value',FConfig.ShouldFail);
end;

procedure TTestUtilsCase.TestNoRun;
begin
  CreateFile(['{%NORUN}']);
  DoRead;
  AssertTrue('Value',FConfig.NoRun);
end;

procedure TTestUtilsCase.TestNeedLibrary;
begin
  CreateFile(['{%NEEDLIBRARY}']);
  DoRead;
  AssertTrue('Value',FConfig.NeedLibrary);
end;

procedure TTestUtilsCase.TestNeedAfter;
begin
  CreateFile(['{%NEEDEDAFTER}']);
  DoRead;
  AssertTrue('Value',FConfig.NeededAfter);
end;

procedure TTestUtilsCase.TestTimeout;
begin
  CreateFile(['{%TIMEOUT=123}']);
  DoRead;
  AssertEquals('Value',123,FConfig.Timeout)
end;

procedure TTestUtilsCase.TestFiles;
begin
  CreateFile(['{%FILES=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.Files);;
end;

procedure TTestUtilsCase.TestWPOParas;
begin
  CreateFile(['{%WPOPARAS=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.wpoparas);;
end;

procedure TTestUtilsCase.TestWPOPasses;
begin
  CreateFile(['{%WPOPASSES=2}']);
  DoRead;
  AssertEquals('Value',2,FConfig.wpopasses);
end;

procedure TTestUtilsCase.TestDelFiles;
begin
  CreateFile(['{%DELFILES=XYZ}']);
  DoRead;
  AssertEquals('Value','XYZ',FConfig.DelFiles);
end;

procedure TTestUtilsCase.TestInteractive;
begin
  CreateFile(['{%INTERACTIVE}']);
  DoRead;
  AssertTrue('Value',FConfig.IsInteractive);
end;

initialization
  RegisterTest(TTestUtilsCase);
end.

