{ Tests for the command line handling of ppufiles and ppumove.
  The environment variables PPUFILES and PPUMOVE name the binaries to test;
  tests for a binary that is not named are skipped. }
unit tcPPUUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpcunit, testregistry;

type

  { TTestPPUTool }

  TTestPPUTool = class(TTestCase)
  private
    FTool: string;
    FDir: string;
    FOutput: string;
    FExitCode: Integer;
  protected
    function ToolEnvironmentName: string; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTool(const aParams: array of string);
    procedure AssertOutputContains(const aMsg, aText: string);
    procedure AssertOutputLacks(const aMsg, aText: string);
  public
    // Output of the last RunTool, standard output and standard error combined.
    property Output: string read FOutput;
    // Exit code of the last RunTool.
    property ExitCode: Integer read FExitCode;
  end;

  { TTestPPUFiles }

  TTestPPUFiles = class(TTestPPUTool)
  protected
    function ToolEnvironmentName: string; override;
  published
    procedure TestHelp;
    procedure TestHelpUpperCase;
    procedure TestHelpQuestionMark;
    procedure TestHelpWithFile;
    procedure TestNoArguments;
    procedure TestOptionsWithoutFile;
    procedure TestUnknownOption;
    procedure TestMissingFile;
  end;

  { TTestPPUMove }

  TTestPPUMove = class(TTestPPUTool)
  protected
    function ToolEnvironmentName: string; override;
  published
    procedure TestHelp;
    procedure TestNoArguments;
    procedure TestStaticOption;
    procedure TestVerboseOptionRemoved;
    procedure TestMissingFile;
  end;

implementation

{ TTestPPUTool }

procedure TTestPPUTool.SetUp;

begin
  FTool:=GetEnvironmentVariable(ToolEnvironmentName);
  if FTool='' then
    Ignore('Set '+ToolEnvironmentName+' to the binary to run this test');
  inherited SetUp;
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tppuutils'+IntToStr(GetProcessID)+PathDelim;
  AssertTrue('Create test directory '+FDir,ForceDirectories(FDir));
end;


procedure TTestPPUTool.TearDown;

begin
  if (FDir<>'') and DirectoryExists(FDir) then
    RemoveDir(FDir);
  inherited TearDown;
end;


procedure TTestPPUTool.RunTool(const aParams: array of string);

var
  lProc: TProcess;
  lStream: TStringStream;
  lBuf: array[1..4096] of byte;
  lCount: LongInt;
  lParam: string;

begin
  lProc:=TProcess.Create(nil);
  lStream:=TStringStream.Create('');
  try
    lProc.Executable:=FTool;
    for lParam in aParams do
      lProc.Parameters.Add(lParam);
    lProc.CurrentDirectory:=FDir;
    lProc.Options:=[poUsePipes,poStdErrToOutput];
    lProc.Execute;
    repeat
      lCount:=lProc.Output.Read(lBuf{%H-},SizeOf(lBuf));
      if lCount>0 then
        lStream.Write(lBuf,lCount);
    until lCount=0;
    lProc.WaitOnExit;
    FExitCode:=lProc.ExitCode;
    FOutput:=lStream.DataString;
  finally
    lStream.Free;
    lProc.Free;
  end;
end;


procedure TTestPPUTool.AssertOutputContains(const aMsg, aText: string);

begin
  AssertTrue(aMsg+': "'+aText+'" expected in output: '+Output,Pos(aText,Output)>0);
end;


procedure TTestPPUTool.AssertOutputLacks(const aMsg, aText: string);

begin
  AssertTrue(aMsg+': "'+aText+'" not expected in output: '+Output,Pos(aText,Output)=0);
end;


{ TTestPPUFiles }

function TTestPPUFiles.ToolEnvironmentName: string;

begin
  Result:='PPUFILES';
end;


procedure TTestPPUFiles.TestHelp;

begin
  RunTool(['-h']);
  AssertEquals('Exit code of -h',0,ExitCode);
  AssertOutputContains('Title','PPU-Files Version');
  AssertOutputContains('Copyright','Copyright (c)');
  AssertOutputContains('Usage line','usage: ppufiles [options]');
  AssertOutputContains('Lowercase option','    -a ');
  AssertOutputContains('Case note','case insensitive');
end;


procedure TTestPPUFiles.TestHelpUpperCase;

begin
  RunTool(['-H']);
  AssertEquals('Exit code of -H',0,ExitCode);
  AssertOutputContains('Usage line','usage: ppufiles [options]');
end;


procedure TTestPPUFiles.TestHelpQuestionMark;

begin
  RunTool(['-?']);
  AssertEquals('Exit code of -?',0,ExitCode);
  AssertOutputContains('Usage line','usage: ppufiles [options]');
end;


procedure TTestPPUFiles.TestHelpWithFile;

begin
  RunTool(['-h','nonexist']);
  AssertEquals('Exit code of -h nonexist',0,ExitCode);
  AssertOutputContains('Usage line','usage: ppufiles [options]');
end;


procedure TTestPPUFiles.TestNoArguments;

begin
  RunTool([]);
  AssertEquals('Exit code without arguments',1,ExitCode);
  AssertOutputContains('Error','No PPU file given');
  AssertOutputContains('Usage line','usage: ppufiles [options]');
end;


procedure TTestPPUFiles.TestOptionsWithoutFile;

begin
  RunTool(['-a']);
  AssertEquals('Exit code of -a without file',1,ExitCode);
  AssertOutputContains('Error','No PPU file given');
end;


procedure TTestPPUFiles.TestUnknownOption;

begin
  RunTool(['-x','some.ppu']);
  AssertEquals('Exit code of -x',1,ExitCode);
  AssertOutputContains('Error','Unknown option -x');
  AssertOutputContains('Usage line','usage: ppufiles [options]');
end;


procedure TTestPPUFiles.TestMissingFile;

begin
  RunTool(['missing.ppu']);
  AssertEquals('Exit code for a missing file',0,ExitCode);
  AssertOutputLacks('No help for a file name','usage:');
end;


{ TTestPPUMove }

function TTestPPUMove.ToolEnvironmentName: string;

begin
  Result:='PPUMOVE';
end;


procedure TTestPPUMove.TestHelp;

begin
  RunTool(['-h']);
  AssertEquals('Exit code of -h',0,ExitCode);
  AssertOutputContains('Title','PPU-Mover Version');
  AssertOutputContains('Usage line with program name only','usage: '+ExtractFileName(FTool)+' [options]');
  AssertOutputContains('Option -i','    -i <path>');
  AssertOutputContains('Option -S','    -S ');
  AssertOutputLacks('No -v option','    -v');
end;


procedure TTestPPUMove.TestNoArguments;

begin
  RunTool([]);
  AssertEquals('Exit code without arguments',1,ExitCode);
  AssertOutputContains('Usage line','[options] file [file ...]');
end;


procedure TTestPPUMove.TestStaticOption;

begin
  RunTool(['-S','missing']);
  AssertOutputLacks('-S is accepted','illegal option');
  AssertOutputContains('Missing file reported','Could not open');
  AssertEquals('Exit code for a missing file',1,ExitCode);
end;


procedure TTestPPUMove.TestVerboseOptionRemoved;

begin
  RunTool(['-v','missing']);
  AssertEquals('Exit code of -v',1,ExitCode);
  AssertOutputContains('-v is rejected','illegal option');
end;


procedure TTestPPUMove.TestMissingFile;

begin
  RunTool(['missing']);
  AssertEquals('Exit code for a missing file',1,ExitCode);
  AssertOutputContains('Missing file reported','Could not open : missing.ppu');
  AssertOutputLacks('No runtime error','EInOutError');
  AssertOutputLacks('No linking after the error','Done.');
end;


initialization
  RegisterTests([TTestPPUFiles,TTestPPUMove]);
end.
