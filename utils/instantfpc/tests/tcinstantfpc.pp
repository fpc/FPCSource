{ Tests for the instantfpc helper routines and for running scripts that use relative paths.
  The run tests need the environment variable INSTANTFPC (path of the instantfpc binary);
  FPCCOMPILER optionally names the compiler instantfpc calls. }
unit tcInstantFPC;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpcunit, testregistry, InstantFPTools;

type

  { TTestInstantFPCDir }

  TTestInstantFPCDir = class(TTestCase)
  private
    FDir: string;
    procedure DeleteTree(const aDir: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure WriteLines(const aFilename: string; const aLines: array of string);
  public
    // Temporary directory of the test, with a trailing path delimiter.
    property Dir: string read FDir;
  end;

  { TTestInstantFPCTools }

  TTestInstantFPCTools = class(TTestInstantFPCDir)
  published
    procedure TestParametersSkipShebang;
    procedure TestParametersNoSkipShebang;
    procedure TestCanCompileShebang;
    procedure TestCanCompileBOMShebang;
    procedure TestCanCompileNoShebang;
    procedure TestCanCompileNoExtension;
    procedure TestCannotCompileHashLine;
    procedure TestCannotCompileMissing;
    procedure TestCannotCompileExtensionClash;
  end;

  { TTestInstantFPCRun }

  TTestInstantFPCRun = class(TTestInstantFPCDir)
  private
    FInstantFPC: string;
    FCompiler: string;
    function RunScript(const aScript: string; out aOutput: string): Integer;
  protected
    procedure SetUp; override;
  published
    procedure TestRelativeUnitPath;
    procedure TestRelativeInclude;
    procedure TestUnitNextToScript;
    procedure TestErrorLine;
  end;

implementation

{ TTestInstantFPCDir }

procedure TTestInstantFPCDir.DeleteTree(const aDir: string);

var
  lInfo: TSearchRec;

begin
  if FindFirst(aDir+AllFilesMask,faAnyFile,lInfo)=0 then
    try
      repeat
        if (lInfo.Name='.') or (lInfo.Name='..') then
          continue;
        if (lInfo.Attr and faDirectory)<>0 then
          DeleteTree(aDir+lInfo.Name+PathDelim)
        else
          DeleteFile(aDir+lInfo.Name);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  RemoveDir(aDir);
end;


procedure TTestInstantFPCDir.SetUp;

begin
  inherited SetUp;
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tinstantfpc'+IntToStr(GetProcessID)+PathDelim;
  if DirectoryExists(FDir) then
    DeleteTree(FDir);
  AssertTrue('Create test directory '+FDir,ForceDirectories(FDir));
end;


procedure TTestInstantFPCDir.TearDown;

begin
  if (FDir<>'') and DirectoryExists(FDir) then
    DeleteTree(FDir);
  inherited TearDown;
end;


procedure TTestInstantFPCDir.WriteLines(const aFilename: string; const aLines: array of string);

var
  lList: TStringList;
  lLine: string;

begin
  ForceDirectories(ExtractFilePath(aFilename));
  lList:=TStringList.Create;
  try
    for lLine in aLines do
      lList.Add(lLine);
    lList.SaveToFile(aFilename);
  finally
    lList.Free;
  end;
end;


{ TTestInstantFPCTools }

procedure TTestInstantFPCTools.TestParametersSkipShebang;

var
  lParams: string;

begin
  lParams:=GetCompilerParameters('script.pas','','out',true);
  AssertTrue('-Sb before the source file in "'+lParams+'"',Pos('-Sb script.pas',lParams)>0);
  AssertEquals('Source file is the last parameter','script.pas',Copy(lParams,Length(lParams)-9,10));
end;


procedure TTestInstantFPCTools.TestParametersNoSkipShebang;

var
  lParams: string;

begin
  lParams:=GetCompilerParameters('script.pas','','out');
  AssertEquals('No -Sb by default in "'+lParams+'"',0,Pos('-Sb',lParams));
end;


procedure TTestInstantFPCTools.TestCanCompileShebang;

begin
  WriteLines(Dir+'script.pas',['#!/usr/bin/env instantfpc','begin','end.']);
  AssertTrue('Script with shebang',CanCompileSource(Dir+'script.pas'));
end;


procedure TTestInstantFPCTools.TestCanCompileBOMShebang;

begin
  WriteLines(Dir+'script.pas',[#$EF#$BB#$BF'#!/usr/bin/env instantfpc','begin','end.']);
  AssertTrue('Script with BOM and shebang',CanCompileSource(Dir+'script.pas'));
end;


procedure TTestInstantFPCTools.TestCanCompileNoShebang;

begin
  WriteLines(Dir+'script.pas',['begin','end.']);
  AssertTrue('Script without shebang',CanCompileSource(Dir+'script.pas'));
end;


procedure TTestInstantFPCTools.TestCanCompileNoExtension;

begin
  WriteLines(Dir+'script',['#!/usr/bin/env instantfpc','begin','end.']);
  AssertTrue('Script without extension',CanCompileSource(Dir+'script'));
end;


procedure TTestInstantFPCTools.TestCannotCompileHashLine;

begin
  WriteLines(Dir+'script.pas',['# not a shebang','begin','end.']);
  AssertFalse('First line starting with # but not #!',CanCompileSource(Dir+'script.pas'));
end;


procedure TTestInstantFPCTools.TestCannotCompileMissing;

begin
  AssertFalse('Missing file',CanCompileSource(Dir+'missing.pas'));
end;


procedure TTestInstantFPCTools.TestCannotCompileExtensionClash;

begin
  WriteLines(Dir+'script',['#!/usr/bin/env instantfpc','begin','end.']);
  WriteLines(Dir+'script.pp',['begin','end.']);
  AssertFalse('Script without extension next to script.pp',CanCompileSource(Dir+'script'));
end;


{ TTestInstantFPCRun }

procedure TTestInstantFPCRun.SetUp;

begin
  FInstantFPC:=GetEnvironmentVariable('INSTANTFPC');
  FCompiler:=GetEnvironmentVariable('FPCCOMPILER');
  if FInstantFPC='' then
    Ignore('Set INSTANTFPC to the instantfpc binary to run this test');
  inherited SetUp;
end;


function TTestInstantFPCRun.RunScript(const aScript: string; out aOutput: string): Integer;

var
  lProc: TProcess;
  lStream: TStringStream;
  lBuf: array[1..4096] of byte;
  lCount: LongInt;
  lIndex: Integer;

begin
  ForceDirectories(Dir+'cwd'+PathDelim+'deeper');
  lProc:=TProcess.Create(nil);
  lStream:=TStringStream.Create('');
  try
    lProc.Executable:=FInstantFPC;
    if FCompiler<>'' then
      lProc.Parameters.Add('--compiler='+FCompiler);
    lProc.Parameters.Add('-B');
    lProc.Parameters.Add(aScript);
    lProc.CurrentDirectory:=Dir+'cwd'+PathDelim+'deeper';
    for lIndex:=1 to GetEnvironmentVariableCount do
      lProc.Environment.Add(GetEnvironmentString(lIndex));
    lProc.Environment.Add('INSTANTFPCCACHE='+Dir+'cache'+PathDelim+'deeper');
    lProc.Options:=[poUsePipes,poStdErrToOutput];
    lProc.Execute;
    repeat
      lCount:=lProc.Output.Read(lBuf{%H-},SizeOf(lBuf));
      if lCount>0 then
        lStream.Write(lBuf,lCount);
    until lCount=0;
    lProc.WaitOnExit;
    Result:=lProc.ExitStatus;
    aOutput:=lStream.DataString;
  finally
    lStream.Free;
    lProc.Free;
  end;
end;


procedure TTestInstantFPCRun.TestRelativeUnitPath;

var
  lOutput: string;
  lExitCode: Integer;

begin
  WriteLines(Dir+'lib'+PathDelim+'ulibmsg.pas',
    ['unit ulibmsg;','{$mode objfpc}{$H+}','interface','function LibMessage: string;','implementation',
     'function LibMessage: string;','begin','  Result:=''unit found'';','end;','end.']);
  WriteLines(Dir+'s'+PathDelim+'script.pas',
    ['#!/usr/bin/env instantfpc','{$UNITPATH ../lib}','uses ulibmsg;','begin','  writeln(LibMessage);','end.']);
  lExitCode:=RunScript(Dir+'s'+PathDelim+'script.pas',lOutput);
  AssertEquals('Exit code, output: '+lOutput,0,lExitCode);
  AssertTrue('Unit from {$UNITPATH ../lib} used, output: '+lOutput,Pos('unit found',lOutput)>0);
end;


procedure TTestInstantFPCRun.TestRelativeInclude;

var
  lOutput: string;
  lExitCode: Integer;

begin
  WriteLines(Dir+'inc'+PathDelim+'msg.inc',['writeln(''include found'');']);
  WriteLines(Dir+'s'+PathDelim+'script.pas',
    ['#!/usr/bin/env instantfpc','begin','  {$I ../inc/msg.inc}','end.']);
  lExitCode:=RunScript(Dir+'s'+PathDelim+'script.pas',lOutput);
  AssertEquals('Exit code, output: '+lOutput,0,lExitCode);
  AssertTrue('Include file from ../inc used, output: '+lOutput,Pos('include found',lOutput)>0);
end;


procedure TTestInstantFPCRun.TestUnitNextToScript;

var
  lOutput: string;
  lExitCode: Integer;

begin
  WriteLines(Dir+'s'+PathDelim+'unextmsg.pas',
    ['unit unextmsg;','{$mode objfpc}{$H+}','interface','function NextMessage: string;','implementation',
     'function NextMessage: string;','begin','  Result:=''next unit found'';','end;','end.']);
  WriteLines(Dir+'s'+PathDelim+'script',
    ['#!/usr/bin/env instantfpc','uses unextmsg;','begin','  writeln(NextMessage);','end.']);
  lExitCode:=RunScript(Dir+'s'+PathDelim+'script',lOutput);
  AssertEquals('Exit code, output: '+lOutput,0,lExitCode);
  AssertTrue('Unit next to the script used, output: '+lOutput,Pos('next unit found',lOutput)>0);
end;


procedure TTestInstantFPCRun.TestErrorLine;

var
  lOutput: string;
  lExitCode: Integer;

begin
  WriteLines(Dir+'s'+PathDelim+'script.pas',
    ['#!/usr/bin/env instantfpc','begin','  x:=1;','end.']);
  lExitCode:=RunScript(Dir+'s'+PathDelim+'script.pas',lOutput);
  AssertTrue('Compilation fails, output: '+lOutput,lExitCode<>0);
  AssertTrue('Error reported at line 3 of script.pas, output: '+lOutput,Pos('script.pas(3,',lOutput)>0);
end;


initialization
  RegisterTests([TTestInstantFPCTools,TTestInstantFPCRun]);
end.
