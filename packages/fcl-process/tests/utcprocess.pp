unit utcprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, pipes, process;

type

  { TTestProcess }

  TTestProcess= class(TTestCase)
  private
    FProc: TProcess;
    FProc2: TProcess;
    FProc3: TProcess;
    procedure AssertFileContent(const aFileName, aContent: String);
    procedure AssertFileContent(const aFileName: String; aContent: array of string);
    procedure AssertGenOutLines(const S: String; aCount: integer);
    procedure AssertGenOutLinesFile(const aFileName : string; aCount : Integer);
    procedure CreateInputLinesFile(const aFileName : string; aCount : Integer);
    function GetHelper(const aHelper: string): String;
    function GetTestFile(const aName: string): String;
    function ReadProcessOutput(aProc: TProcess; ReadStdErr : Boolean = False): string;
    procedure WaitForFile(const aFileName: String);
  protected
    procedure CheckHelper(const aHelper : string);
    procedure SetUp; override;
    procedure TearDown; override;
    property Proc : TProcess read FProc;
    property Proc2 : TProcess read FProc2;
    property Proc3 : TProcess read FProc3;
  published
    procedure TestHookUp;
    procedure TestSimple;
    procedure TestSimpleParam;
    Procedure TestExitStatus;
    Procedure TestWaitFor;
    Procedure TestOptionWaitOnExit;
    Procedure TestTerminate;
    Procedure TestPipes;
    Procedure TestWritePipes;
    Procedure TestStdErr;
    Procedure TestStdErrToOutput;
    Procedure TestInputFile;
    Procedure TestOutputFile;
    Procedure TestStdErrFile;
    Procedure TestStdErrToStdOut;
    Procedure TestInputNull;
    Procedure TestOutputFileExistingAppend;
    Procedure TestOutputFileExistingTruncate;
    Procedure TestOutputFileExistingAtStart;
    Procedure TestPipeOut;
    Procedure TestPipeOutToFile;
    Procedure TestPipeInOutToFile;
    Procedure TestPipeRestart;
  end;

implementation

uses dateutils;

const
  dotouch = 'tdotouch';
  docat = 'tdocat';
  doexit = 'tdoexit';
  genout = 't_genout';
  fntouch = 'touch.txt';
  fntestoutput = 'output.txt';
  fntestinput = 'input.txt';

var
  TestDir : String;
  TmpDir : String;

procedure TTestProcess.AssertFileContent(const aFileName,aContent : String);
begin
  AssertFileContent(aFileName,[aContent]);
end;

procedure TTestProcess.AssertFileContent(const aFileName : String; aContent : Array of string);

var
  L : TStrings;
  I : integer;

begin
  L:=TStringList.Create;
  try
    L.LoadFromFile(aFileName);
    AssertEquals('Line count',Length(aContent),L.Count);
    for I:=0 to L.Count-1 do
      AssertEquals('Line '+Inttostr(i)+'content',aContent[I],L[i]);
  finally
    L.Free;
  end;

end;

Procedure TTestProcess.WaitForFile(const aFileName : String);

var
  aCount : Integer;
  FN : String;
  Exists : boolean;

begin
  FN:=aFileName;
  aCount:=0;
  Repeat
    Sleep(20);
    Inc(aCount);
    Exists:=FileExists(FN);
  Until (aCount>=50) or Exists;
  AssertTrue('File did not appear: '+FN,Exists);
  Sleep(20);
end;

procedure TTestProcess.TestHookUp;

  procedure AssertNoFile(const FN :string);
  begin
    AssertFalse('File '+FN+' does not exist',FileExists(FN));
  end;

begin
  AssertNotNull('Have process 1',Proc);
  AssertNotNull('Have process 2',Proc2);
  AssertNotNull('Have process 3',Proc3);
  AssertNoFile(fntouch);
  AssertNoFile(GetTestFile(fnTouch));
  AssertNoFile(GetTestFile(fntestoutput));
end;

procedure TTestProcess.TestSimple;

begin
  Proc.Executable:=GetHelper(dotouch);
  Proc.Execute;
  AssertNull('no input stream',Proc.Input);
  AssertNull('no output stream',Proc.Output);
  AssertNull('no error stream',Proc.Stderr);
  WaitForFile(fntouch);
  AssertFileContent(fntouch,fntouch);
end;

procedure TTestProcess.TestSimpleParam;

var
  FN : String;
begin
  FN:=GetTestFile(fntouch);
  Proc.Executable:=GetHelper(dotouch);
  Proc.Parameters.Add(FN);
  Proc.Execute;
  WaitForFile(FN);
  AssertFileContent(FN,FN);
end;

procedure TTestProcess.TestExitStatus;
// Test that halt(23) results in 23...
begin
  Proc.Executable:=GetHelper(doexit);
  Proc.Parameters.Add('23');
  Proc.Execute;
  Proc.WaitOnExit;
  AssertEquals('Exit code',23,Proc.ExitStatus);
end;

procedure TTestProcess.TestWaitFor;

var
  N : TDateTime;
  ms : Int64;

begin
  Proc.Executable:=GetHelper(doexit);
  Proc.Parameters.Add('0');
  Proc.Parameters.Add('1000');
  N:=Now;
  Proc.Execute;
  Proc.WaitOnExit;
  ms:=MilliSecondsBetween(Now,N);
  AssertEquals('Exit code',0,Proc.ExitStatus);
  AssertTrue('Wait time',ms>900);

end;

procedure TTestProcess.TestOptionWaitOnExit;
var
  N : TDateTime;
  ms : Int64;

begin
  Proc.Executable:=GetHelper(doexit);
  Proc.Parameters.Add('0');
  Proc.Parameters.Add('1000');
  N:=Now;
  Proc.Options:=Proc.Options+[poWaitOnExit];
  Proc.Execute;
  ms:=MilliSecondsBetween(Now,N);
  AssertEquals('Exit code',0,Proc.ExitStatus);
  AssertTrue('Wait time',ms>900);
end;

procedure TTestProcess.TestTerminate;

var
  N : TDateTime;
  ms : Int64;

begin
  Proc.Executable:=GetHelper(doexit);
  Proc.Parameters.Add('0');
  Proc.Parameters.Add('2000');
  N:=Now;
  Proc.Execute;
  Sleep(500);
  Proc.Terminate(23);
  ms:=MilliSecondsBetween(Now,N);
  AssertTrue('Process exits at once',ms<1000);
{$IFDEF UNIX}
  // Also check Kill if term will not work
  AssertTrue('Exit status',(15=Proc.ExitStatus) or (9=Proc.ExitStatus));
{$ENDIF}
{$IFDEF WINDOWS}
  // Check exit status provided to terminate.
  AssertTrue('Exit status',(23=Proc.ExitCode));
{$ENDIF}
end;

procedure TTestProcess.AssertGenOutLines(const S : String; aCount : integer);

var
  L : TStrings;
  I : Integer;

begin
  sleep(100);
//  Writeln('Testing >>',S,'<<');
  L:=TStringList.Create;
  try
    L.Text:=S;
    AssertEquals('Count',aCount,L.Count);
    For I:=1 to aCount do
      AssertEquals('Content Line '+IntToStr(I),'Line '+IntToStr(I),L[I-1]);
  finally
    L.Free;
  end;
end;

procedure TTestProcess.AssertGenOutLinesFile(const aFileName: string; aCount: Integer);
var
  L : TStrings;
  I : Integer;

begin
  sleep(100);
  // Writeln('Testing file >>',aFileName,'<<');
  L:=TStringList.Create;
  try
    L.LoadFromFile(aFileName);
    AssertEquals('Count',aCount,L.Count);
    For I:=1 to aCount do
      AssertEquals('Content Line '+IntToStr(I),'Line '+IntToStr(I),L[I-1]);
  finally
    L.Free;
  end;
end;

procedure TTestProcess.CreateInputLinesFile(const aFileName: string; aCount: Integer);
var
  L : TStrings;
  I : Integer;

begin
  // Writeln('Creating Test file >>',aFileName,'<<');
  L:=TStringList.Create;
  try
    For I:=1 to aCount do
      L.Add('Line '+IntToStr(I));
    L.SaveToFile(aFileName);
  finally
    L.Free;
  end;
end;

function TTestProcess.ReadProcessOutput(aProc: TProcess; ReadStdErr: Boolean): string;

var
  aRead,aLen: Integer;
  S : String;
  St : TInputPipeStream;
begin
  aRead:=0;
  aLen:=0;
  S:='';
  Sleep(100);
  if ReadStdErr then
    st:=aProc.StdErr
  else
    st:=aProc.Output;
  AssertNotNull('Have stream to read output from',St);
  AssertTrue('Read input',aProc.ReadInputStream(St,aRead,aLen,S,100));
  SetLength(S,aRead);
//  Writeln('>>>',S,'<<<');
  Result:=S;
end;

procedure TTestProcess.TestPipes;

var
  S : String;

begin
  Proc.Executable:=GetHelper(genout);
  Proc.Options:=[poUsePipes];
  Proc.Execute;
  AssertNotNull('input stream',Proc.Input);
  AssertNotNull('output stream',Proc.Output);
  AssertNotNull('error stream',Proc.Stderr);
  S:=ReadProcessOutput(Proc);
  AssertGenOutLines(S,3);
end;

procedure TTestProcess.TestWritePipes;
var
  Sin,Sout : String;

begin
  Proc.Executable:=GetHelper(docat);
  Proc.Options:=[poUsePipes];
  Proc.Execute;
  // Note: this test will only work for small amounts of data, less than pipe buffer size.
  Sin:='this is some text'+sLineBreak+'And some more text'+sLineBreak;
  Proc.Input.Write(Sin[1],Length(Sin));
  Proc.CloseInput;
  SOut:=ReadProcessOutput(Proc);
  AssertEquals('Out equals in',SIn,Sout);
end;

procedure TTestProcess.TestStdErr;
var
  S : String;

begin
  Proc.Executable:=GetHelper(genout);
  Proc.Parameters.Add('-3');
  Proc.Options:=[poUsePipes];
  Proc.Execute;
  S:=ReadProcessOutput(Proc,true);
  AssertGenOutLines(S,3);
end;

procedure TTestProcess.TestStdErrToOutput;
var
  S : String;

begin
  Proc.Executable:=GetHelper(genout);
  Proc.Parameters.Add('-3');
  Proc.Options:=[poUsePipes,poStderrToOutPut];
  Proc.Execute;
  S:=ReadProcessOutput(Proc);
  AssertGenOutLines(S,3);
end;

procedure TTestProcess.TestInputFile;

var
  S : String;

begin
  CreateInputLinesFile(GetTestFile(fntestinput),3);
  Proc.Executable:=GetHelper(docat);
  Proc.InputDescriptor.FileName:=GetTestFile(fntestinput);
  AssertTrue('Descriptor IOType', Proc.InputDescriptor.IOType=iotFile);
  Proc.OutputDescriptor.IOType:=iotPipe;
  Proc.Execute;
  AssertNull('input stream',Proc.Input);
  AssertNotNull('output stream',Proc.Output);
  AssertNull('error stream',Proc.Stderr);
  S:=ReadProcessOutput(Proc);
  AssertGenOutLines(S,3);
end;

procedure TTestProcess.TestOutputFile;

begin
  Proc.Executable:=GetHelper(genout);
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.Execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestStdErrFile;
begin
  Proc.Executable:=GetHelper(genout);
  Proc.Parameters.Add('-3');
  Proc.ErrorDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.Execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestStdErrToStdOut;
begin
  Proc.Executable:=GetHelper(genout);
  Proc.Options:=Proc.Options+[poStderrToOutPut];
  Proc.Parameters.Add('-3');
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.Execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestInputNull;

var
  B : TBytes;

begin
  Proc.Executable:=GetHelper(docat);
  Proc.InputDescriptor.IOType:=iotNull;
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.Execute;
  Sleep(100);
  B:=Sysutils.GetFileContents(GetTestFile(fntestoutput));
  AssertEquals('Empty file',0,Length(B));
end;

procedure TTestProcess.TestOutputFileExistingAppend;
// Check that we actually append
begin
  CreateInputLinesFile(GetTestFile(fntestoutput),3);
  Proc.Executable:=GetHelper(genout);
  Proc.Parameters.add('3');
  Proc.Parameters.add('3');
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.OutputDescriptor.FileWriteMode:=fwmAppend;
  Proc.Execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),6);

end;

procedure TTestProcess.TestOutputFileExistingTruncate;
// Check that we actually rewrite
begin
  CreateInputLinesFile(GetTestFile(fntestoutput),6);
  AssertGenOutLinesFile(GetTestFile(fntestoutput),6);
  Proc.Executable:=GetHelper(genout);
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.OutputDescriptor.FileWriteMode:=fwmTruncate;
  Proc.Execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestOutputFileExistingAtStart;
// Check that we actually write at start of file...
// Write file with 6 lines (1-6), overwrite files with first 3 lines 7-9
// Result has 7 - 8 - 9 - 4 - 5 -6

var
  L : TStrings;
  I : Integer;
begin
  CreateInputLinesFile(GetTestFile(fntestoutput),6);
  Proc.Executable:=GetHelper(genout);
  Proc.Parameters.add('3');
  Proc.Parameters.add('6'); // Offset 6, so first output line is 7
  Proc.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.OutputDescriptor.FileWriteMode:=fwmAtStart;
  Proc.Execute;
  sleep(100);
  // Writeln('Testing file >>',aFileName,'<<');
  L:=TStringList.Create;
  try
    L.LoadFromFile(GetTestFile(fntestoutput));
    AssertEquals('Count',6,L.Count);
    For I:=1 to 3 do
      AssertEquals('Content Line '+IntToStr(I),'Line '+IntToStr(I+6),L[I-1]);
    For I:=4 to 6 do
      AssertEquals('Content Line '+IntToStr(I),'Line '+IntToStr(I),L[I-1]);
  finally
    L.Free;
  end;

end;

procedure TTestProcess.TestPipeOut;
{ Simulate
  genout | docat
  we read output of docat.
}
var
  S : String;

begin
  Proc.Executable:=GetHelper(genout);
  Proc2.Executable:=GetHelper(docat);
  Proc2.OutputDescriptor.IOType:=iotPipe;
  Proc.OutputDescriptor.Process:=Proc2;
  AssertTrue('Proc2 input is pipe',Proc2.InputDescriptor.IOType=iotPipe);
  Proc2.Execute;
  Proc.execute;
  S:=ReadProcessOutput(Proc2);
  AssertGenOutLines(S,3);
end;

procedure TTestProcess.TestPipeOutToFile;

{ Simulate
  genout | docat > file
  we read output from file
}
var
  S : String;

begin
  Proc.Executable:=GetHelper(genout);
  Proc2.Executable:=GetHelper(docat);
  Proc2.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.OutputDescriptor.Process:=Proc2;
  AssertTrue('Proc2 input is pipe',Proc2.InputDescriptor.IOType=iotPipe);
  Proc2.Execute;
  Proc.execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestPipeInOutToFile;
{ Simulate
  docat <input | docat > file
  we read output from file
}
var
  S : String;

begin
  CreateInputLinesFile(GetTestFile(fntestinput),3);
  Proc.Executable:=GetHelper(docat);
  Proc.InputDescriptor.FileName:=GetTestFile(fntestinput);
  Proc2.Executable:=GetHelper(docat);
  Proc2.OutputDescriptor.FileName:=GetTestFile(fntestoutput);
  Proc.OutputDescriptor.Process:=Proc2;
  AssertTrue('Proc2 input is pipe',Proc2.InputDescriptor.IOType=iotPipe);
  Proc2.Execute;
  Proc.execute;
  AssertGenOutLinesFile(GetTestFile(fntestoutput),3);
end;

procedure TTestProcess.TestPipeRestart;
begin

end;

function TTestProcess.GetTestFile(const aName: string) : String;

begin
  if TmpDir='' then
    TmpDir:=GetTempDir(False);
  Result:=IncludeTrailingPathDelimiter(TmpDir)+aName;
end;

function TTestProcess.GetHelper(const aHelper: string) : String;
begin
  if TestDir='' then
    TestDir:=ExtractFilePath(ParamStr(0));
  Result:=IncludeTrailingPathDelimiter(TestDir)+aHelper;
  {$IFDEF WINDOWS}
  Result:=Result+'.exe';
  {$ENDIF}
end;

procedure TTestProcess.CheckHelper(const aHelper: string);
var
  F : String;
begin
  F:=GetHelper(aHelper);
  AssertTrue('No helper '+F+' please compile '+aHelper+'.pp',FileExists(F));
end;

procedure TTestProcess.SetUp;
begin
  FProc:=TProcess.Create(Nil);
  FProc2:=TProcess.Create(Nil);
  FProc3:=TProcess.Create(Nil);
  // CheckHelper(dols);
  CheckHelper(genout);
  CheckHelper(docat);
  CheckHelper(dotouch);
  CheckHelper(doexit);
  DeleteFile(fntouch);
  DeleteFile(GetTestFile(fntouch));
  DeleteFile(GetTestFile(fntestoutput));
end;

procedure TTestProcess.TearDown;
begin
  FreeAndNil(FProc);
end;

initialization
  RegisterTest(TTestProcess);
end.

