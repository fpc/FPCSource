{ Tests the threaded compile of fpmkunit with packages that need no compilation.
  Without arguments it runs itself as fpmake in a temporary directory, with a time limit,
  passing $FPCDIR as --localunitdir and $FPC as --compiler.
  With arguments it is an fpmake program: packages declared in reverse dependency order,
  or two packages that depend on each other when FPMKTEST_CYCLE=1. }
program tcompilethreads;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif} SysUtils, Classes, Process, fpmkunit;

const
  cChainLength = 6;
  cTimeLimit = 60000;
  cCycleVar = 'FPMKTEST_CYCLE';

var
  lFailures: Integer = 0;


// Declare packages pkg1..pkgN, each depending on the next one
procedure DeclareChain;

var
  lIdx: Integer;
  lPackage: TPackage;

begin
  for lIdx:=1 to cChainLength do
    begin
    lPackage:=Installer.AddPackage('pkg'+IntToStr(lIdx));
    lPackage.Version:='1.0';
    if lIdx<cChainLength then
      lPackage.Dependencies.Add('pkg'+IntToStr(lIdx+1));
    end;
end;


// Declare packages cyclea and cycleb that depend on each other
procedure DeclareCycle;

var
  lPackage: TPackage;

begin
  lPackage:=Installer.AddPackage('cyclea');
  lPackage.Version:='1.0';
  lPackage.Dependencies.Add('cycleb');
  lPackage:=Installer.AddPackage('cycleb');
  lPackage.Version:='1.0';
  lPackage.Dependencies.Add('cyclea');
end;


// Report aOK for aName, counting failures
procedure Check(aOK: Boolean; const aName: String);

begin
  if aOK then
    Writeln('ok:     ', aName)
  else
    begin
    Writeln('FAILED: ', aName);
    Inc(lFailures);
    end;
end;


// Run this program with aArgs in aDir; False if it did not end within the time limit
function RunSelf(const aDir: String; const aArgs: array of String; aCycle: Boolean;
  out aExitCode: Integer; out aOutput: String): Boolean;

var
  lProcess: TProcess;
  lOutput: TStringStream;
  lArg: String;
  lIdx: Integer;
  lStart: QWord;
  lBuffer: array[0..4095] of Byte;
  lCount: Integer;

begin
  aExitCode:=-1;
  lOutput:=TStringStream.Create('');
  lProcess:=TProcess.Create(nil);
  try
    lProcess.Executable:=ExpandFileName(ParamStr(0));
    for lArg in aArgs do
      lProcess.Parameters.Add(lArg);
    if GetEnvironmentVariable('FPCDIR')<>'' then
      lProcess.Parameters.Add('--localunitdir='+GetEnvironmentVariable('FPCDIR'));
    if GetEnvironmentVariable('FPC')<>'' then
      lProcess.Parameters.Add('--compiler='+GetEnvironmentVariable('FPC'));
    lProcess.CurrentDirectory:=aDir;
    for lIdx:=0 to GetEnvironmentVariableCount-1 do
      lProcess.Environment.Add(GetEnvironmentString(lIdx));
    if aCycle then
      lProcess.Environment.Add(cCycleVar+'=1');
    lProcess.Options:=[poUsePipes, poStderrToOutPut];
    lProcess.Execute;
    lStart:=GetTickCount64;
    repeat
      while lProcess.Output.NumBytesAvailable>0 do
        begin
        lCount:=lProcess.Output.Read(lBuffer, SizeOf(lBuffer));
        lOutput.WriteBuffer(lBuffer, lCount);
        end;
      if lProcess.Running then
        Sleep(50);
    until not lProcess.Running or (GetTickCount64-lStart>cTimeLimit);
    Result:=not lProcess.Running;
    if Result then
      begin
      repeat
        lCount:=lProcess.Output.Read(lBuffer, SizeOf(lBuffer));
        if lCount>0 then
          lOutput.WriteBuffer(lBuffer, lCount);
      until lCount<=0;
      aExitCode:=lProcess.ExitCode;
      end
    else
      lProcess.Terminate(1);
    aOutput:=lOutput.DataString;
  finally
    lProcess.Free;
    lOutput.Free;
  end;
end;


// Run fpmake with aArgs in a new directory below aBase and check the exit code and, if not empty, the output
procedure RunCase(const aBase, aName: String; const aArgs: array of String; aCycle: Boolean;
  aExpectedExit: Integer; const aExpectedText: String);

var
  lDir, lOutput: String;
  lExit: Integer;
  lEnded: Boolean;

begin
  lDir:=IncludeTrailingPathDelimiter(aBase)+aName;
  ForceDirectories(lDir);
  lEnded:=RunSelf(lDir, aArgs, aCycle, lExit, lOutput);
  Check(lEnded, aName+' ends within '+IntToStr(cTimeLimit div 1000)+' seconds');
  if not lEnded then
    Exit;
  Check(lExit=aExpectedExit, aName+' exits with code '+IntToStr(aExpectedExit)+', got '+IntToStr(lExit));
  if aExpectedText<>'' then
    Check(Pos(aExpectedText, lOutput)>0, aName+' output contains "'+aExpectedText+'"');
  if lFailures>0 then
    Writeln(lOutput);
end;


// Run all cases in a temporary directory
procedure RunTests;

var
  lBase, lPrefix: String;

begin
  lBase:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tcompilethreads-'+IntToStr(GetProcessID);
  lPrefix:='--prefix='+IncludeTrailingPathDelimiter(lBase)+'inst';
  RunCase(lBase, 'compile', ['compile', '-T', '2'], False, 0, '');
  RunCase(lBase, 'install', ['install', '-T', '2', lPrefix], False, 0, 'Installation package pkg1 for target');
  RunCase(lBase, 'serialinstall', ['install', lPrefix], False, 0, 'Installation package pkg1 for target');
  RunCase(lBase, 'cycle', ['compile', '-T', '2'], True, 1, 'unresolved dependencies: cyclea, cycleb');
  if lFailures=0 then
    Writeln('All tests passed')
  else
    begin
    Writeln(lFailures, ' test(s) failed');
    ExitCode:=1;
    end;
end;


begin
  if ParamCount=0 then
    RunTests
  else
    begin
    if GetEnvironmentVariable(cCycleVar)='1' then
      DeclareCycle
    else
      DeclareChain;
    Installer.Run;
    end;
end.
