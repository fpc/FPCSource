{$MODE objfpc}{$H+}

uses
  SysUtils, StrUtils, Process;

function GenerateTempDir: string;
var
  FileName: string;
  TempDir: string;
  Done: Boolean = False;
begin
  TempDir := GetTempDir(False);
  repeat
    try
      FileName := TempDir + 'dosboxwrappertmp_' + IntToStr(Random(100000));
      MkDir(FileName);
      Done := True;
    except
      on E: EInOutError do
      begin
        { 5 = Access Denied, returned when a file is duplicated }
        if E.ErrorCode <> 5 then
          raise;
      end;
    end;
  until Done;
  Result := FileName + DirectorySeparator;
end;

procedure GenerateDosBoxConf(const ADosBoxDir: string);
var
  SourceConfFileName, TargetConfFileName: string;
  SourceFile, TargetFile: TextFile;
  S: string;
begin
  SourceConfFileName := ExtractFilePath(ParamStr(0)) + 'dosbox.conf';
  TargetConfFileName := ADosBoxDir + 'dosbox.conf';
  AssignFile(SourceFile, SourceConfFileName);
  AssignFile(TargetFile, TargetConfFileName);
  Reset(SourceFile);
  try
    Rewrite(TargetFile);
    try
      while not EoF(SourceFile) do
      begin
        Readln(SourceFile, S);
        S := AnsiReplaceStr(S, '$DosBoxDir', ADosBoxDir);
        Writeln(TargetFile, S);
      end;
    finally
      CloseFile(TargetFile);
    end;
  finally
    CloseFile(SourceFile);
  end;
end;

procedure CopyFile(ASrcFileName, ADestFileName: string);
var
  SrcF, DestF: File;
  OldFileMode: Integer;
  Buf: array [0..4095] of Byte;
  BytesRead: Integer;
begin
  Writeln('CopyFile ', ASrcFileName, '->', ADestFileName);
  if not AnsiEndsText('.exe', ASrcFileName) then
    ASrcFileName := ASrcFileName + '.exe';
  OldFileMode := FileMode;
  try
    AssignFile(SrcF, ASrcFileName);
    AssignFile(DestF, ADestFileName);
    FileMode := fmOpenRead;
    Reset(SrcF, 1);
    try
      FileMode := fmOpenWrite;
      try
        Rewrite(DestF, 1);
        repeat
          BlockRead(SrcF, Buf, SizeOf(Buf), BytesRead);
          BlockWrite(DestF, Buf, BytesRead);
        until BytesRead < SizeOf(Buf);
      finally
        CloseFile(DestF);
      end;
    finally
      CloseFile(SrcF);
    end;
  finally
    FileMode := OldFileMode;
  end;
end;

function ReadExitCode(const ADosBoxDir: string): Integer;
var
  F: TextFile;
begin
  AssignFile(F, ADosBoxDir + 'EXITCODE.TXT');
  Reset(F);
  try
    Readln(F, Result);
  finally
    CloseFile(F);
  end;
end;

procedure ExecuteDosBox(const ADosBoxBinaryPath, ADosBoxDir: string);
const
  Timeout = 10*15;  { 15 seconds }
var
  Process: TProcess;
  Time: Integer = 0;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := ADosBoxBinaryPath;
    Process.Parameters.Add('-conf');
    Process.Parameters.Add(ADosBoxDir + 'dosbox.conf');
    Process.Execute;
    repeat
      Inc(Time);
      if Time > Timeout then
        break;
      Sleep(100);
    until not Process.Running;
    if Process.Running then
      Process.Terminate(254);
  finally
    Process.Free;
  end;
end;

procedure Cleanup(const ADosBoxDir: string);

  procedure DeleteIfExists(const AFileName: string);
  begin
    if FileExists(AFileName) then
      DeleteFile(AFileName);
  end;

begin
  DeleteIfExists(ADosBoxDir + 'dosbox.conf');
  DeleteIfExists(ADosBoxDir + 'EXITCODE.TXT');
  DeleteIfExists(ADosBoxDir + 'EXITCODE.EXE');
  DeleteIfExists(ADosBoxDir + 'TEST.EXE');
  RmDir(ADosBoxDir);
end;

var
  DosBoxDir: string;
  ExitCode: Integer = 255;
  DosBoxBinaryPath: string;
begin
  Randomize;
  if ParamCount = 0 then
  begin
    Writeln('Usage: ' + ParamStr(0) + ' <executable>');
    halt(1);
  end;
  DosBoxBinaryPath := GetEnvironmentVariable('DOSBOX');
  if DosBoxBinaryPath = '' then
  begin
    Writeln('Please set the DOSBOX environment variable to the dosbox executable');
    halt(1);
  end;
  DosBoxDir := GenerateTempDir;
  try
    GenerateDosBoxConf(DosBoxDir);
    CopyFile(ExtractFilePath(ParamStr(0)) + 'exitcode.exe', DosBoxDir + 'EXITCODE.EXE');
    CopyFile(ParamStr(1), DosBoxDir + 'TEST.EXE');
    ExecuteDosBox(DosBoxBinaryPath, DosBoxDir);
    ExitCode := ReadExitCode(DosBoxDir);
  finally
    Cleanup(DosBoxDir);
  end;
  halt(ExitCode);
end.
