{$MODE objfpc}{$H+}

uses
  SysUtils, StrUtils, Process;

const
  use_temp_dir : boolean = true;
  hide_execution : boolean = true;
  do_exit : boolean = true;
  verbose : boolean = false;

  dosbox_timeout : integer = 100;  { default timeout in seconds }
var
  OutputFileName : String;

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
      if verbose then
        writeln('Trying to create directory ',Filename);
      MkDir(FileName);
      Done := True;
    except
      on E: EInOutError do
      begin
        { 5 = Access Denied, returned when a file is duplicated }
        if E.ErrorCode <> 5 then
          begin
            Writeln('Directory creation failed');
            raise;
          end;
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
  OutputFileName := ADosBoxDir + 'dosbox.out';
  if verbose then
    Writeln('Using target dosbox.conf ',TargetConfFileName);
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
        S := AnsiReplaceStr(S, '$wrapper_output', OutputFileName);
        if do_exit then
          S := AnsiReplaceStr(S, '$exit', 'exit')
        else
          S := AnsiReplaceStr(S, '$exit', '');
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
  if verbose then
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

{ On modified dosbox executable it is possible to get
  a copy of all output to CON into a file, simply write it
  back to output, so it ends up into testname.elg file.
  Skip all until line beginning with 'Drive C is mounted as' }
procedure EchoOutput;
const
  SkipUntilText = 'Drive C is mounted as ';
var
  StdText : TextFile;
  st : string;
  line : longint;
  SkipUntilSeen : boolean;
begin
  if FileExists(OutputFileName) then
    begin
      if verbose then
        Writeln('Trying to open ',OutputFileName);
      try
        AssignFile(StdText, OutputFileName);
        Reset(StdText);
        if verbose then
          Writeln('Successfully opened ',OutputFileName,', copying content to output');
        try
          line:=0;
          SkipUntilSeen:=false;
          while not eof(StdText) do
            begin
              Readln(StdText,st);
              inc(line);
	      if not SkipUntilSeen then
                SkipUntilSeen:=pos(SkipUntilText,st)>0;
              if SkipUntilSeen then
                Writeln(line,': ',st);
            end;
        finally
	  if not SkipUntilSeen then
            Writeln('Could not find "',SkipUntilText,'" in file ',OutputFilename);
          Flush(output);
          CloseFile(StdText);
        end;
      finally
        if use_temp_dir then
          DeleteFile(OutputFileName);
      end;
    end;
end;

function ReadExitCode(const ADosBoxDir: string): Integer;
var
  F: TextFile;
begin
  AssignFile(F, ADosBoxDir + 'EXITCODE.TXT');
  try
    Reset(F);
    Readln(F, Result);
    if Result <> 0 then
      Writeln('ExitCode=',Result);
    CloseFile(F);
  except
    Writeln('Unable to read exitcode value');
    ReadExitCode:=127*256;
  end;
end;

procedure ExecuteDosBox(const ADosBoxBinaryPath, ADosBoxDir: string);
var
  Process: TProcess;
  Time: Integer = 0;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := ADosBoxBinaryPath;
    Process.Parameters.Add('-conf');
    Process.Parameters.Add(ADosBoxDir + 'dosbox.conf');
    if hide_execution then
      Process.ShowWindow := swoHIDE;
    Process.Execute;
    repeat
      Inc(Time);
      if (Time > 10*dosbox_timeout) and do_exit then
        break;
      Sleep(100);
    until not Process.Running;
    if Process.Running then
    begin
      Writeln('Timeout exceeded. Killing dosbox...');
      Process.Terminate(254);
    end;
  finally
    Process.Free;
    EchoOutput;
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

  if GetEnvironmentVariable('DOSBOX_NO_TEMPDIR')<>'' then
    begin
      use_temp_dir:=false;
      Writeln('use_temp_dir set to false');
    end;
  if GetEnvironmentVariable('DOSBOX_NO_HIDE')<>'' then
    begin
      hide_execution:=false;
      Writeln('hide_execution set to false');
    end;
  if GetEnvironmentVariable('DOSBOX_NO_EXIT')<>'' then
    begin
      do_exit:=false;
      Writeln('do_exit set to false');
    end;
  if GetEnvironmentVariable('DOSBOX_VERBOSE')<>'' then
    begin
      verbose:=true;
      Writeln('verbose set to true');
    end;
  if GetEnvironmentVariable('DOSBOX_TIMEOUT')<>'' then
    begin
      dosbox_timeout:=StrToInt(GetEnvironmentVariable('DOSBOX_TIMEOUT'));
      Writeln('dosbox_timeout set to ', dosbox_timeout, ' seconds');
    end;
  if ParamCount = 0 then
  begin
    Writeln('Usage: ' + ParamStr(0) + ' <executable>');
    Writeln('Set DOSBOX_NO_TEMPDIR env variable to 1 to avoid using a temporary directory');
    Writeln('Set DOSBOX_NO_HIDE to avoid running dosbox in an hidden window');
    Writeln('Set DOSBOX_NO_EXIT to avoid exiting dosbox after test has been run');
    Writeln('Set DOSBOX_TIMEOUT to set the timeout in seconds before killing the dosbox process, assuming the test has hanged');
    halt(1);
  end;
  DosBoxBinaryPath := GetEnvironmentVariable('DOSBOX');
  if DosBoxBinaryPath = '' then
  begin
    Writeln('Please set the DOSBOX environment variable to the dosbox executable');
    halt(1);
  end
  else
  begin
    Writeln('Using DOSBOX executable: ',DosBoxBinaryPath);
  end;

  { DosBoxDir is used inside dosbox.conf as a MOUNT parameter }
  if use_temp_dir then
    DosBoxDir := GenerateTempDir
  else
    begin
      Writeln('Using ',ParamStr(1));
      DosBoxDir:=ExtractFilePath(ParamStr(1));
      if DosBoxDir='' then
        DosBoxDir:=GetCurrentDir+DirectorySeparator;
      Writeln('Using DosBoxDir=',DosBoxDir);
    end;
  try
    GenerateDosBoxConf(DosBoxDir);
    CopyFile(ExtractFilePath(ParamStr(0)) + 'exitcode.exe', DosBoxDir + 'EXITCODE.EXE');
    CopyFile(ParamStr(1), DosBoxDir + 'TEST.EXE');
    ExecuteDosBox(DosBoxBinaryPath, DosBoxDir);
    ExitCode := ReadExitCode(DosBoxDir);
  finally
    if use_temp_dir then
      Cleanup(DosBoxDir);
  end;
  halt(ExitCode);
end.
