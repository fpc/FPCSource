unit tstppuutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

function FileIsExecutable(const AFilename: string): boolean;
function RunTool(const Filename: string; Params: TStrings;
  WorkingDirectory: string; Quiet, WriteOnError: boolean; out Lines: TStringList): boolean;

implementation

{$IFDEF Unix}
uses BaseUnix;
{$ENDIF}

function FileIsExecutable(const AFilename: string): boolean;
{$IFDEF Unix}
var
  Info : Stat;
begin
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info{%H-})<>-1) and FPS_ISREG(info.st_mode)
       and (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
end;
{$ELSE}
begin
  Result:=FileExists(AFilename);
end;
{$ENDIF}

function RunTool(const Filename: string; Params: TStrings;
  WorkingDirectory: string; Quiet, WriteOnError: boolean; out Lines: TStringList): boolean;
var
  buf: string;
  TheProcess: TProcess;
  OutputLine: String;
  OutLen: Integer;
  LineStart, i: Integer;
begin
  Result:=false;
  Lines:=nil;
  if not FileIsExecutable(Filename) then
    raise Exception.Create('Compiler not executable: "'+Filename+'"');
  if (WorkingDirectory<>'') and not DirectoryExists(WorkingDirectory) then
    raise Exception.Create('WorkingDirectory not found "'+WorkingDirectory+'"');
  Lines:=TStringList.Create;
  buf:='';
  if (MainThreadID=GetCurrentThreadId) and not Quiet then begin
    write('Hint: RunTool: "',Filename,'"');
    for i:=0 to Params.Count-1 do
      write(' "',Params[i],'"');
    if WorkingDirectory<>'' then
      write(', WorkDir="',WorkingDirectory,'"');
    writeln;
  end;
  TheProcess := TProcess.Create(nil);
  try
    TheProcess.Executable := Filename;
    TheProcess.Parameters:=Params;
    TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
    TheProcess.ShowWindow := swoHide;
    TheProcess.CurrentDirectory:=WorkingDirectory;
    TheProcess.Execute;
    OutputLine:='';
    SetLength(buf,4096);
    repeat
      if (TheProcess.Output<>nil) then begin
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
      end else
        OutLen:=0;
      LineStart:=1;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
          Lines.Add(OutputLine);
          OutputLine:='';
          if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
          then
            inc(i);
          LineStart:=i+1;
        end;
        inc(i);
      end;
      OutputLine:=OutputLine+copy(Buf,LineStart,OutLen-LineStart+1);
    until OutLen=0;
    if OutputLine<>'' then
      Lines.Add(OutputLine);
    TheProcess.WaitOnExit;
    Result:=(TheProcess.ExitCode=0) and (TheProcess.ExitStatus=0);
  finally
    if not Result and WriteOnError then
    begin
      for i:=0 to Lines.Count-1 do
        writeln(Lines[i]);
    end;
    TheProcess.Free;
  end;
end;

end.

