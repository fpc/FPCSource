{
    Copyright (c) 2015 by Nikolay Nikolov
    Copyright (c) 1998 by Peter Vreman

    This is a replacement for GDBCon, implemented on top of GDB/MI,
    instead of LibGDB. This allows integration of GDB/MI support in the
    text mode IDE.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit gdbmicon;

{$MODE fpc}{$H-}

interface

uses
  gdbmiint, gdbmiwrap;

type
  TGDBController = object(TGDBInterface)
  private
    procedure RunExecCommand(const Cmd: string);
  protected
    TBreakNumber,
    start_break_number: LongInt;
    in_command: LongInt;

    procedure CommandBegin(const s: string); virtual;
    procedure CommandEnd(const s: string); virtual;

  public
    constructor Init;
    destructor Done;

    procedure Command(const s: string);
    procedure Reset; virtual;
    { tracing }
    procedure StartTrace;
    procedure Run; virtual;
    procedure TraceStep;
    procedure TraceNext;
    procedure TraceStepI;
    procedure TraceNextI;
    procedure Continue; virtual;
    procedure UntilReturn; virtual;
    function BreakpointInsert(const location: string): LongInt;
    procedure SetTBreak(tbreakstring : string);
    procedure Backtrace;
    function LoadFile(var fn: string): Boolean;
    procedure SetDir(const s: string);
    procedure SetArgs(const s: string);
  end;

implementation

uses
{$ifdef Windows}
  Windebug,
{$endif Windows}
  strings;

procedure UnixDir(var s : string);
var i : longint;
begin
  for i:=1 to length(s) do
    if s[i]='\' then
{$ifdef windows}
  { Don't touch at '\ ' used to escapes spaces in windows file names PM }
     if (i=length(s)) or (s[i+1]<>' ') then
{$endif windows}
      s[i]:='/';
{$ifdef windows}
  { if we are using cygwin, we need to convert e:\ into /cygdriveprefix/e/ PM }
  if using_cygwin_gdb and (length(s)>2) and (s[2]=':') and (s[3]='/') then
    s:=CygDrivePrefix+'/'+s[1]+copy(s,3,length(s));
{$endif windows}
end;

constructor TGDBController.Init;
begin
  inherited Init;
end;

destructor TGDBController.Done;
begin
  inherited Done;
end;

procedure TGDBController.CommandBegin(const s: string);
begin
end;

procedure TGDBController.Command(const s: string);
begin
  Inc(in_command);
  CommandBegin(s);
  GDBOutputBuf.Reset;
  GDBErrorBuf.Reset;
  i_gdb_command(s);
  CommandEnd(s);
  Dec(in_command);
end;

procedure TGDBController.CommandEnd(const s: string);
begin
end;

procedure TGDBController.Reset;
begin
end;

procedure TGDBController.StartTrace;
begin
  Command('-break-insert -t PASCALMAIN');
  start_break_number := GDB.ResultRecord.Parameters['bkpt'].AsTuple['number'].AsLongInt;
  Run;
end;

procedure TGDBController.RunExecCommand(const Cmd: string);
begin
  UserScreen;
  Command(Cmd);
  WaitForProgramStop;
end;

procedure TGDBController.Run;
begin
  RunExecCommand('-exec-run');
end;

procedure TGDBController.TraceStep;
begin
  RunExecCommand('-exec-step');
end;

procedure TGDBController.TraceNext;
begin
  RunExecCommand('-exec-next');
end;

procedure TGDBController.TraceStepI;
begin
  RunExecCommand('-exec-step-instruction');
end;

procedure TGDBController.TraceNextI;
begin
  RunExecCommand('-exec-next-instruction');
end;

procedure TGDBController.Continue;
begin
  RunExecCommand('-exec-continue');
end;

procedure TGDBController.UntilReturn;
begin
  RunExecCommand('-exec-finish');
end;

function TGDBController.BreakpointInsert(const location: string): LongInt;
begin
  Command('-break-insert ' + location);
  if GDB.ResultRecord.Success then
    BreakpointInsert := GDB.ResultRecord.Parameters['bkpt'].AsTuple['number'].AsLongInt
  else
    BreakpointInsert := 0;
end;

procedure TGDBController.SetTBreak(tbreakstring : string);
begin
  Command('-break-insert -t ' + tbreakstring);
  TBreakNumber := GDB.ResultRecord.Parameters['bkpt'].AsTuple['number'].AsLongInt;
end;

procedure TGDBController.Backtrace;
var
  FrameList: TGDBMI_ListValue;
  I: LongInt;
begin
  { forget all old frames }
  clear_frames;

  Command('-stack-list-frames');
  if not GDB.ResultRecord.Success then
    exit;

  FrameList := GDB.ResultRecord.Parameters['stack'].AsList;
  frame_count := FrameList.Count;
  frames := AllocMem(SizeOf(PFrameEntry) * frame_count);
  for I := 0 to frame_count - 1 do
    frames[I] := New(PFrameEntry, Init);
  for I := 0 to FrameList.Count - 1 do
  begin
    frames[I]^.address := FrameList.ValueAt[I].AsTuple['addr'].AsPtrInt;
    frames[I]^.level := FrameList.ValueAt[I].AsTuple['level'].AsLongInt;
    if Assigned(FrameList.ValueAt[I].AsTuple['line']) then
      frames[I]^.line_number := FrameList.ValueAt[I].AsTuple['line'].AsLongInt;
    if Assigned(FrameList.ValueAt[I].AsTuple['func']) then
      frames[I]^.function_name := StrNew(PChar(FrameList.ValueAt[I].AsTuple['func'].AsString));
    if Assigned(FrameList.ValueAt[I].AsTuple['fullname']) then
      frames[I]^.file_name := StrNew(PChar(FrameList.ValueAt[I].AsTuple['fullname'].AsString));
  end;
end;

function TGDBController.LoadFile(var fn: string): Boolean;
var
  cmd: string;
begin
  getdir(0,cmd);
  UnixDir(cmd);
  Command('-environment-cd ' + cmd);
  GDBOutputBuf.Reset;
  GDBErrorBuf.Reset;
  UnixDir(fn);
  Command('-file-exec-and-symbols ' + fn);
  LoadFile := True;
end;

procedure TGDBController.SetDir(const s: string);
var
  hs: string;
begin
  hs:=s;
  UnixDir(hs);
  Command('-environment-cd ' + hs);
end;

procedure TGDBController.SetArgs(const s: string);
begin
  Command('-exec-arguments ' + s);
end;

end.
