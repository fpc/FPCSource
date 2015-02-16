{
    Copyright (c) 2015 by Nikolay Nikolov
    Copyright (c) 1998 by Peter Vreman

    This is a replacement for GDBInt, implemented on top of GDB/MI,
    instead of LibGDB. This allows integration of GDB/MI support in the
    text mode IDE.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit gdbmiint;

{$MODE fpc}{$H-}

interface

uses
  gdbmiwrap;

type
  CORE_ADDR = PtrInt;

  PPFrameEntry = ^PFrameEntry;
  PFrameEntry = ^TFrameEntry;
  TFrameEntry = object
  private
    procedure Reset;
    procedure Clear;
  public
    file_name: PChar;
    function_name: PChar;
    args: PChar;
    line_number: LongInt;
    address: PtrInt;
    constructor Init;
    destructor Done;
  end;

  TGDBBuffer = object
  private
    buf: PChar;
    size, idx: LongInt;
    procedure Resize(nsize: LongInt);
    procedure Append(p: PChar);
    procedure LAppend(p: PChar; len: LongInt);
  public
    constructor Init;
    destructor Done;
    procedure Reset;
  end;

  TGDBInterface = object
  private
    user_screen_shown: Boolean;
    frame_size: LongInt;
  protected
    GDB: TGDBWrapper;

    procedure i_gdb_command(const S: string);
    procedure WaitForProgramStop;
    procedure ProcessResponse;
  public
    GDBErrorBuf: TGDBBuffer;
    GDBOutputBuf: TGDBBuffer;
    got_error: Boolean;
    reset_command: Boolean;
    Debuggee_started: Boolean;
    { frames and frame info while recording a frame }
    frames: PPFrameEntry;
    frame_count: LongInt;
    command_level,
    stop_breakpoint_number: LongInt;
    signal_name: PChar;
    signal_string: PChar;
    current_pc: CORE_ADDR;
    last_breakpoint_number: LongInt;
    switch_to_user: Boolean;

    { init }
    constructor Init;
    destructor Done;
    { from gdbcon }
    function GetOutput: PChar;
    function GetError: PChar;
    { Lowlevel }
    function error: Boolean;
    function error_num: LongInt;
    function get_current_frame: PtrInt;
    function set_current_frame(level: LongInt): Boolean;
    procedure clear_frames;
    { Highlevel }
    procedure DebuggerScreen;
    procedure UserScreen;
    procedure FlushAll; virtual;
    function Query(question: PChar; args: PChar): LongInt; virtual;
    { Hooks }
    procedure DoSelectSourceline(const fn: string; line: LongInt); virtual;
    procedure DoStartSession; virtual;
    procedure DoBreakSession; virtual;
    procedure DoEndSession(code: LongInt); virtual;
    procedure DoUserSignal; virtual;
    procedure DoDebuggerScreen; virtual;
    procedure DoUserScreen; virtual;
    function AllowQuit: Boolean; virtual;
  end;

const
  use_gdb_file: Boolean = False;

var
  gdb_file: Text;

function GDBVersion: string;

implementation

uses
  strings;

constructor TFrameEntry.Init;
begin
  Reset;
end;

destructor TFrameEntry.Done;
begin
  Clear;
end;

procedure TFrameEntry.Reset;
begin
  file_name := nil;
  function_name := nil;
  args := nil;
  line_number := 0;
  address := 0;
end;

procedure TFrameEntry.Clear;
begin
  if Assigned(file_name) then
    StrDispose(file_name);
  if Assigned(function_name) then
    StrDispose(function_name);
  if Assigned(args) then
    StrDispose(args);
  Reset;
end;

const
  BlockSize = 2048;

constructor TGDBBuffer.Init;
begin
  buf := nil;
  size := 0;
  Resize(BlockSize);
  Reset;
end;

destructor TGDBBuffer.Done;
begin
  if Assigned(buf) then
    FreeMem(buf, size);
end;

procedure TGDBBuffer.Reset;
begin
  idx := 0;
  buf[0] := #0;
end;

procedure TGDBBuffer.Resize(nsize: LongInt);
var
  np: PChar;
begin
  nsize := ((nsize + BlockSize - 1) div BlockSize) * BlockSize;
  GetMem(np, nsize);
  if Assigned(buf) then
  begin
    Move(buf^, np^, size);
    FreeMem(buf, size);
  end;
  buf := np;
  size := nsize;
end;

procedure TGDBBuffer.Append(p: PChar);
var
  len: LongInt;
begin
  if not Assigned(p) then
    exit;
  len := StrLen(p);
  LAppend(p, len);
end;

procedure TGDBBuffer.LAppend(p: PChar; len: LongInt);
begin
  if not Assigned(p) then
    exit;
  if (len + idx + 1) > size then
    Resize(len + idx + 1);
  Move(p^, buf[idx], len);
  Inc(idx, len);
  buf[idx] := #0;
end;

constructor TGDBInterface.Init;
begin
  GDBErrorBuf.Init;
  GDBOutputBuf.Init;
  GDB := TGDBWrapper.Create;
  command_level := 0;
end;

destructor TGDBInterface.Done;
begin
  GDB.Free;
  GDBErrorBuf.Done;
  GDBOutputBuf.Done;
end;

function TGDBInterface.GetOutput: PChar;
begin
  GetOutput := GDBOutputBuf.buf;
end;

function TGDBInterface.GetError: PChar;
var
  p: PChar;
begin
  p := GDBErrorBuf.buf;
  if (p^=#0) and got_error then
    GetError := PChar(PtrInt(GDBOutputBuf.buf) + GDBOutputBuf.idx)
  else
    GetError := p;
end;

procedure TGDBInterface.i_gdb_command(const S: string);
var
  prev_stop_breakpoint_number: LongInt;
  I: LongInt;
begin
  Inc(command_level);
  got_error := False;
  if command_level = 1 then
    prev_stop_breakpoint_number := 0
  else
    prev_stop_breakpoint_number := stop_breakpoint_number;
  GDB.Command(S);
  for I := 0 to GDB.ConsoleStream.Count - 1 do
    GDBOutputBuf.Append(PChar(GDB.ConsoleStream[I]));
  ProcessResponse;
  Dec(command_level);
  stop_breakpoint_number := prev_stop_breakpoint_number;
end;

procedure TGDBInterface.WaitForProgramStop;
var
  Line: LongInt;
begin
  GDB.WaitForProgramStop;
  if not GDB.Alive then
  begin
    DebuggerScreen;
    current_pc := 0;
    Debuggee_started := False;
    exit;
  end;
  ProcessResponse;
  case GDB.ExecAsyncOutput.Parameters['reason'].AsString of
    'breakpoint-hit':
      begin
        stop_breakpoint_number := GDB.ExecAsyncOutput.Parameters['bkptno'].AsLongInt;
        DebuggerScreen;
        Debuggee_started := True;
        DoSelectSourceLine(GDB.ExecAsyncOutput.Parameters['frame'].AsTuple['fullname'].AsString, GDB.ExecAsyncOutput.Parameters['frame'].AsTuple['line'].AsLongInt);
      end;
    'end-stepping-range':
      begin
        DebuggerScreen;
        Debuggee_started := True;
        current_pc := GDB.ExecAsyncOutput.Parameters['frame'].AsTuple['addr'].AsPtrInt;
        DoSelectSourceLine(GDB.ExecAsyncOutput.Parameters['frame'].AsTuple['fullname'].AsString, GDB.ExecAsyncOutput.Parameters['frame'].AsTuple['line'].AsLongInt);
      end;
    'exited':
      begin
        DebuggerScreen;
        current_pc := 0;
        Debuggee_started := False;
        DoEndSession(GDB.ExecAsyncOutput.Parameters['exit-code'].AsLongInt);
      end;
    'exited-normally':
      begin
        DebuggerScreen;
        current_pc := 0;
        Debuggee_started := False;
        DoEndSession(0);
      end;
  end;
end;

procedure TGDBInterface.ProcessResponse;
var
  NAO: TGDBMI_AsyncOutput;
  Code: LongInt;
begin
  for NAO in GDB.NotifyAsyncOutput do
  begin
    if NAO.AsyncClass = 'breakpoint-created' then
    begin
//      Writeln('BREAKPOINT created!');
      Val(NAO.Parameters['bkpt'].AsTuple['number'].AsString, last_breakpoint_number, Code);
//      Writeln('last_breakpoint_number=', last_breakpoint_number);
//      if Assigned(NAO.Parameters['bkpt'].AsTuple['file']) then
//        Writeln('file = ', NAO.Parameters['bkpt'].AsTuple['file'].AsString);
//      Readln;
    end;
  end;
end;

function TGDBInterface.error: Boolean;
begin
  error := got_error or not GDB.Alive;
end;

function TGDBInterface.error_num: LongInt;
begin
  error_num := 0;  { TODO }
end;

function TGDBInterface.get_current_frame: PtrInt;
begin
end;

function TGDBInterface.set_current_frame(level: LongInt): Boolean;
begin
end;

procedure TGDBInterface.clear_frames;
var
  I: LongInt;
begin
  for I := 0 to frame_size - 1 do
    Dispose(frames[I], Done);
  if Assigned(frames) then
  begin
    FreeMem(frames, SizeOf(Pointer) * frame_size);
    frames := nil;
  end;
  frame_count := 0;
  frame_size := 0;
end;

procedure TGDBInterface.DebuggerScreen;
begin
  if user_screen_shown then
    DoDebuggerScreen;
  user_screen_shown := False;
end;

procedure TGDBInterface.UserScreen;
begin
  if switch_to_user then
  begin
    if not user_screen_shown then
      DoUserScreen;
    user_screen_shown := True;
  end;
end;

procedure TGDBInterface.FlushAll;
begin
end;

function TGDBInterface.Query(question: PChar; args: PChar): LongInt;
begin
  Query := 0;
end;

procedure TGDBInterface.DoSelectSourceline(const fn: string; line: LongInt);
begin
end;

procedure TGDBInterface.DoStartSession;
begin
end;

procedure TGDBInterface.DoBreakSession;
begin
end;

procedure TGDBInterface.DoEndSession(code: LongInt);
begin
end;

procedure TGDBInterface.DoUserSignal;
begin
end;

procedure TGDBInterface.DoDebuggerScreen;
begin
end;

procedure TGDBInterface.DoUserScreen;
begin
end;

function TGDBInterface.AllowQuit: Boolean;
begin
  AllowQuit := True;
end;

var
  CachedGDBVersion: string;

function GDBVersion: string;
var
  GDB: TGDBWrapper;
begin
  if CachedGDBVersion <> '' then
  begin
    GDBVersion := CachedGDBVersion;
    exit;
  end;
  GDBVersion := '';
  GDB := TGDBWrapper.Create;
  GDB.Command('-gdb-version');
  if GDB.ConsoleStream.Count > 0 then
    GDBVersion := GDB.ConsoleStream[0];
  if (GDBVersion <> '') and (GDBVersion[Length(GDBVersion)]=#10) then
    Delete(GDBVersion, Length(GDBVersion), 1);
  GDB.Free;
  CachedGDBVersion := GDBVersion;
  if GDBVersion = '' then
    GDBVersion := 'GDB missing or does not work';
end;

begin
  CachedGDBVersion := '';
end.
