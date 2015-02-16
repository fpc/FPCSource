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
  protected
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
    function LoadFile(var fn: string): Boolean;
    procedure SetDir(const s: string);
    procedure SetArgs(const s: string);
  end;

implementation

procedure UnixDir(var s : string);
var i : longint;
begin
  for i:=1 to length(s) do
    if s[i]='\' then
{$ifdef win32}
  { Don't touch at '\ ' used to escapes spaces in windows file names PM }
     if (i=length(s)) or (s[i+1]<>' ') then
{$endif win32}
      s[i]:='/';
{$ifdef win32}
{$ifndef USE_MINGW_GDB}
{ for win32 we should convert e:\ into //e/ PM }
  if (length(s)>2) and (s[2]=':') and (s[3]='/') then
    s:=CygDrivePrefix+'/'+s[1]+copy(s,3,length(s));
{$endif USE_MINGW_GDB}
{$endif win32}
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

procedure TGDBController.Run;
begin
  UserScreen;
  Command('-exec-run');
  WaitForProgramStop;
end;

procedure TGDBController.TraceStep;
begin
  UserScreen;
  Command('-exec-step');
  WaitForProgramStop;
end;

procedure TGDBController.TraceNext;
begin
  UserScreen;
  Command('-exec-next');
  WaitForProgramStop;
end;

procedure TGDBController.TraceStepI;
begin
  UserScreen;
  Command('-exec-step-instruction');
  WaitForProgramStop;
end;

procedure TGDBController.TraceNextI;
begin
  UserScreen;
  Command('-exec-next-instruction');
  WaitForProgramStop;
end;

procedure TGDBController.Continue;
begin
  UserScreen;
  Command('-exec-continue');
  WaitForProgramStop;
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
