{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    Lowlevel GDB interface which communicates directly with libgdb

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit GDBCon;
interface

uses
  GDBInt;

type
  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
    progname   : pchar;
    progargs   : pchar;
    in_command,
    init_count : longint;
    constructor Init;
    destructor  Done;
    procedure CommandBegin(const s:string);virtual;
    procedure Command(const s:string);
    procedure CommandEnd(const s:string);virtual;
    procedure Reset;virtual;
    { tracing }
    procedure StartTrace;
    procedure Run;virtual;
    procedure TraceStep;virtual;
    procedure TraceNext;virtual;
    procedure Continue;virtual;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  GetOutput : Pchar;
    function  GetError : Pchar;
    function  LoadFile(var fn:string):boolean;
    procedure SetArgs(const s : string);
    procedure ClearSymbols;
  end;

procedure UnixDir(var s : string);

implementation

uses
  strings;

procedure UnixDir(var s : string);
var i : longint;
begin
  for i:=1 to length(s) do
    if s[i]='\' then s[i]:='/';
end;

constructor TGDBController.Init;
begin
  inherited init;
end;


destructor TGDBController.Done;
begin
  if assigned(progname) then
    strdispose(progname);
  if assigned(progargs) then
    strdispose(progargs);
  inherited done;
end;


procedure TGDBController.Command(const s:string);
begin
  CommandBegin(s);
  gdboutputbuf.reset;
  gdberrorbuf.reset;
  inc(in_command);
  gdb_command(s);
  dec(in_command);
  {
    What is that for ?? PM
    I had to comment it because
    it resets the debuggere after each command !!
    Maybe it can happen on errors ??
  if in_command<0 then
   begin
     in_command:=0;
     inc(in_command);
     Reset;
     dec(in_command);
   end; }
  CommandEnd(s);
end;

procedure TGDBController.CommandBegin(const s:string);
begin
end;

procedure TGDBController.CommandEnd(const s:string);
begin
end;

function TGDBController.LoadFile(var fn:string):boolean;
var
  cmd : string;
begin
  getdir(0,cmd);
  UnixDir(cmd);
  cmd:='cd '+cmd;
  Command(cmd);
  GDB__Init;
  UnixDir(fn);
  if assigned(progname) then
    strdispose(progname);
  getmem(progname,length(fn)+1);
  strpcopy(progname,fn);
  if fn<>'' then
    Command('file '+fn);
  LoadFile:=true;
end;

procedure TGDBController.SetArgs(const s : string);
begin
  if assigned(progargs) then
    strdispose(progargs);
  getmem(progargs,length(s)+1);
  strpcopy(progargs,s);
  command('set args '+s);
end;

procedure TGDBController.Reset;
begin
  call_reset:=false;
{ DeleteBreakPoints(); }
  if debuggee_started then
   begin
     reset_command:=true;
     BreakSession;
     Command('kill');
     reset_command:=false;
     debuggee_started:=false;
   end;
end;

procedure TGDBController.StartTrace;
begin
  Command('tbreak PASCALMAIN');
  Run;
end;

procedure TGDBController.Run;
begin
  Command('run');
  inc(init_count);
end;


procedure TGDBController.TraceStep;
begin
  Command('step');
end;


procedure TGDBController.TraceNext;
begin
  Command('next');
end;


procedure TGDBController.Continue;
begin
  Command('continue');
end;


procedure TGDBController.ClearSymbols;
begin
  if debuggee_started then
   Reset;
  if init_count>0 then
   Command('file');
end;


procedure BufWrite(Buf : pchar);
  var p,pe : pchar;
begin
  p:=buf;
  While assigned(p) do
    begin
       pe:=strscan(p,#10);
       if pe<>nil then
         pe^:=#0;
       Writeln(p);
       { restore for dispose }
       if pe<>nil then
         pe^:=#10;
       if pe=nil then
         p:=nil
       else
         begin
           p:=pe;
           inc(p);
         end;
    end;
end;


function  TGDBController.GetOutput : Pchar;
begin
  GetOutput:=gdboutputbuf.buf;
end;

function  TGDBController.GetError : Pchar;
var p : pchar;
begin
  p:=gdberrorbuf.buf;
  if (p^=#0) and got_error then
    GetError:=pchar(longint(gdboutputbuf.buf)+gdboutputbuf.idx)
  else
    GetError:=p;
end;

procedure TGDBController.WriteErrorBuf;
begin
   BufWrite(gdberrorbuf.buf);
end;


procedure TGDBController.WriteOutputBuf;
begin
   BufWrite(gdboutputbuf.buf);
end;


end.
{
  $Log$
  Revision 1.1.2.1  2000-12-18 11:41:55  pierre
   * avoid calling file if no nmae given

  Revision 1.1  2000/07/13 06:33:58  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:32  peter
    * moved to packages dir

  Revision 1.3  1999/08/23 09:16:48  pierre
   * Better GetError code

  Revision 1.2  1999/07/12 13:08:19  pierre
    + added GDBVersion function
    * tries to intercept quit command from GDB Window
    + AllowQuit method

  Revision 1.1  1999/05/22 13:43:00  peter
    * moved

  Revision 1.12  1999/02/11 13:03:27  pierre
      Problem with last commit
    + added virtuals CommandBegin and CommandEnd
    + added command_level for TGDBInterface


  Revision 1.10  1999/02/10 09:00:42  pierre
     * duplicate call_reset removed
     * frames allocation and freeing corrected
     + GetError and GetOutput pchar function added
     + stop_breakpoint_number to know why the program stopped
       (used for watches)

  Revision 1.9  1999/02/08 17:35:09  pierre
    + added Run made TraceStep TraceNext Continue virtual

  Revision 1.8  1999/02/08 14:00:00  pierre
    - removed second debugger_started in TGDBController
    + StartTrace and Reset made virtual to be able to
      change CmResetDebugger state in IDE

  Revision 1.7  1999/02/08 11:37:13  pierre
   + added procargs var and SetArgs method

  Revision 1.6  1999/02/04 14:29:35  pierre
    + Continue command added
    * Reset inside command removed

  Revision 1.5  1999/01/22 18:07:44  pierre
   * Loadfile arg changed to var

  Revision 1.4  1999/01/22 18:05:40  pierre
   * change dir sep from  to / for dos

  Revision 1.3  1999/01/22 10:23:49  peter
    * small update to get it working with the IDE

  Revision 1.2  1999/01/18 11:01:58  pierre
   + working version for go32v2

  Revision 1.1  1998/10/07 15:57:38  peter
    * initial version

  Revision 1.1  1998/10/07 15:48:20  peter
    * initial version

}
