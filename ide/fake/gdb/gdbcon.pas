{
    $Id$

    Fake GDBCon unit (including base from GDBInt)

 **********************************************************************}
unit GDBCon;
interface

type
  psyminfo=^tsyminfo;
  tsyminfo=record
    address  : longint;
    fname    : pchar;
    line     : longint;
    funcname : pchar;
  end;

  tframeentry = object
    file_name : pchar;
    function_name : pchar;
    args : pchar;
    line_number : longint;
    address : longint;
    constructor init;
    destructor done;
    procedure reset;
    procedure clear;
  end;
  pframeentry=^tframeentry;
  ppframeentry=^pframeentry;

  tgdbbuffer=object
    buf   : pchar;
    size,
    idx   : longint;
    constructor Init;
    destructor  Done;
    procedure Reset;
    procedure Resize(nsize : longint);
    procedure Append(p:pchar);
  end;

  PGDBInterface=^TGDBInterface;
  TGDBInterface=object
    gdberrorbuf,
    gdboutputbuf  : tgdbbuffer;
    command_level,
    got_error,
    reset_command,
    call_reset,
    Debugger_started : boolean;
    { frames and frame info while recording a frame }
    frames        : ppframeentry;
    frame_size,
    frame_count   : longint;
    record_frames,
    frame_begin_seen : boolean;
    stop_breakpoint_number,
    current_address,
    current_line_number,
    signal_start,
    signal_end,
    error_start,
    error_end,
    function_start,
    function_end,
    args_start,
    args_end,
    file_start,
    file_end,
    line_start,
    line_end : longint;
    { breakpoint }
    last_breakpoint_number,
    last_breakpoint_address,
    last_breakpoint_line : longint;
    last_breakpoint_file : pchar;
    invalid_breakpoint_line : boolean;
    constructor Init;
    destructor  Done;
    { functions }
    function  error:boolean;
    function  error_num:longint;
    { Hooks }
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;
    procedure DoEndSession(code:longint);virtual;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
  end;

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
    function  LoadFile(const fn:string):boolean;
    procedure SetArgs(const s : string);
    procedure ClearSymbols;
  end;


var
  curr_gdb : pgdbinterface;

const
  use_gdb_file : boolean = false;
var
  gdb_file : text;
  
  { gdb does not allow \ in dir or filenames }
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
  inherited Init;
end;


destructor TGDBController.Done;
begin
  inherited Done;
end;


procedure TGDBController.Command(const s:string);
begin
end;

procedure TGDBController.CommandBegin;
begin
end;

procedure TGDBController.CommandEnd;
begin
end;

procedure TGDBController.Reset;
begin
end;


function TGDBController.LoadFile(const fn:string):boolean;
begin
  LoadFile:=true;
end;

procedure TGDBController.SetArgs(const s : string);
begin
end;


var
  stepline : longint;
procedure TGDBController.StartTrace;
begin
  Run;
end;

procedure TGDBController.Run;
begin
  stepline:=1;
  DoSelectSourceLine('test.pas',stepline);
end;

procedure TGDBController.TraceStep;
begin
  inc(stepline);
  DoUserScreen;
  DoDebuggerScreen;
  DoSelectSourceLine('test.pas',stepline);
end;


procedure TGDBController.TraceNext;
begin
  inc(stepline,2);
  DoUserScreen;
  DoDebuggerScreen;
  DoSelectSourceLine('test.pas',stepline);
end;


procedure TGDBController.Continue;
begin
end;


procedure TGDBController.ClearSymbols;
begin
end;


procedure TGDBController.WriteErrorBuf;
begin
end;


procedure TGDBController.WriteOutputBuf;
begin
end;

function  TGDBController.GetOutput : Pchar;
begin
  GetOutput:=nil;
end;

function  TGDBController.GetError : Pchar;
begin
  GetError:=nil;
end;

constructor TGDBInterface.Init;
begin
end;


destructor TGDBInterface.Done;
begin
end;


function tgdbinterface.error:boolean;
begin
  error:=false;
end;

function tgdbinterface.error_num:longint;
begin
  error_num:=0;
end;

procedure TGDBInterface.DoSelectSourceline(const fn:string;line:longint);
begin
end;


procedure TGDBInterface.DoStartSession;
begin
end;


procedure TGDBInterface.DoBreakSession;
begin
end;


procedure TGDBInterface.DoEndSession(code:longint);
begin
end;


procedure TGDBInterface.DoDebuggerScreen;
begin
end;


procedure TGDBInterface.DoUserScreen;
begin
end;

{*****************************************************************************
                               TFrameEntry
*****************************************************************************}

constructor tframeentry.init;
begin
  Reset;
end;

destructor tframeentry.done;
begin
  Clear;
end;

procedure tframeentry.reset;
begin
  file_name:=nil;
  function_name:=nil;
  args:=nil;
  line_number:=0;
  address:=0;
end;

procedure tframeentry.clear;
begin
  if assigned(file_name) then
   strdispose(file_name);
  if assigned(function_name) then
   strdispose(function_name);
  if assigned(args) then
   strdispose(args);
  reset;
end;


{*****************************************************************************
                                 tgdbbuffer
*****************************************************************************}

const
  blocksize=2048;

constructor tgdbbuffer.init;
begin
  Buf:=nil;
  Size:=0;
  Resize(blocksize);
  Reset;
end;


destructor tgdbbuffer.done;
begin
  if assigned(buf) then
    freemem(buf,size);
end;



procedure tgdbbuffer.reset;
begin
  idx:=0;
  Buf[0]:=#0;
end;


procedure tgdbbuffer.append(p:pchar);
var
  len : longint;
begin
  if not assigned(p) then
   exit;
  len:=Strlen(p);
  if len+idx>size then
   Resize(len+idx);
  Move(p^,buf[idx],len);
  inc(idx,len);
  buf[idx]:=#0;
end;


procedure tgdbbuffer.resize(nsize : longint);
var
  np    : pchar;
begin
  nsize:=((nsize+blocksize-1) div blocksize)*blocksize;
  getmem(np,nsize);
  move(buf^,np^,size);
  freemem(buf,size);
  buf:=np;
  size:=nsize;
end;



end.
{
  $Log$
  Revision 1.9  1999-02-11 13:03:28  pierre
      Problem with last commit
    + added virtuals CommandBegin and CommandEnd
    + added command_level for TGDBInterface


  Revision 1.7  1999/02/10 09:00:43  pierre
     * duplicate call_reset removed
     * frames allocation and freeing corrected
     + GetError and GetOutput pchar function added
     + stop_breakpoint_number to know why the program stopped
       (used for watches)

  Revision 1.6  1999/02/08 17:35:07  pierre
    + added Run made TraceStep TraceNext Continue virtual

  Revision 1.5  1999/02/08 13:59:58  pierre
    - removed second debugger_started in TGDBController
    + StartTrace and Reset made virtual to be able to
      change CmResetDebugger state in IDE

  Revision 1.4  1999/02/08 11:39:33  pierre
   * reflect added setArgs in gdbint

  Revision 1.3  1999/02/06 00:04:55  florian
    * faked gdb fixed

  Revision 1.2  1999/02/04 17:19:22  peter
    * linux fixes

  Revision 1.1  1999/02/02 16:38:05  peter
    * renamed for better tp7 usage

  Revision 1.1  1999/01/28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.1  1999/01/22 10:24:17  peter
    + gdbcon fake unit

}

