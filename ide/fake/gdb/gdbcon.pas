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
    procedure Resize;
    procedure Append(p:pchar);
  end;

  PGDBInterface=^TGDBInterface;
  TGDBInterface=object
    gdberrorbuf,
    gdboutputbuf  : tgdbbuffer;
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
    in_command,
    init_count : longint;
    debugger_started,
    call_reset : boolean;
    constructor Init;
    destructor  Done;
    procedure Command(const s:string);
    procedure Reset;
    procedure StartTrace;
    procedure TraceStep;
    procedure TraceNext;
    procedure Continue;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  LoadFile(const fn:string):boolean;
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


procedure TGDBController.Reset;
begin
end;


function TGDBController.LoadFile(const fn:string):boolean;
begin
  LoadFile:=true;
end;


var
  stepline : longint;
procedure TGDBController.StartTrace;
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

constructor tgdbbuffer.init;
begin
  Size:=0;
  Resize;
  Reset;
end;


destructor tgdbbuffer.done;
begin
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
   Resize;
  Move(p^,buf[idx],len);
  inc(idx,len);
  buf[idx]:=#0;
end;


procedure tgdbbuffer.resize;
const
  blocksize=2048;
var
  nsize : longint;
  np    : pchar;
begin
  nsize:=size+blocksize;
  getmem(np,nsize);
  move(buf^,np^,size);
  freemem(buf,size);
  buf:=np;
  size:=nsize;
end;



end.
{
  $Log$
  Revision 1.3  1999-02-06 00:04:55  florian
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

