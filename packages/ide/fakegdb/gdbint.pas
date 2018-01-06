{

    Fake GDBInt unit

 **********************************************************************}
unit GDBInt;
interface

type
  CORE_ADDR = cardinal; { might be target dependent PM }
  streamtype = (afile,astring);
  C_FILE     = longint; { at least under DJGPP }
  P_C_FILE   = ^C_FILE;

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
    Debuggee_started : boolean;
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
    signal_name,
    signal_string : pchar;
    current_pc      : CORE_ADDR;
    { breakpoint }
    last_breakpoint_number,
    last_breakpoint_address,
    last_breakpoint_line : longint;
    last_breakpoint_file : pchar;
    invalid_breakpoint_line : boolean;
    { Highlevel }
    user_screen_shown,
    switch_to_user     : boolean;
    constructor Init;
    destructor  Done;
    procedure clear_frames;
    { functions }
    function  error:boolean;
    function  error_num:longint;
    function  get_current_frame : longint;
    function  set_current_frame(level : longint) : boolean;
    procedure DebuggerScreen;
    procedure UserScreen;
    { Hooks }
    procedure DoSelectSourceline(const fn:string;line:longint);virtual;
    procedure DoStartSession;virtual;
    procedure DoBreakSession;virtual;
    procedure DoEndSession(code:longint);virtual;
    procedure DoDebuggerScreen;virtual;
    procedure DoUserScreen;virtual;
    function  AllowQuit : boolean;virtual;
  end;

function  GDBVersion : string;

var
  curr_gdb : pgdbinterface;

const
  use_gdb_file : boolean = false;
var
  gdb_file : text;
  inferior_pid : longint;

implementation

uses
  strings;


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

function TGDBInterface.get_current_frame : longint;
begin
  get_current_frame:=0;
end;

function TGDBInterface.set_current_frame(level : longint) : boolean;
begin
  set_current_frame:=true;
end;

procedure TGDBInterface.Clear_Frames;
begin
end;


procedure TGDBInterface.DebuggerScreen;
begin
end;


procedure TGDBInterface.UserScreen;
begin
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

function  tgdbinterface.AllowQuit : boolean;
begin
  AllowQuit:=true;
end;

function  GDBVersion : string;
begin
  GDBVersion:='Fake GDB';
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
