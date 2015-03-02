{
    Copyright (c) 1998 by Peter Vreman

    Lowlevel GDB interface which communicates directly with libgdb

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit GDBCon;

{$ifdef USE_GDBLIBINC}
  {$i gdblib.inc}
{$else not USE_GDBLIBINC}
  {$i gdbver.inc}
{$endif not USE_GDBLIBINC}

interface

uses
  GDBInt;

type
  TBreakpointFlags = set of (bfTemporary, bfHardware);
  TWatchpointType = (wtWrite, wtReadWrite, wtRead);
  TPrintFormatType = (pfbinary, pfdecimal, pfhexadecimal, pfoctal, pfnatural);

  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
  private
    { print }
    function InternalGetValue(Const expr : string) : AnsiString;
  public
    progname,
    progdir,
    progargs   : pchar;
    TBreakNumber,
    start_break_number,
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
    procedure TraceStepI;virtual;
    procedure TraceNextI;virtual;
    procedure Continue;virtual;
    procedure UntilReturn;virtual;
    { registers }
    function GetIntRegister(const RegName: string; var Value: UInt64): Boolean;
    function GetIntRegister(const RegName: string; var Value: Int64): Boolean;
    function GetIntRegister(const RegName: string; var Value: UInt32): Boolean;
    function GetIntRegister(const RegName: string; var Value: Int32): Boolean;
    { set command }
    function SetCommand(Const SetExpr : string) : boolean;
    { print }
    function PrintCommand(const expr : string): AnsiString;
    function PrintFormattedCommand(const expr : string; Format : TPrintFormatType): AnsiString;
    { breakpoints }
    function BreakpointInsert(const location: string; BreakpointFlags: TBreakpointFlags): LongInt;
    function WatchpointInsert(const location: string; WatchpointType: TWatchpointType): LongInt;
    function BreakpointDelete(BkptNo: LongInt): Boolean;
    function BreakpointEnable(BkptNo: LongInt): Boolean;
    function BreakpointDisable(BkptNo: LongInt): Boolean;
    function BreakpointCondition(BkptNo: LongInt; const ConditionExpr: string): Boolean;
    function BreakpointSetIgnoreCount(BkptNo: LongInt; const IgnoreCount: LongInt): Boolean;
    procedure SetTBreak(tbreakstring : string);
    { frame commands }
    procedure Backtrace;
    function SelectFrameCommand(level :longint) : boolean;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  GetOutput : Pchar;
    function  GetError : Pchar;
    function  LoadFile(var fn:string):boolean;
    procedure SetDir(const s : string);
    procedure SetArgs(const s : string);
    procedure ClearSymbols;
  end;

procedure UnixDir(var s : string);

implementation

uses
{$ifdef win32}
  windows,
{$endif win32}
  dos,
  strings;

{$ifdef win32}
const
  CygDrivePrefixKey1 = 'Software';
  CygDrivePrefixKey2 = 'Cygnus Solutions';
  CygDrivePrefixKey3 = 'Cygwin';
  CygDrivePrefixKey4 = 'mounts v2';
  CygDrivePrefixKey = 'cygdrive prefix';

function CygDrivePrefix : string;
var
  i : longint;
  length : dword;
  Value : pchar;
  _type : dword;
  Key,NKey : HKey;
begin
  Length:=0;
  Key:=HKEY_CURRENT_USER;
  i := RegOpenKeyEx(Key, CygDrivePrefixKey1, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
  if i=ERROR_SUCCESS then
    begin
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey2, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey3, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegOpenKeyEx(Key, CygDrivePrefixKey4, 0, KEY_ENUMERATE_SUB_KEYS, @NKey);
    end;
  if i=ERROR_SUCCESS then
    begin
      RegCloseKey(Key);
      Key:=NKey;
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, nil, @length);
    end;
  if i<>ERROR_SUCCESS then
    CygDrivePrefix:='/cygdrive'
  else
    Begin
      GetMem(Value,Length);
      i := RegQueryValueEx( Key, CygDrivePrefixKey, nil, @_type, LPByte(Value), @length);
      if i<>ERROR_SUCCESS then
        CygDrivePrefix:='/cygdrive'
      else
        CygDrivePrefix:=StrPas(Value);
      FreeMem(Value,Length);
    End;
  if Key<>HKEY_CURRENT_USER then
    RegCloseKey(Key);
end;
{$endif win32}

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
  inherited init;
end;


destructor TGDBController.Done;
begin
  if assigned(progname) then
    strdispose(progname);
  if assigned(progdir) then
    strdispose(progdir);
  if assigned(progargs) then
    strdispose(progargs);
  inherited done;
end;


procedure TGDBController.Command(const s:string);
begin
  inc(in_command);
  CommandBegin(s);
  gdboutputbuf.reset;
  gdberrorbuf.reset;
  gdb_command(s);
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
  dec(in_command);
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

procedure TGDBController.SetDir(const s : string);
var
  hs : string;
begin
  hs:=s;
  UnixDir(hs);
  if assigned(progdir) then
    strdispose(progdir);
  getmem(progdir,length(hs)+1);
  strpcopy(progdir,hs);
  command('cd '+hs);
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
  start_break_number:=last_breakpoint_number;
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


procedure TGDBController.TraceStepI;
begin
  Command('stepi');
end;


procedure TGDBController.TraceNextI;
begin
  Command('nexti');
end;


procedure TGDBController.Continue;
begin
  Command('continue');
end;


procedure TGDBController.UntilReturn;
begin
  Command('finish');
end;

{ Register functions }

function TGDBController.GetIntRegister(const RegName: string; var Value: UInt64): Boolean;
var
  RegValueStr: string;
  Code: LongInt;
  p, po, p1: PChar;
  buffer: array [0..255] of char;
begin
  GetIntRegister := False;
  Value := 0;
  Command('info registers ' + RegName);
  if Error then
    exit;

  po:=StrNew(GetOutput);
  p:=po;
  if not assigned(p) then
    exit;

  p1:=strscan(p,' ');
  if not assigned(p1) then
  begin
    StrDispose(po);
    exit;
  end;

  p1:=strscan(p,'$');
  { some targets use 0x instead of $ }
  if p1=nil then
    p:=strpos(p,'0x')
  else
    p:=p1;
  p1:=strscan(p,#9);
  if p1=nil then
  begin
    StrDispose(po);
    exit;
  end;
  strlcopy(buffer,p,p1-p);
  RegValueStr:=strpas(buffer);
  StrDispose(po);

  { replace the $? }
  if copy(RegValueStr,1,2)='0x' then
    RegValueStr:='$'+copy(RegValueStr,3,length(RegValueStr)-2);

  Val(RegValueStr, Value, Code);
  if Code <> 0 then
    exit;
  GetIntRegister := True;
end;

function TGDBController.GetIntRegister(const RegName: string; var Value: Int64): Boolean;
var
  U64Value: UInt64;
begin
  GetIntRegister := GetIntRegister(RegName, U64Value);
  Value := Int64(U64Value);
end;

function TGDBController.GetIntRegister(const RegName: string; var Value: UInt32): Boolean;
var
  U64Value: UInt64;
begin
  GetIntRegister := GetIntRegister(RegName, U64Value);
  Value := UInt32(U64Value);
  if (U64Value shr 32) <> 0 then
    GetIntRegister := False;
end;

function TGDBController.GetIntRegister(const RegName: string; var Value: Int32): Boolean;
var
  U32Value: UInt32;
begin
  GetIntRegister := GetIntRegister(RegName, U32Value);
  Value := Int32(U32Value);
end;

{ set command }
function TGDBController.SetCommand(Const SetExpr : string) : boolean;
begin
  SetCommand:=false;
  Command('set '+SetExpr);
  if error then
    exit;
  SetCommand:=true;
end;

{ print }

function TGDBController.InternalGetValue(Const expr : string) : AnsiString;
var
  p,p2,p3 : pchar;
  st : string;
  WindowWidth : longint;
begin
  Command('show width');
  p:=GetOutput;

  p3:=nil;
  if assigned(p) and (p[strlen(p)-1]=#10) then
   begin
     p3:=p+strlen(p)-1;
     p3^:=#0;
   end;
  if assigned(p) then
    p2:=strpos(p,' in a line is ')
  else
    p2:=nil;
  if assigned(p2) then
    p:=p2+length(' in a line is ');
  while p^ in [' ',#9] do
    inc(p);
  p3:=strpos(p,'.');
  if assigned(p3) then
    p3^:=#0;
  WindowWidth:=-1;
  val(strpas(p),WindowWidth);
  if WindowWidth<>-1 then
    Command('set width 0xffffffff');
  Command('p '+expr);
  p:=GetOutput;
  p3:=nil;
  if assigned(p) and (p[strlen(p)-1]=#10) then
   begin
     p3:=p+strlen(p)-1;
     p3^:=#0;
   end;
  if assigned(p) then
    p2:=strpos(p,'=')
  else
    p2:=nil;
  if assigned(p2) then
    p:=p2+1;
  while p^ in [' ',#9] do
    inc(p);
  { get rid of type }
  if p^ = '(' then
    p:=strpos(p,')')+1;
  while p^ in [' ',#9] do
    inc(p);
  if assigned(p) then
    InternalGetValue:=AnsiString(p)
  else
    InternalGetValue:=AnsiString(GetError);
  if assigned(p3) then
    p3^:=#10;
  got_error:=false;
  if WindowWidth<>-1 then
    begin
      str(WindowWidth,st);
      Command('set width '+St);
    end;
end;


function TGDBController.PrintCommand(const expr : string): AnsiString;
begin
  PrintCommand:=InternalGetValue(expr);
end;

const
  PrintFormatName : Array[TPrintFormatType] of string[11] =
  (' /b ', ' /d ', ' /x ', ' /o ', '');

function TGDBController.PrintFormattedCommand(const expr : string; Format : TPrintFormatType): AnsiString;
begin
  PrintFormattedCommand:=InternalGetValue(PrintFormatName[Format]+expr);
end;


function TGDBController.BreakpointInsert(const location: string; BreakpointFlags: TBreakpointFlags): LongInt;
var
  Prefix: string = '';
begin
  if bfTemporary in BreakpointFlags then
    Prefix:=Prefix+'t';
  if bfHardware in BreakpointFlags then
    Prefix:=Prefix+'h';
  Last_breakpoint_number:=0;
  Command(Prefix+'break '+location);
  BreakpointInsert:=Last_breakpoint_number;
end;

function TGDBController.WatchpointInsert(const location: string; WatchpointType: TWatchpointType): LongInt;
begin
  Last_breakpoint_number:=0;
  case WatchpointType of
    wtWrite:
      Command('watch ' + location);
    wtReadWrite:
      Command('awatch ' + location);
    wtRead:
      Command('rwatch ' + location);
  end;
  WatchpointInsert:=Last_breakpoint_number;
end;

function TGDBController.BreakpointDelete(BkptNo: LongInt): Boolean;
var
  BkptNoStr: string;
begin
  Str(BkptNo, BkptNoStr);
  Command('delete ' + BkptNoStr);
  BreakpointDelete := not Error;
end;

function TGDBController.BreakpointEnable(BkptNo: LongInt): Boolean;
var
  BkptNoStr: string;
begin
  Str(BkptNo, BkptNoStr);
  Command('enable ' + BkptNoStr);
  BreakpointEnable := not Error;
end;

function TGDBController.BreakpointDisable(BkptNo: LongInt): Boolean;
var
  BkptNoStr: string;
begin
  Str(BkptNo, BkptNoStr);
  Command('disable ' + BkptNoStr);
  BreakpointDisable := not Error;
end;

function TGDBController.BreakpointCondition(BkptNo: LongInt; const ConditionExpr: string): Boolean;
var
  BkptNoStr: string;
begin
  Str(BkptNo, BkptNoStr);
  Command('condition ' + BkptNoStr + ' ' + ConditionExpr);
  BreakpointCondition := not Error;
end;

function TGDBController.BreakpointSetIgnoreCount(BkptNo: LongInt; const IgnoreCount: LongInt): Boolean;
var
  BkptNoStr, IgnoreCountStr: string;
begin
  Str(BkptNo, BkptNoStr);
  Str(IgnoreCount, IgnoreCountStr);
  Command('ignore ' + BkptNoStr + ' ' + IgnoreCountStr);
  BreakpointSetIgnoreCount := not Error;
end;

procedure TGDBController.SetTBreak(tbreakstring : string);
begin
  Last_breakpoint_number:=0;
  Command('tbreak '+tbreakstring);
  TBreakNumber:=Last_breakpoint_number;
end;

procedure TGDBController.Backtrace;
begin
  { forget all old frames }
  clear_frames;

  Command('backtrace');
end;

function TGDBController.SelectFrameCommand(level :longint) : boolean;
var
  LevelStr : String;
begin
  Str(Level, LevelStr);
  Command('frame '+LevelStr);
  SelectFrameCommand:=not error;
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
    GetError:=pchar(ptrint(gdboutputbuf.buf)+gdboutputbuf.idx)
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
