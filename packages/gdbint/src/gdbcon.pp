{
    Copyright (c) 1998 by Peter Vreman

    Lowlevel GDB interface which communicates directly with libgdb

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit GDBCon;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef USE_GDBLIBINC}
  {$i gdblib.inc}
{$else not USE_GDBLIBINC}
  {$i gdbver.inc}
{$endif not USE_GDBLIBINC}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.Gdbint;
{$ELSE FPC_DOTTEDUNITS}
uses
  GDBInt;
{$ENDIF FPC_DOTTEDUNITS}

type
  TBreakpointFlags = set of (bfTemporary, bfHardware);
  TWatchpointType = (wtWrite, wtReadWrite, wtRead);
  TPrintFormatType = (pfbinary, pfdecimal, pfhexadecimal, pfoctal, pfnatural);

  PGDBController=^TGDBController;
  TGDBController=object(TGDBInterface)
  private
    SavedWindowWidth : longint;
    { width }
    procedure MaxWidth;
    procedure NormWidth;
    { print }
    function InternalGetValue(Const expr : ShortString) : AnsiString;
  public
    progname,
    progdir,
    progargs   : PAnsiChar;
    TBreakNumber,
    start_break_number,
    in_command,
    init_count : longint;
    constructor Init;
    destructor  Done;
    procedure CommandBegin(const s:ShortString);virtual;
    procedure Command(const s:ShortString);
    procedure CommandEnd(const s:ShortString);virtual;
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
    function GetIntRegister(const RegName: ShortString; var Value: UInt64): Boolean;
    function GetIntRegister(const RegName: ShortString; var Value: Int64): Boolean;
    function GetIntRegister(const RegName: ShortString; var Value: UInt32): Boolean;
    function GetIntRegister(const RegName: ShortString; var Value: Int32): Boolean;
    function GetIntRegister(const RegName: ShortString; var Value: UInt16): Boolean;
    function GetIntRegister(const RegName: ShortString; var Value: Int16): Boolean;
    { set command }
    function SetCommand(Const SetExpr : ShortString) : boolean;
    { print }
    function PrintCommand(const expr : ShortString): AnsiString;
    function PrintFormattedCommand(const expr : ShortString; Format : TPrintFormatType): AnsiString;
    { breakpoints }
    function BreakpointInsert(const location: ShortString; BreakpointFlags: TBreakpointFlags): LongInt;
    function WatchpointInsert(const location: ShortString; WatchpointType: TWatchpointType): LongInt;
    function BreakpointDelete(BkptNo: LongInt): Boolean;
    function BreakpointEnable(BkptNo: LongInt): Boolean;
    function BreakpointDisable(BkptNo: LongInt): Boolean;
    function BreakpointCondition(BkptNo: LongInt; const ConditionExpr: ShortString): Boolean;
    function BreakpointSetIgnoreCount(BkptNo: LongInt; const IgnoreCount: LongInt): Boolean;
    procedure SetTBreak(tbreakstring : ShortString);
    { frame commands }
    procedure Backtrace;
    function SelectFrameCommand(level :longint) : boolean;
    { needed for dos because newlines are only #10 (PM) }
    procedure WriteErrorBuf;
    procedure WriteOutputBuf;
    function  GetOutput : PAnsiChar;
    function  GetError : PAnsiChar;
    function  LoadFile(var fn:ShortString):boolean;
    procedure SetDir(const s : ShortString);
    procedure SetArgs(const s : ShortString);
    procedure ClearSymbols;
  end;

procedure UnixDir(var s : ShortString);

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
{$ifdef win32}
  WinApi.Windows,
{$endif win32}
  TP.DOS,
  System.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses
{$ifdef win32}
  windows,
{$endif win32}
  dos,
  strings;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef win32}
const
  CygDrivePrefixKey1 = 'Software';
  CygDrivePrefixKey2 = 'Cygnus Solutions';
  CygDrivePrefixKey3 = 'Cygwin';
  CygDrivePrefixKey4 = 'mounts v2';
  CygDrivePrefixKey = 'cygdrive prefix';

function CygDrivePrefix : ShortString;
var
  i : longint;
  length : dword;
  Value : PAnsiChar;
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

procedure UnixDir(var s : ShortString);
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


procedure TGDBController.Command(const s:ShortString);
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

procedure TGDBController.CommandBegin(const s:ShortString);
begin
end;

procedure TGDBController.CommandEnd(const s:ShortString);
begin
end;

function TGDBController.LoadFile(var fn:ShortString):boolean;
var
  cmd : ShortString;
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

procedure TGDBController.SetDir(const s : ShortString);
var
  hs : ShortString;
begin
  hs:=s;
  UnixDir(hs);
  if assigned(progdir) then
    strdispose(progdir);
  getmem(progdir,length(hs)+1);
  strpcopy(progdir,hs);
  command('cd '+hs);
end;

procedure TGDBController.SetArgs(const s : ShortString);
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

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: UInt64): Boolean;
var
  RegValueStr: ShortString;
  Code: LongInt;
  p, po, p1: PAnsiChar;
  buffer: array [0..255] of AnsiChar;
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

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: Int64): Boolean;
var
  U64Value: UInt64;
begin
  GetIntRegister := GetIntRegister(RegName, U64Value);
  Value := Int64(U64Value);
end;

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: UInt32): Boolean;
var
  U64Value: UInt64;
begin
  GetIntRegister := GetIntRegister(RegName, U64Value);
  Value := UInt32(U64Value);
  if (U64Value shr 32) <> 0 then
    GetIntRegister := False;
end;

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: Int32): Boolean;
var
  U32Value: UInt32;
begin
  GetIntRegister := GetIntRegister(RegName, U32Value);
  Value := Int32(U32Value);
end;

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: UInt16): Boolean;
var
  U64Value: UInt64;
begin
  GetIntRegister := GetIntRegister(RegName, U64Value);
  Value := UInt16(U64Value);
  if (U64Value shr 16) <> 0 then
    GetIntRegister := False;
end;

function TGDBController.GetIntRegister(const RegName: ShortString; var Value: Int16): Boolean;
var
  U16Value: UInt16;
begin
  GetIntRegister := GetIntRegister(RegName, U16Value);
  Value := Int16(U16Value);
end;

{ set command }
function TGDBController.SetCommand(Const SetExpr : ShortString) : boolean;
begin
  SetCommand:=false;
  Command('set '+SetExpr);
  if error then
    exit;
  SetCommand:=true;
end;

{ width }

procedure TGDBController.MaxWidth;
var
  p,p2,p3 : PAnsiChar;
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
  SavedWindowWidth:=-1;
  val(strpas(p),SavedWindowWidth);
  if SavedWindowWidth<>-1 then
    Command('set width 0');
end;

procedure TGDBController.NormWidth;
var
  st : ShortString;
  saved_got_error : boolean;
begin
  saved_got_error:=got_error;
  if SavedWindowWidth<>-1 then
    begin
      str(SavedWindowWidth,st);
      Command('set width '+St);
    end;
  got_error:=saved_got_error;
end;

{ print }

function TrimEnd(s: AnsiString): AnsiString;
var
  I: LongInt;
begin
  if (s<>'') and (s[Length(s)]=#10) then
  begin
    I:=Length(s);
    while (i>1) and ((s[i-1]=' ') or (s[i-1]=#9)) do
      dec(i);
	delete(s,i,Length(s)-i+1);
  end;
  TrimEnd:=s;
end;

function TGDBController.InternalGetValue(Const expr : ShortString) : AnsiString;
var
  p,p2 : PAnsiChar;
begin
  MaxWidth;

  Command('p '+expr);
  p:=GetOutput;
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
  if assigned(p) and not got_error then
    InternalGetValue:=TrimEnd(AnsiString(p))
  else
    InternalGetValue:=TrimEnd(AnsiString(GetError));

  NormWidth;
end;


function TGDBController.PrintCommand(const expr : ShortString): AnsiString;
begin
  PrintCommand:=InternalGetValue(expr);
end;

const
  PrintFormatName : Array[TPrintFormatType] of String[11] =
  (' /b ', ' /d ', ' /x ', ' /o ', '');

function TGDBController.PrintFormattedCommand(const expr : ShortString; Format : TPrintFormatType): AnsiString;
begin
  PrintFormattedCommand:=InternalGetValue(PrintFormatName[Format]+expr);
end;


function TGDBController.BreakpointInsert(const location: ShortString; BreakpointFlags: TBreakpointFlags): LongInt;
var
  Prefix: ShortString = '';
begin
  if bfTemporary in BreakpointFlags then
    Prefix:=Prefix+'t';
  if bfHardware in BreakpointFlags then
    Prefix:=Prefix+'h';
  Last_breakpoint_number:=0;
  Command(Prefix+'break '+location);
  BreakpointInsert:=Last_breakpoint_number;
end;

function TGDBController.WatchpointInsert(const location: ShortString; WatchpointType: TWatchpointType): LongInt;
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
  BkptNoStr: ShortString;
begin
  Str(BkptNo, BkptNoStr);
  Command('delete ' + BkptNoStr);
  BreakpointDelete := not Error;
end;

function TGDBController.BreakpointEnable(BkptNo: LongInt): Boolean;
var
  BkptNoStr: ShortString;
begin
  Str(BkptNo, BkptNoStr);
  Command('enable ' + BkptNoStr);
  BreakpointEnable := not Error;
end;

function TGDBController.BreakpointDisable(BkptNo: LongInt): Boolean;
var
  BkptNoStr: ShortString;
begin
  Str(BkptNo, BkptNoStr);
  Command('disable ' + BkptNoStr);
  BreakpointDisable := not Error;
end;

function TGDBController.BreakpointCondition(BkptNo: LongInt; const ConditionExpr: ShortString): Boolean;
var
  BkptNoStr: ShortString;
begin
  Str(BkptNo, BkptNoStr);
  Command('condition ' + BkptNoStr + ' ' + ConditionExpr);
  BreakpointCondition := not Error;
end;

function TGDBController.BreakpointSetIgnoreCount(BkptNo: LongInt; const IgnoreCount: LongInt): Boolean;
var
  BkptNoStr, IgnoreCountStr: ShortString;
begin
  Str(BkptNo, BkptNoStr);
  Str(IgnoreCount, IgnoreCountStr);
  Command('ignore ' + BkptNoStr + ' ' + IgnoreCountStr);
  BreakpointSetIgnoreCount := not Error;
end;

procedure TGDBController.SetTBreak(tbreakstring : ShortString);
begin
  Last_breakpoint_number:=0;
  Command('tbreak '+tbreakstring);
  TBreakNumber:=Last_breakpoint_number;
end;

procedure TGDBController.Backtrace;
begin
  { forget all old frames }
  clear_frames;

  MaxWidth;
  Command('backtrace');
  NormWidth;
end;

function TGDBController.SelectFrameCommand(level :longint) : boolean;
var
  LevelStr : ShortString;
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


procedure BufWrite(Buf : PAnsiChar);
  var p,pe : PAnsiChar;
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


function  TGDBController.GetOutput : PAnsiChar;
begin
  GetOutput:=gdboutputbuf.buf;
end;

function  TGDBController.GetError : PAnsiChar;
var p : PAnsiChar;
begin
  p:=gdberrorbuf.buf;
  if (p^=#0) and got_error then
    GetError:=PAnsiChar(ptrint(gdboutputbuf.buf)+gdboutputbuf.idx)
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
