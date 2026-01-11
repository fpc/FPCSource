{
   This unit is part of the Free Vision package
   Copyright (c) 1999 by Pierre Muller

   General OS clipboard support unit.

   Connection with Windows Clipboard based on Ralph Brown Interrupt List

   Copyright (c) 2024 by Margers
   Bracketed paste and OSC 52 clipboard support.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.

 ****************************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
{$ifdef FV_UNICODE}
unit ufvclip;
{$else FV_UNICODE}
unit fvclip;
{$endif FV_UNICODE}
{$ENDIF}
{$i platform.inc}
{$h-}

interface

{$undef WinClipSupported}
{$undef DOS}
{ ----------- define DOS for DOS targets ---------- }
{$ifdef GO32V2}{$define DOS}{$endif}

function WinClipboardSupported : boolean;
function OpenWinClipboard : boolean;
function EmptyWinClipboard : boolean;
function CloseWinClipboard : boolean;
function GetTextWinClipboardSize : longint;
function GetTextWinClipboardData(var p : PAnsiChar;var l : longint) : boolean;
function SetTextWinClipboardData(p : PAnsiChar;l : longint) : boolean;

{Should be called after InitKeyboard }
procedure InitClip;
procedure DoneClip;

implementation

{$IFDEF FPC_DOTTEDUNITS}
{$ifdef DOS}
  uses
    FreeVision.Pmode,
{$ifdef go32v2}
    {go32   sorry Gabor, but its still not compiling without that ! }
    {now it works. btw. you don't have to sorry - just to tell me... ;)) Gabor }
{$endif go32v2}
    TP.DOS;
{$endif DOS}

{$ifdef unix}
  uses
    UnixApi.Base,UnixApi.TermIO,System.Console.Keyboard,FreeVision.Sysmsg;
{$endif}

{$ifdef Windows}
  uses
    System.Strings,WinApi.Windows;
{$endif Windows}

{$ifdef HASAMIGA}
  uses
    {$ifdef AMIGA68K} Amiga.Core.Clipboard,{$endif}
    {$ifdef AMIGAOS4} AmigaApi.Clipboard,{$endif}
    {$ifdef AROS} AROSApi.Clipboard,{$endif}
    {$ifdef MorphOS} MorphApi.Clipboard,{$endif}
    AmigaApi.Cliputils;
{$endif}

{$ifdef os2}
  uses
    OS2Api.DosCalls, OS2Api.OS2Def;
{$endif os2}

{$ELSE  not FPC_DOTTEDUNITS}
{$ifdef DOS}
  uses
    pmode,
{$ifdef go32v2}
    {go32   sorry Gabor, but its still not compiling without that ! }
    {now it works. btw. you don't have to sorry - just to tell me... ;)) Gabor }
{$endif go32v2}
    dos;
{$endif DOS}

{$ifdef unix}
  uses
    baseUnix,termio,keyboard,sysmsg;
{$endif}

{$ifdef Windows}
  uses
    strings,windows;
{$endif Windows}

{$ifdef HASAMIGA}
  uses
    clipboard,cliputils;
{$endif}

{$ifdef os2}
  uses
    DosCalls, OS2Def;
{$endif os2}

{$ENDIF  FPC_DOTTEDUNITS}


{$ifdef UNIX}
var PText : PAnsiChar;

{Could not use unit base64 because of Sysutils and reasons }
{Speed or reusability here is not a concern                }
const
  EncodingTable: PAnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
Alphabet = ['a'..'z','A'..'Z','0'..'9','+','/','=']; // all 65 chars that are in the base64 encoding alphabet

{ Memory has to be preallocated;  source p, destination d }
procedure encodeBase64(p:PAnsiChar;len:longint; d:PAnsiChar; var outlen: longint);
var
  i, rem : longint;
  Buf : array [0..3] of byte;
  WriteBuf: array [0..3] of AnsiChar;
begin
  rem:=len mod 3;
  outlen:=len div 3;
  for i:=1 to outlen do
  begin
    plongword(@buf[0])^:=plongword(p)^;
    WriteBuf[0] := EncodingTable[Buf[0] shr 2];
    WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
    WriteBuf[2] := EncodingTable[(Buf[1] and 15) shl 2 or (Buf[2] shr 6)];
    WriteBuf[3] := EncodingTable[Buf[2] and 63];
    plongword(d)^:=plongword(@WriteBuf[0])^;
    inc(p,3);
    inc(d,4);
  end;
  outlen:=outlen*4;
  case rem of
    1: begin
         Buf[0]:=byte(p^);
         WriteBuf[0] := EncodingTable[Buf[0] shr 2];
         WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4];
         WriteBuf[2] := '=';
         WriteBuf[3] := '=';
         plongword(d)^:=plongword(@WriteBuf[0])^;
         inc(outlen,4);
       end;
    2: begin
         Buf[0]:=byte(p^);inc(p);
         Buf[1]:=byte(p^);
         WriteBuf[0] := EncodingTable[Buf[0] shr 2];
         WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
         WriteBuf[2] := EncodingTable[(Buf[1] and 15) shl 2];
         WriteBuf[3] := '=';
         plongword(d)^:=plongword(@WriteBuf[0])^;
         inc(outlen,4);
       end;
  end;
end;

{ Memory has to be preallocated;  source p, destination d }
procedure decodeBase64(p:PAnsiChar; len: longint; d:PAnsiChar; var outlen: longint);
var
  i,rlen : longint;
  v,b,po : byte;
  ch: AnsiChar;
begin
     outlen:=0;
     rlen:=0;
     v:=0;
     po:=0;
     b:=0;
     for i:= 1 to len do
     begin
       ch:=p^;
       inc(p);
       if not (ch in Alphabet) then exit;
       if ch = '=' then
       begin
         case po of
           0:begin exit; end;
           1:begin exit; end;
           2:begin break; end;
           3:begin break; end;
         end;
       end;
       v:=0;
       if (ch>='A') and (ch<='Z') then // 0..25
         v:=Ord(ch)-Ord('A')
       else if (ch>='a') and (ch<='z') then // 26..51
         v:=Ord(ch)-Ord('a')+26
       else if (ch>='0') and (ch<='9') then // 52..61
         v:=Ord(ch)-Ord('0')+52
       else if ch='+' then // 62
         v:=62
       else if ch='/' then // 63
         v:=63;
       case po of
         0:begin b:=v shl 2; end;
         1:begin b:=b or (v shr 4); d^:=char(b); inc(d); b:=v shl 4; inc(rlen); end;
         2:begin b:=b or (v shr 2); d^:=char(b); inc(d); b:=v shl 6; inc(rlen); end;
         3:begin b:=b or v;         d^:=char(b); inc(d);{b:=0;}      inc(rlen); end;
       end;
       inc(po);
       po:=po and $3;
     end;
     outlen:=rlen; {length for output}
end;


procedure PutInEventQue (var zt: AnsiString;l:Longint);
var SysEvent:TSystemEvent;
    PrevPText : PAnsiChar;
    NewPText : PAnsiChar;
begin
  GetMem(NewPText,l+1);
  Move(zt[1],NewPText^,l+1);
  { Create paste event }
  SysEvent.Typ:=SysPaste;
  SysEvent.P:=PAnsiChar(NewPText);
  SysEvent.Len:=l;
  PutSystemEvent(SysEvent);
  { Discard previous pasted data }
  { This is not Thread safe, but expectation is that FV is running in single thread }
  PrevPText:=PText;
  PText:=NewPText;
  if Assigned(PrevPText) then
    FreeMem(PrevPText);
end;


procedure OSC_52_ClipboardData;
var zt,rt  : AnsiString;
  escSeq : ShortString;
  inEsc,inRead : boolean;
  k : Longint;
  ch : AnsiChar;
  timewait,finalparsec : TimeSpec;
  ree:longint;
  countemptines : Longint;
  rlen : longint;
begin
  countemptines:=0;
  zt:='';
  escSeq:='';
  inEsc:=false;
  inRead:=true;
  {-- read and parse --}
  while inRead do
  begin
    if keypressed then
      ch:=RawReadKey
    else
    begin
      inc(countemptines);
      if countemptines = 30 then break;
      {we might be ahead for a short while, wait a little bit longer}
      timewait.tv_sec := 0;
      timewait.tv_nsec := 2000000;
      ree:=fpNanoSleep(@timewait,@finalparsec);
      continue;
    end;
    countemptines:=0;
    if ch = ';' then
      continue; {eat very first ';', there should not be any other}
    if inEsc then
      begin
        escSeq:=escSeq+ch;
        if ch = '\' then
        begin
          {data end, kitty terminal}
          inRead:=false;
          break;
        end;
        {we should not reach this point}
        {unless OSC 52 is spliced by other escape sequence}
        break;
      end;
    if ch = #27 then
      inEsc:=true
    else if ch <> #7 then
      zt:=zt+ch  {base64 encoded data in}
    else
      begin
        {data end, xterm terminal}
        inRead:=false;
        break;
      end;
  end; {while do }
  if length(zt)=0 then exit;
  {-- data decode --}
  SetLength(rt,(length(zt) div 4)*3);
  decodeBase64(@zt[1],length(zt),@rt[1],rlen);
  SetLength(rt,rlen);
  if length(rt)=0 then exit; {probably error}
  PutInEventQue(rt,rlen);
end;


procedure BracketedPaste;
var zt  : AnsiString;
  k : Longint;
  ch : AnsiChar;
  timewait,finalparsec : TimeSpec;
  ree:longint;
  countemptines : Longint;
  len : Longint;
begin
  countemptines:=0;
  zt:='';
  {-- read and parse --}
  while true do
  begin
    if keypressed then
      ch:=RawReadKey
    else
    begin
      inc(countemptines);
      if countemptines = 30 then break;
      {we might be ahead for a short while, wait a little bit longer}
      timewait.tv_sec := 0;
      timewait.tv_nsec := 2000000;
      ree:=fpNanoSleep(@timewait,@finalparsec);
      continue;
    end;
    countemptines:=0;
    zt:=zt+ch;  {data in}
    if ch = '~' then
      begin
        {test for terminator string}
        if copy(zt,length(zt)-5,6)=#27'[201~' then
          break;
      end;
  end; {while do }
  len:=length(zt);
  if len=0 then exit;
  if len>6 then
  begin
    len:=length(zt)-6;
    SetLength(zt,len); {get rid of bracketed paste end sequence}
  end;
  PutInEventQue(zt,len);
end;
{$endif}

procedure InitClip;
begin
{$ifdef unix}
  if Assigned(PText) then
    FreeMem(PText);
  PText:=nil;
  AddSpecialSequence(#27'[200~',@BracketedPaste);
  AddSpecialSequence(#27']52;c',@OSC_52_ClipboardData);
  write(#27'[?2004h');
{$endif}
end;

procedure DoneClip;
begin
{$ifdef unix}
  write(#27'[?2004l');
  if Assigned(PText) then
    FreeMem(PText);
  PText:=nil;
{$endif}
end;

{function GetGlobalClipboardData(var P: PAnsiChar;var ASize: longint): boolean;}
procedure GetGlobalClipboardData;
begin
{$ifdef unix}
  write(#27']52;c;?'#7); { OSC 52  Get Clipboard Content }
{$endif}
end;

function SetGlobalClipboardData(P: PAnsiChar; ASize: longint): boolean;
var S : AnsiString;
  rem,len,rlen: longint;
begin
  SetGlobalClipboardData:=true;
{$ifdef unix}
  len:=ASize;
  rem:=len mod 3;
  rem:=((rem+1) shr 1)*4; {remainder 4 bytes (or not if rem = 0)}
  SetLength(S,(len div 3)*4+rem);
  encodeBase64 (P,len,@S[1],rlen);
  if rlen<>length(S) then exit; {preallocated length have to match with returned length}
  write(#27']52;c;'); { OSC 52  Set Clipboard Content }
  write(S); { base64 encoded data }
  write(#7); { String Terminator }
{$endif}
end;


{$ifdef DOS}
{$define WinClipSupported}
function WinClipboardSupported : boolean;
var
  r : registers;
begin
  r.ax:=$1700;
  RealIntr($2F,r);
  WinClipboardSupported:=(r.ax<>$1700);
end;

function OpenWinClipboard : boolean;
var
  r : Registers;
begin
  r.ax:=$1701;
  RealIntr($2F,r);
  OpenWinClipboard:=(r.ax<>0);
end;

function EmptyWinClipboard : boolean;
var
  r : Registers;
begin
  r.ax:=$1702;
  RealIntr($2F,r);
  EmptyWinClipboard:=(r.ax<>0);
end;

function CloseWinClipboard : boolean;
var
  r : Registers;
begin
  r.ax:=$1708;
  RealIntr($2F,r);
  CloseWinClipboard:=(r.ax<>0);
end;

function InternGetDataSize : longint;
var
  r : Registers;
begin
  r.ax:=$1704;
  r.dx:=7 {OEM Text rather then 1 : Text };
  RealIntr($2F,r);
  InternGetDataSize:=(r.dx shl 16) + r.ax;
end;
{$endif DOS}

{$ifdef UNIX}
{$define WinClipSupported}
function WinClipboardSupported : boolean;
var term, typ : string;
    thistty : shortstring;
begin
  WinClipboardSupported:=false;
{$if not defined(LINUX) and not defined(BSD)}
  Exit; { Report as not supported when in realty it has not been tested. }
{$endif}
{$ifndef LINUX}
  thistty:=ttyname(stdinputhandle);
  if (copy(thistty,1,8)<>'/dev/tty') then
    WinClipboardSupported:=true; { probably we are good }
{$endif}
{$ifdef LINUX}
  typ:=fpgetenv('XDG_SESSION_TYPE');
  if length(typ)>0 then
    if lowercase(typ)='tty' then
      Exit; { in console mode OSC 52 is not supported }
  term:=fpgetenv('TERM');
  if length(term)>0 then
    if lowercase(term)<>'linux' then
      WinClipboardSupported:=true; { probably we are good }
{$endif}
end;

function OpenWinClipboard : boolean;
begin
  OpenWinClipboard:=true;
end;

function EmptyWinClipboard : boolean;
begin
  EmptyWinClipboard:=true;
end;

function CloseWinClipboard : boolean;
begin
  CloseWinClipboard:=true;
end;

function GetTextWinClipboardSize : longint;
begin
  GetTextWinClipboardSize:=1;  {there has to be something in order for menu to be active}
end;

function GetTextWinClipBoardData(var P : PAnsiChar;var L : longint) : boolean;
begin
  GetTextWinClipBoardData:=true;
  P:=nil;L:=0;              { Have no immediate response }
  GetGlobalClipboardData;   { Request now, get clipboard data later }
end;

function SetTextWinClipBoardData(P : PAnsiChar; L : longint) : boolean;
begin
  SetTextWinClipBoardData:= SetGlobalClipboardData(P,L);
end;
{$endif}

{$ifdef Windows}
{$define WinClipSupported}
function WinClipboardSupported : boolean;
begin
  WinClipboardSupported:=true;
end;

function OpenWinClipboard : boolean;
begin
  OpenWinClipboard:=OpenClipboard(0);
end;

function EmptyWinClipboard : boolean;
begin
  EmptyWinClipboard:=EmptyClipboard;
end;

function CloseWinClipboard : boolean;
begin
  CloseWinClipboard:=CloseClipboard;
end;

function InternGetDataSize : longint;
var HC : Handle;
begin
  HC:=GetClipBoardData(CF_OEMTEXT);
  if HC<>0 then
    begin
      InternGetDataSize:=strlen(PAnsiChar(GlobalLock(HC)))+1;
      GlobalUnlock(HC);
    end
  else
    InternGetDataSize:=0;
end;
{$endif Windows}

{$ifdef HASAMIGA}
{$define WinClipSupported}
function WinClipboardSupported: Boolean;
begin
  WinClipboardSupported := True;
end;

function OpenWinClipboard: boolean;
begin
  OpenWinClipboard := True;
end;

function EmptyWinClipboard: boolean;
begin
  EmptyWinClipboard := GetTextFromClip(PRIMARY_CLIP) = '';
end;

function CloseWinClipboard : boolean;
begin
  CloseWinClipboard:= True;
end;

function InternGetDataSize: LongInt;
var
  Text: string;
begin
  Text := GetTextFromClip(PRIMARY_CLIP);
  InternGetDataSize := Length(Text);
end;
{$endif HASAMIGA}

{$ifdef os2}
{$define WinClipSupported}
const
  CF_TEXT = 1;
  CF_BITMAP = 2;
  CF_DSPTEXT = 3;
  CF_DSPBITMAP = 4;
  CF_METAFILE = 5;
  CF_DSPMETAFILE = 6;
  CF_PALETTE = 9;

  CFI_OWNERFREE = $0001;
  CFI_OWNERDISPLAY = $0002;
  CFI_POINTER = $0400;
  CFI_HANDLE = $0200;

var
  OS2ClipboardSupported: boolean = false;
  PMWHandle: cardinal;
  MsgQueueHandle: cardinal;
  PIB: PProcessInfoBlock;

type
(*  TWinSetClipbrdOwner = function (hab, hwnd: cardinal): longbool; cdecl;*)
  TWinSetClipbrdData = function (hab, ulData, fmt, rgfFmtInfo: cardinal): longbool;  cdecl;
  TWinQueryClipbrdData = function (hab, fmt: cardinal): cardinal; cdecl;
  TWinQueryClipbrdFmtInfo = function (hab, fmt: cardinal; var prgfFmtInfo: cardinal): longbool; cdecl;
{    function WinSetClipbrdViewer(hab,hwndNewClipViewer : cardinal) : longbool;  cdecl;}
{    function WinEnumClipbrdFmts(hab,fmt : cardinal) : cardinal; cdecl;}
  TWinEmptyClipbrd = function (hab: cardinal): longbool; cdecl;
  TWinOpenClipbrd = function (hab: cardinal): longbool; cdecl;
  TWinCloseClipbrd = function (hab: cardinal): longbool; cdecl;
(*  TWinQueryClipbrdOwner = function (hab: cardinal): cardinal; cdecl;*)
{    function WinQueryClipbrdViewer(hab : cardinal) : cardinal; cdecl;}
  TWinInitialize = function (flOptions: cardinal): cardinal; cdecl;
  TWinTerminate = function (hab: cardinal): longbool; cdecl;
  TWinCreateMsgQueue = function (hab: cardinal; cmsg: longint): cardinal; cdecl;
  TWinDestroyMsgQueue = function (hmq: cardinal): longbool; cdecl;

var
(*  WinSetClipbrdOwner: TWinSetClipbrdOwner;*)
  ClWinSetClipbrdData: TWinSetClipbrdData;
  ClWinQueryClipbrdData: TWinQueryClipbrdData;
  ClWinQueryClipbrdFmtInfo: TWinQueryClipbrdFmtInfo;
{    function WinSetClipbrdViewer(hab,hwndNewClipViewer : cardinal) : longbool;  cdecl;}
{    function WinEnumClipbrdFmts(hab,fmt : cardinal) : cardinal; cdecl;}
  ClWinEmptyClipbrd: TWinEmptyClipbrd;
  ClWinOpenClipbrd: TWinOpenClipbrd;
  ClWinCloseClipbrd: TWinCloseClipbrd;
(*  WinQueryClipbrdOwner: TWinQueryClipbrdOwner;*)
{    function WinQueryClipbrdViewer(hab : cardinal) : cardinal; cdecl;}
  ClWinInitialize: TWinInitialize;
  ClWinTerminate: TWinTerminate;
  ClWinCreateMsgQueue: TWinCreateMsgQueue;
  ClWinDestroyMsgQueue: TWinDestroyMsgQueue;

  OrigSessType: cardinal;


function WinClipboardSupported : boolean;
begin
  WinClipboardSupported:=OS2ClipboardSupported;
end;

function OpenWinClipboard : boolean;
var
  SessType: cardinal;
begin
  OpenWinClipboard := false;
  if not (OS2ClipboardSupported) then
   Exit;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  OpenWinClipboard := ClWinOpenClipbrd (PMWHandle);
  PIB^.tType := SessType;
end;

function EmptyWinClipboard : boolean;
var
  SessType: cardinal;
begin
  EmptyWinClipboard := false;
  if not (OS2ClipboardSupported) then
   Exit;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  EmptyWinClipboard := ClWinEmptyClipbrd (PMWHandle);
  PIB^.tType := SessType;
end;

function CloseWinClipboard : boolean;
var
  SessType: cardinal;
begin
  CloseWinClipboard := false;
  if not (OS2ClipboardSupported) then
   Exit;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  CloseWinClipboard := ClWinCloseClipbrd (PMWHandle);
  PIB^.tType := SessType;
end;

function InternGetDataSize : longint;
var
  P: PAnsiChar;
  SessType: cardinal;
begin
  InternGetDataSize := 0;
  if not (OS2ClipboardSupported) then
   Exit;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  P := PAnsiChar (ClWinQueryClipbrdData (PMWHandle, CF_TEXT));
  PIB^.tType := SessType;

  if P <> nil then
   InternGetDataSize := StrLen (PAnsiChar (P)) + 1;
end;

procedure InitClipboard;
var
  RC: cardinal;
  ProcOK: boolean;
  TIB: PThreadInfoBlock;
  PMWModHandle: THandle;
  Err: string;
  ErrL: cardinal;
begin
  if OS2ClipboardSupported then
   Exit;
  DosGetInfoBlocks (TIB, PIB);
  OrigSessType := PIB^.tType;
  PIB^.tType := 3;

{  RC := DosQueryModuleHandle ('PMWIN', PMWModHandle);}
  RC := DosLoadModule (Err, ErrL, 'PMWIN', PMWModHandle);
  if RC <> 0 then
   begin
    PIB^.tType := OrigSessType;
    Exit;
   end;

  ProcOK := (DosQueryProcAddr (PMWModHandle, 707, nil, pointer (ClWinCloseClipbrd)) = 0)
     and
   (DosQueryProcAddr (PMWModHandle, 716, nil, pointer (ClWinCreateMsgQueue)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 726, nil, pointer (ClWinDestroyMsgQueue)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 733, nil, pointer (ClWinEmptyClipbrd)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 763, nil, pointer (ClWinInitialize)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 793, nil, pointer (ClWinOpenClipbrd)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 806, nil, pointer (ClWinQueryClipbrdData)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 807, nil, pointer (ClWinQueryClipbrdFmtInfo)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 854, nil, pointer (ClWinSetClipbrdData)) = 0) and
   (DosQueryProcAddr (PMWModHandle, 888, nil, pointer (ClWinTerminate)) = 0);

  if ProcOK then
   begin
    PMWHandle := ClWinInitialize (0);
    if PMWHandle <> 0 then
     begin
      MsgQueueHandle := ClWinCreateMsgQueue (PMWHandle, 0);
      ProcOK := MsgQueueHandle <> 0;
     end
    else
     ProcOK := false;
   end;

  PIB^.tType := OrigSessType;

  if ProcOK then
   OS2ClipboardSupported := true;
end;

procedure DoneClipboard;
var
  SessType: cardinal;
begin
  if not (OS2ClipboardSupported) then
   Exit;
  OS2ClipboardSupported := false;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  if MsgQueueHandle <> 0 then
   begin
    ClWinDestroyMsgQueue (MsgQueueHandle);
    MsgQueueHandle := 0;
   end;
  if PMWHandle <> 0 then
   begin
    ClWinTerminate (PMWHandle);
    PMWHandle := 0;
   end;
  PIB^.tType := SessType;
end;
{$endif os2}

{$ifndef UNIX} { Unix have defined all functions, lets go for every other system }
{$ifdef WinClipSupported}
function GetTextWinClipboardSize : longint;
begin
  OpenWinClipboard;
  GetTextWinClipboardSize:=InternGetDataSize;
  CloseWinClipboard;
end;

function GetTextWinClipBoardData(var p : PAnsiChar;var l : longint) : boolean;
var
{$ifdef DOS}
  r : Registers;
  M : MemPtr;
  pp: PAnsiChar;
{$endif DOS}
{$ifdef Windows}
  h : HGlobal;
  pp : PAnsiChar;
{$endif Windows}
{$ifdef HASAMIGA}
  Text: AnsiString;
  pp: PAnsiChar;
{$endif HASAMIGA}
{$IFDEF OS2}
  PP: PAnsiChar;
  SessType: cardinal;
{$ENDIF OS2}
begin
  p:=nil;
  GetTextWinClipBoardData:=False;
  if not OpenWinClipBoard then
    exit;
{$ifdef DOS}
  l:=InternGetDataSize;
  if (l=0) or (l>65520) then
    begin
      l:=0;
      CloseWinClipBoard;
      exit;
    end;
  GetMem(p,l+1);
  GetDosMem(M,l);
  r.ax:=$1705;
  r.dx:=7{ OEM Text rather then 1 : Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  RealIntr($2F,r);
  GetTextWinClipBoardData:=(r.ax<>0);
{$endif DOS}
{$ifdef Windows}
  h:=GetClipboardData(CF_OEMTEXT);
  if h<>0 then
    begin
      pp:=PAnsiChar(GlobalLock(h));
      l:=strlen(pp)+1;
      getmem(p,l);
      move(pp^,p^,l);
      GlobalUnlock(h);
    end;
  GetTextWinClipBoardData:=h<>0;
{$endif Windows}
{$ifdef HASAMIGA}
  Text := GetTextFromClip(0) + #0;
  PP := @Text[1];
  l := Length(Text);
  GetMem(p,l);
  Move(pp^,p^,l);
  GetTextWinClipBoardData := True;
{$endif HASAMIGA}
{$IFDEF OS2}
  GetTextWinClipboardData := false;
  L := 0;
  if not (OS2ClipboardSupported) then
   Exit;
  SessType := PIB^.tType;
  PIB^.tType := 3;
  PP := PAnsiChar (ClWinQueryClipbrdData (PMWHandle, CF_TEXT));
  PIB^.tType := SessType;

  if PP <> nil then
   begin
    L := StrLen (PAnsiChar (PP)) + 1;
    GetMem (P, L);
    if P <> nil then
     begin
      Move (PP^, P^, L);
      GetTextWinClipBoardData := true;
     end;
   end;
{$ENDIF OS2}
  CloseWinClipBoard;
{$ifdef DOS}
  M.MoveDataFrom(l,P^);
  FreeDosMem(M);
  pp:=p+l;
  pp^:=#0; { make null terminated }
{$endif DOS}
end;

function SetTextWinClipBoardData(p : PAnsiChar;l : longint) : boolean;
var
{$ifdef DOS}
  r : Registers;
  M : MemPtr;
  pp: PAnsiChar;
  op: PAnsiChar;
{$endif DOS}
{$ifdef Windows}
  h : HGlobal;
  pp : PAnsiChar;
  res : boolean;
{$endif Windows}
{$ifdef HASAMIGA}
  pp: PAnsiChar;
  Test: AnsiString;
{$endif HASAMIGA}
{$IFDEF OS2}
  RC: cardinal;
  PShared: pointer;
  SessType: cardinal;
{$ENDIF OS2}
begin
  SetTextWinClipBoardData:=False;
  if (l=0) or (l>65520) then
    exit;
  if not OpenWinClipBoard then
    exit;
  EmptyWinClipBoard;
{$ifdef DOS}
  GetMem(pp,l+1);
  Move(p^,pp^,l);
  op:=pp+l;
  op^:=#0; { make sure that string is null terminated }
  GetDosMem(M,l+1);
  M.MoveDataTo(PP^,l+1);
  FreeMem(pp);
  r.ax:=$1703;
  r.dx:=7{ OEM Text rather then 1 : Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  r.si:=l shr 16;
  r.cx:=l and $ffff;
  RealIntr($2F,r);
  SetTextWinClipBoardData:=(r.ax<>0);
  (*
  r.ax:=$1703;
  r.dx:=1{ Empty  Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  r.si:=0;
  r.cx:=0;
  RealIntr($2F,r);
  *)
  FreeDosMem(M);
{$endif DOS}
{$ifdef Windows}
  h:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,l+1);
  pp:=PAnsiChar(GlobalLock(h));
  move(p^,pp^,l+1);
  GlobalUnlock(h);
  res:=(SetClipboardData(CF_OEMTEXT,h)=h);
  h:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,l+1);
  pp:=PAnsiChar(GlobalLock(h));
  OemToCharBuffA(p,pp,l+1);
  SetClipboardData(CF_TEXT,h);
  GlobalUnlock(h);
  SetTextWinClipBoardData:=res;
{$endif Windows}
{$ifdef HASAMIGA}
  PutTextToClip(0, AnsiString(p));
{$endif HASAMIGA}
{$IFDEF OS2}
  SetTextWinClipboardData := false;
  if not (OS2ClipboardSupported) then
   Exit;
  RC := DosAllocSharedMem (PShared, nil, Succ (L),
                                      PAG_WRITE or PAG_COMMIT or OBJ_GIVEABLE);
  if RC = 0 then
   begin
    Move (P^, PShared^, Succ (L));

    SessType := PIB^.tType;
    PIB^.tType := 3;
    SetTextWinClipboardData := ClWinSetClipbrdData (PMWHandle,
                                     cardinal (PShared), CF_TEXT, CFI_POINTER);
    PIB^.tType := SessType;
   end;
{$ENDIF OS2}
  CloseWinClipBoard;
end;

{$else WinClipSupported}
{ Implementation for not supported OS clipboard. }

function WinClipboardSupported : boolean;
begin
  WinClipboardSupported:=false;
end;

function OpenWinClipboard : boolean;
begin
  OpenWinClipboard:=false;
end;

function EmptyWinClipboard : boolean;
begin
  EmptyWinClipboard:=false;
end;

function CloseWinClipboard : boolean;
begin
  CloseWinClipboard:=false;
end;

function GetTextWinClipboardSize : longint;
begin
  GetTextWinClipboardSize:=0;
end;

function GetTextWinClipBoardData(var P : PAnsiChar;var L : longint) : boolean;
begin
  GetTextWinClipBoardData:=false;
end;

function SetTextWinClipBoardData(P : PAnsiChar; L : longint) : boolean;
begin
  SetTextWinClipBoardData:=false;
end;
{$endif}
{$endif UNIX}

{$ifdef os2}
initialization
 InitClipboard;

finalization
 DoneClipboard;
{$endif os2}
end.
