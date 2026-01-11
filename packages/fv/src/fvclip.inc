{
   This unit is part of the Free Vision package

   Copyright 2024 by Margers

   Bracketed paste and OSC 52 clipboard support (Unix only).

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

{$ifdef LINUX}
  {$define WinClipSupported}
{$endif}
{$ifdef BSD}
  {$define WinClipSupported}
{$endif}


function WinClipboardSupported : boolean;
function OpenWinClipboard : boolean;
function EmptyWinClipboard : boolean;
function CloseWinClipboard : boolean;
function GetTextWinClipboardSize : longint;
function GetTextWinClipBoardData(var p : PAnsiChar;var l : longint) : boolean;
function SetTextWinClipBoardData(p : PAnsiChar;l : longint) : boolean;

{Should be called after InitKeyboard }
procedure InitClip;
procedure DoneClip;

{Request clipboard content}
{Actual clipboard content will be returned via event system, if terminal supports OSC 52}
procedure GetGlobalClipboardData;

{ Set clipboard content, if terminal supports OSC 52. Return true always }
function SetGlobalClipboardData(P: PAnsiChar; ASize: longint): boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
{$ifdef unix}
  UnixApi.Base,UnixApi.TermIO,System.Console.Keyboard, FreeVision.Sysmsg
{$endif}
  ,FreeVision.Fvconsts;
{$ELSE}
uses
{$ifdef unix}
  baseUnix,termio,keyboard,sysmsg
{$endif}
  ,fvconsts;
{$ENDIF}


{$ifdef UNIX}
function WinClipboardSupported : boolean;
var term, typ : string;
    thistty : shortstring;
begin
  WinClipboardSupported:=false;
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
  GetTextWinClipboardSize:=0;
end;

function GetTextWinClipBoardData(var P : PAnsiChar;var L : longint) : boolean;
begin
  GetTextWinClipBoardData:=true;
  GetGlobalClipboardData;
end;

function SetTextWinClipBoardData(P : PAnsiChar; L : longint) : boolean;
begin
  SetTextWinClipBoardData:= SetGlobalClipboardData(P,L);
end;
{$endif}



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


{$ifndef WinClipSupported}
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


end.
