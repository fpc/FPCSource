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

{$IFDEF FPC_DOTTEDUNITS}
{$ifdef FV_UNICODE}
uses System.Objects, FreeVision.Uapp;
{$else FV_UNICODE}
uses System.Objects, FreeVision.App;
{$endif FV_UNICODE}
{$ELSE}
{$ifdef FV_UNICODE}
uses objects,uapp;
{$else FV_UNICODE}
uses objects,app;
{$endif FV_UNICODE}
{$ENDIF}

{Should be called after InitKeyboard}
procedure InitClip(AProgram :PProgram);
procedure DoneClip;

{Request clipboard content}
{Actual clipboard content will be returned via event system, if terminal supports OSC 52}
procedure GetGlobalClipboardData;

{ Set clipboard content, if terminal supports OSC 52. Return true allways }
function SetGlobalClipboardData(P: PAnsiChar; ASize: longint): boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
{$ifdef unix}
  UnixApi.Base, System.Console.Keyboard,
{$endif}
{$ifdef FV_UNICODE}
  FreeVision.UDrivers,
{$else FV_UNICODE}
  FreeVision.Drivers,
{$endif FV_UNICODE}
  FreeVision.Fvconsts,FreeVision.Fvcommon;
{$ELSE}
uses
{$ifdef unix}
  baseUnix,keyboard,
{$endif}
{$ifdef FV_UNICODE}
  udrivers,
{$else FV_UNICODE}
  drivers,
{$endif FV_UNICODE}
  fvconsts,FVCommon;
{$ENDIF}
var cProgram : PProgram;
  PText : PAnsiChar;

{Could not use unit base64 because of Sysutils and reasons }
{Speed or reuseability here is not a concern               }
const
  EncodingTable: PAnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
Alphabet = ['a'..'z','A'..'Z','0'..'9','+','/','=']; // all 65 chars that are in the base64 encoding alphabet

{ Memory have to be preallocated;  source p, destination d }
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

{ Memory have to be preallocated;  source p, destination d }
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

{$ifdef unix}
procedure PutInEventQue (var zt: AnsiString;l:sw_integer);
var Event:TEvent;
begin
  if Assigned(PText) then
    FreeMem(PText);
  GetMem(PText,l+1);
  Move(zt[1],PText^,l+1);
  Event.What:=evCommand;
  Event.Command:=cmPasteText;
  Event.Id:=l; {length of pasted text}
  Event.InfoPtr:=pointer(PText);
  cProgram^.PutEvent(Event);
end;

procedure LinuxClipBoardData;
var zt,rt  : AnsiString;
  st  : ShortString;
  escSeq : ShortString;
  inEsc,inRead : boolean;
  k : sw_integer;
  ch : AnsiChar;
  timewait,finalparsec : TimeSpec;
  ree:longint;
  countemptines : sw_integer;
  l : sw_integer;
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
      st:=RawReadString
    else
      st:='';
    if length(st)=0 then
    begin
      inc(countemptines);
      if countemptines = 3 then break;
      {we might be ahead for a shortwhile, wait a little bit longer}
      timewait.tv_sec := 0;
      timewait.tv_nsec := 20000000;
      ree:=fpNanoSleep(@timewait,@finalparsec);
      continue;
    end;
    countemptines:=0;
    if st[1]=';' then
      st:=copy(st,2,length(st));
    for k:=1 to length(st) do
    begin
      ch:=st[k];
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
        continue;
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
    end; {for..}
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
  st  : ShortString;
  k : sw_integer;
  ch : AnsiChar;
  timewait,finalparsec : TimeSpec;
  ree:longint;
  countemptines : sw_integer;
  l : sw_integer;
begin
  countemptines:=0;
  zt:='';
  {-- read and parse --}
  while true do
  begin
    if keypressed then
      st:=RawReadString
    else
      st:='';
    if length(st)=0 then
    begin
      inc(countemptines);
      if countemptines = 3 then break;
      {we might be ahead for a shortwhile, wait a little bit longer}
      timewait.tv_sec := 0;
      timewait.tv_nsec := 20000000;
      ree:=fpNanoSleep(@timewait,@finalparsec);
      continue;
    end;
    countemptines:=0;
    for k:=1 to length(st) do
    begin
      ch:=st[k];
      if ch <> '~' then
        zt:=zt+ch  {data in}
      else
      begin
        {test for terminator string}
        if copy(zt,length(zt)-5,6)=#27'[201~' then
          break;
        zt:=zt+ch;  {data in}
      end;
    end; {for..}
  end; {while do }
  if length(zt)=0 then exit;
  l:=length(zt);
  if length(zt)>6 then
  begin
    l:=length(zt)-6;
    SetLength(zt,l); {get rid of bracketed paste end sequence}
  end;
  PutInEventQue(zt,l);
end;
{$endif}

procedure InitClip(AProgram :PProgram);
begin
{$ifdef unix}
  if Assigned(PText) then
    FreeMem(PText);
  PText:=nil;
  cProgram:=AProgram;
  AddSpecialSequence(#27'[200~',@BracketedPaste);
  AddSpecialSequence(#27']52;c',@LinuxClipBoardData);
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
  {GetGlobalClipboardData:=false;}
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

end.
