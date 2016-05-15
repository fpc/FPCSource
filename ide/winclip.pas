{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999 by Pierre Muller

    Connection with Windows Clipboard
    based on Ralph Brown Interrupt List

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit WinClip;

interface

{$ifdef WinClipSupported}

function WinClipboardSupported : boolean;
function OpenWinClipboard : boolean;
function EmptyWinClipboard : boolean;
function GetTextWinClipboardSize : longint;
function GetTextWinClipBoardData(var p : pchar;var l : longint) : boolean;
function SetTextWinClipBoardData(p : pchar;l : longint) : boolean;
{$endif WinClipSupported}

implementation

{$ifdef WinClipSupported}
{$ifdef DOS}
  uses
    pmode,
{$ifdef go32v2}
    {go32   sorry Gabor, but its still not compiling without that ! }
    {now it works. btw. you don't have to sorry - just to tell me... ;)) Gabor }
{$endif go32v2}
    dos;
{$endif DOS}

{$ifdef Windows}
  uses
    strings,windows;
{$endif Windows}

{$ifdef HASAMIGA}
  uses
    clipboard,cliputils;
{$endif}


{$ifdef DOS}
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

{$ifdef Windows}
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
      InternGetDataSize:=strlen(pchar(GlobalLock(HC)))+1;
      GlobalUnlock(HC);
    end
  else
    InternGetDataSize:=0;
end;
{$endif Windows}

{$ifdef HASAMIGA}
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


function GetTextWinClipboardSize : longint;
begin
  OpenWinClipboard;
  GetTextWinClipboardSize:=InternGetDataSize;
  CloseWinClipboard;
end;

function GetTextWinClipBoardData(var p : pchar;var l : longint) : boolean;
var
{$ifdef DOS}
  r : Registers;
  M : MemPtr;
{$endif DOS}
{$ifdef Windows}
  h : HGlobal;
  pp : pchar;
{$endif Windows}
{$ifdef HASAMIGA}
  Text: AnsiString;
  pp: PChar;
{$endif HASAMIGA}
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
  GetMem(p,l);
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
      pp:=pchar(GlobalLock(h));
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
  CloseWinClipBoard;
{$ifdef DOS}
  M.MoveDataFrom(l,P^);
  FreeDosMem(M);
{$endif DOS}
end;

function SetTextWinClipBoardData(p : pchar;l : longint) : boolean;
var
{$ifdef DOS}
  r : Registers;
  M : MemPtr;
{$endif DOS}
{$ifdef Windows}
  h : HGlobal;
  pp : pchar;
  res : boolean;
{$endif Windows}
{$ifdef HASAMIGA}
  pp: PChar;
  Test: AnsiString;
{$endif HASAMIGA}
begin
  SetTextWinClipBoardData:=False;
  if (l=0) or (l>65520) then
    exit;
  if not OpenWinClipBoard then
    exit;
  EmptyWinClipBoard;
{$ifdef DOS}
  GetDosMem(M,l+1);
  M.MoveDataTo(P^,l+1);
  r.ax:=$1703;
  r.dx:=7{ OEM Text rather then 1 : Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  r.si:=l shr 16;
  r.cx:=l and $ffff;
  RealIntr($2F,r);
  SetTextWinClipBoardData:=(r.ax<>0);
  r.ax:=$1703;
  r.dx:=1{ Empty  Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  r.si:=0;
  r.cx:=0;
  RealIntr($2F,r);
  FreeDosMem(M);
{$endif DOS}
{$ifdef Windows}
  h:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,l+1);
  pp:=pchar(GlobalLock(h));
  move(p^,pp^,l+1);
  GlobalUnlock(h);
  res:=(SetClipboardData(CF_OEMTEXT,h)=h);
  h:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,l+1);
  pp:=pchar(GlobalLock(h));
  OemToCharBuffA(p,pp,l+1);
  SetClipboardData(CF_TEXT,h);
  GlobalUnlock(h);
  SetTextWinClipBoardData:=res;
{$endif Windows}
{$ifdef HASAMIGA}
  PutTextToClip(0, AnsiString(p));
{$endif HASAMIGA}
  CloseWinClipBoard;
end;

{$endif WinClipSupported}
end.
