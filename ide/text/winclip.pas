{
    $Id$
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
unit WinClip;

interface

{$i globdir.inc}

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
{$ifdef go32v2}
  uses
    strings,go32;
{$endif go32v2}

{$ifdef win32}
  uses
    strings,windows;
{$endif win32}

{$ifdef go32v2}
function WinClipboardSupported : boolean;
var
  r : Registers;
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
  r.dx:=1;
  RealIntr($2F,r);
  InternGetDataSize:=(r.dx shl 16) + r.ax;
end;
{$endif go32v2}

{$ifdef win32}
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
begin
  InternGetDataSize:=-1;
end;
{$endif go32v2}


function GetTextWinClipboardSize : longint;
begin
  OpenWinClipboard;
  GetTextWinClipboardSize:=InternGetDataSize;
  CloseWinClipboard;
end;

function GetTextWinClipBoardData(var p : pchar;var l : longint) : boolean;
var
{$ifdef go32v2}
  r : Registers;
  tb_all : longint;
  tb_seg,tb_ofs,tb_sel : word;
{$endif go32v2}
{$ifdef win32}
  h : HGlobal;
  pp : pchar;
{$endif win32}
begin
  p:=nil;
  GetTextWinClipBoardData:=False;
  if not OpenWinClipBoard then
    exit;
{$ifdef go32v2}
  l:=InternGetDataSize;
  if (l=0) or (l>100000) then
    begin
      l:=0;
      CloseWinClipBoard;
      exit;
    end;
  GetMem(p,l);
  if l>tb_size then
    begin
      tb_all:=global_dos_alloc(l);
      { zero means allocation failure }
      if tb_all=0 then
        begin
          FreeMem(p,l);
          p:=nil;
          l:=0;
          CloseWinClipBoard;
          exit;
        end;
      tb_seg:=tb_all shr 16;
      tb_sel:=tb_all and $ffff;
    end
  else
    begin
      tb_seg:=tb_segment;
      tb_ofs:=tb_offset;
      tb_sel:=0;
    end;
  r.ax:=$1705;
  r.dx:=1;
  r.es:=tb_seg;
  r.bx:=tb_ofs;
  RealIntr($2F,r);
  GetTextWinClipBoardData:=(r.ax<>0);
{$endif go32v2}
{$ifdef win32}
  h:=GetClipboardData(CF_TEXT);
  if h<>0 then
    begin
      pp:=pchar(GlobalLock(h));
      l:=strlen(pp)+1;
      getmem(p,l);
      move(pp^,p^,l);
      GlobalUnlock(h);
    end;
  GetTextWinClipBoardData:=h<>0;
{$endif win32}
  CloseWinClipBoard;
{$ifdef go32v2}
  DosMemGet(tb_seg,tb_ofs,p^,l);
  if tb_sel<>0 then
    global_dos_free(tb_sel);
{$endif go32v2}
end;

function SetTextWinClipBoardData(p : pchar;l : longint) : boolean;
var
{$ifdef go32v2}
  r : Registers;
  tb_all : longint;
  tb_seg,tb_ofs,tb_sel : word;
{$endif go32v2}
{$ifdef win32}
  h : HGlobal;
  pp : pchar;
{$endif win32}
begin
  SetTextWinClipBoardData:=False;
  if (l=0) or (l>100000) then
    exit;
  if not OpenWinClipBoard then
    exit;
  EmptyWinClipBoard;
{$ifdef go32v2}
  if l>tb_size then
    begin
      tb_all:=global_dos_alloc(l);
      { zero means allocation failure }
      if tb_all=0 then
        begin
          CloseWinClipBoard;
          exit;
        end;
      tb_seg:=tb_all shr 16;
      tb_sel:=tb_all and $ffff;
    end
  else
    begin
      tb_seg:=tb_segment;
      tb_ofs:=tb_offset;
      tb_sel:=0;
    end;
  DosMemPut(tb_seg,tb_ofs,p^,l);
  r.ax:=$1703;
  r.dx:=1;
  r.es:=tb_seg;
  r.bx:=tb_ofs;
  r.si:=l shr 16;
  r.cx:=l and $ffff;
  RealIntr($2F,r);
  SetTextWinClipBoardData:=(r.ax<>0);
  if tb_sel<>0 then
    global_dos_free(tb_sel);
{$endif go32v2}
{$ifdef win32}
  h:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,l);
  pp:=pchar(GlobalLock(h));
  move(p^,pp^,l);
  GlobalUnlock(h);
  SetTextWinClipBoardData:=(SetClipboardData(CF_TEXT,h)=h);
{$endif win32}
  CloseWinClipBoard;
end;

{$endif WinClipSupported}

end.

{
 $Log $
}
