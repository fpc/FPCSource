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
function GetTextWinClipBoardData(var p : PAnsiChar;var l : longint) : boolean;
function SetTextWinClipBoardData(p : PAnsiChar;l : longint) : boolean;
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

{$ifdef linux}
  uses
    baseUnix,base64,keyboard,Objects,fvclip;
{$endif linux}

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

{$ifdef linux}
function WinClipboardSupported : boolean;
begin
  WinClipboardSupported:=true;
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

function InternGetDataSize : longint;
begin
  InternGetDataSize:=1; {there has to be something in order for menu to be active}
end;

function GetTextLinuxClipBoardData(var p : PAnsiChar;var l : longint) : boolean;
begin
  GetTextLinuxClipBoardData:=false;
  GetGlobalClipboardData;
end;
{$endif linux}

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
      InternGetDataSize:=strlen(PAnsiChar(GlobalLock(HC)))+1;
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

{$ifdef os2}
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
  PIB: PProcessInfoBlock;
  TIB: PThreadInfoBlock;
  PMWModHandle: THandle;
begin
  if OS2ClipboardSupported then
   Exit;
  DosGetInfoBlocks (TIB, PIB);
  OrigSessType := PIB^.tType;
  PIB^.tType := 3;

  RC := DosQueryModuleHandle ('PMWIN', PMWModHandle);
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
{$endif DOS}
{$ifdef linux}
  rez : boolean; {one variable needed to satisfy compiler}
{$endif linux}
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
  GetMem(p,l);
  GetDosMem(M,l);
  r.ax:=$1705;
  r.dx:=7{ OEM Text rather then 1 : Text };
  r.es:=M.DosSeg;
  r.bx:=M.DosOfs;
  RealIntr($2F,r);
  GetTextWinClipBoardData:=(r.ax<>0);
{$endif DOS}
{$ifdef linux}
  rez:=GetTextLinuxClipBoardData(p,l);
  GetTextWinClipBoardData:=rez;
{$endif linux}
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
{$endif DOS}
end;

function SetTextWinClipBoardData(p : PAnsiChar;l : longint) : boolean;
var
{$ifdef DOS}
  r : Registers;
  M : MemPtr;
{$endif DOS}
{$ifdef linux}
  st : AnsiString;
{$endif linux}
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
{$ifdef linux}
  SetTextWinClipBoardData:=SetGlobalClipboardData(p,l);
{$endif linux}
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

{$ifdef os2}
initialization
 InitClipboard;

finalization
 DoneClipboard;
{$endif os2}

{$endif WinClipSupported}
end.
