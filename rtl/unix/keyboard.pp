{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Keyboard unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Keyboard;

{*****************************************************************************}
                                  interface
{*****************************************************************************}

{$i keybrdh.inc}

const
  AltPrefix : byte = 0;
  ShiftPrefix : byte = 0;
  CtrlPrefix : byte = 0;

function RawReadKey:char;
function RawReadString : String;
function KeyPressed : Boolean;
procedure AddSequence(const St : String; AChar,AScan :byte);
function FindSequence(const St : String;var AChar, Ascan : byte) : boolean;
procedure RestoreStartMode;

{*****************************************************************************}
                               implementation
{*****************************************************************************}

uses
  Mouse,  Strings,
  termio,baseUnix
  {$ifdef linux},linuxvcs{$endif};

{$i keyboard.inc}

var OldIO,StartTio : TermIos;
{$ifdef linux}
    is_console:boolean;
    vt_switched_away:boolean;
{$endif}
{$ifdef logging}
    f : text;
{$endif logging}

const
  KeyBufferSize = 20;
var
  KeyBuffer : Array[0..KeyBufferSize-1] of Char;
  KeyPut,
  KeySend   : longint;

{ Buffered Input routines }
const
  InSize=256;
var
  InBuf  : array [0..InSize-1] of char;
{  InCnt,}
  InHead,
  InTail : longint;

{$i keyscan.inc}

{Some internal only scancodes}
const KbShiftUp    = $f0;
      KbShiftLeft  = $f1;
      KbShiftRight = $f2;
      KbShiftDown  = $f3;
      KbShiftHome  = $f4;
      KbShiftEnd   = $f5;

{$ifdef Unused}
type
   TKeyState = Record
      Normal, Shift, Ctrl, Alt : word;
     end;

const
  KeyStates : Array[0..255] of TKeyState
    (

    );

{$endif Unused}

procedure SetRawMode(b:boolean);
var
  Tio : Termios;
Begin
  TCGetAttr(1,Tio);
  if b then
   begin
     OldIO:=Tio;
     CFMakeRaw(Tio);
   end
  else
    Tio := OldIO;
  TCSetAttr(1,TCSANOW,Tio);
End;

{$ifdef linux}

{The Linux console can do nice things: we can get the state of the shift keys,
 and reprogram the keys. That's nice since it allows excellent circumvention
 of VT100 limitations, we can make the keyboard work 100%...

 A 100% working keyboard seems to be a pretty basic requirement, but we're
 one of the few guys providing such an outrageous luxury (DM).}

type
  chgentry=packed record
    tab,
    idx,
    oldtab,
    oldidx : byte;
    oldval,
    newval : word;
  end;
  kbentry=packed record
    kb_table,
    kb_index : byte;
    kb_value : word;
  end;
  kbsentry=packed record
    kb_func:byte;
    kb_string:array[0..511] of char;
  end;
  vt_mode=packed record
    mode,          {vt mode}
    waitv:byte;    {if set, hang on writes if not active}
    relsig,        {signal to raise on release req}
    acqsig,        {signal to raise on acquisition}
    frsig:word;    {unused (set to 0)}
 end;

const
  kbdchange:array[0..23] of chgentry=(
    {This prevents the alt+function keys from switching consoles.
     We code the F1..F12 sequences into ALT+F1..ALT+12, we check
     the shiftstates separetely anyway.}
    (tab:8; idx:$3b; oldtab:0; oldidx:$3b; oldval:0; newval:0),
    (tab:8; idx:$3c; oldtab:0; oldidx:$3c; oldval:0; newval:0),
    (tab:8; idx:$3d; oldtab:0; oldidx:$3d; oldval:0; newval:0),
    (tab:8; idx:$3e; oldtab:0; oldidx:$3e; oldval:0; newval:0),
    (tab:8; idx:$3f; oldtab:0; oldidx:$3f; oldval:0; newval:0),
    (tab:8; idx:$40; oldtab:0; oldidx:$40; oldval:0; newval:0),
    (tab:8; idx:$41; oldtab:0; oldidx:$41; oldval:0; newval:0),
    (tab:8; idx:$42; oldtab:0; oldidx:$42; oldval:0; newval:0),
    (tab:8; idx:$43; oldtab:0; oldidx:$43; oldval:0; newval:0),
    (tab:8; idx:$44; oldtab:0; oldidx:$44; oldval:0; newval:0),
    (tab:8; idx:$45; oldtab:0; oldidx:$45; oldval:0; newval:0),
    (tab:8; idx:$46; oldtab:0; oldidx:$46; oldval:0; newval:0),
    {This prevents the shift+function keys outputting strings, so
     the kernel will the codes for the non-shifted function
     keys. This is desired because normally shift+f1/f2 will output the
     same string as f11/12. We will get the shift state separately.}
    (tab:1; idx:$3b; oldtab:0; oldidx:$3b; oldval:0; newval:0),
    (tab:1; idx:$3c; oldtab:0; oldidx:$3c; oldval:0; newval:0),
    (tab:1; idx:$3d; oldtab:0; oldidx:$3d; oldval:0; newval:0),
    (tab:1; idx:$3e; oldtab:0; oldidx:$3e; oldval:0; newval:0),
    (tab:1; idx:$3f; oldtab:0; oldidx:$3f; oldval:0; newval:0),
    (tab:1; idx:$40; oldtab:0; oldidx:$40; oldval:0; newval:0),
    (tab:1; idx:$41; oldtab:0; oldidx:$41; oldval:0; newval:0),
    (tab:1; idx:$42; oldtab:0; oldidx:$42; oldval:0; newval:0),
    (tab:1; idx:$43; oldtab:0; oldidx:$43; oldval:0; newval:0),
    (tab:1; idx:$44; oldtab:0; oldidx:$44; oldval:0; newval:0),
    (tab:1; idx:$45; oldtab:0; oldidx:$45; oldval:0; newval:0),
    (tab:1; idx:$46; oldtab:0; oldidx:$46; oldval:0; newval:0)
  );

 KDGKBENT=$4B46;
 KDSKBENT=$4B47;
 KDGKBSENT=$4B48;
 KDSKBSENT=$4B49;
 KDGKBMETA=$4B62;
 KDSKBMETA=$4B63;
 K_ESCPREFIX=$4;
 K_METABIT=$3;
 VT_GETMODE=$5601;
 VT_SETMODE=$5602;
 VT_RELDISP=$5605;
 VT_PROCESS=1;

const
  oldmeta : longint = 0;
  meta : longint = 0;

var oldesc0,oldesc1,oldesc2,oldesc4,oldesc8:word;

procedure prepare_patching;

var e:^chgentry;
    entry : kbentry;
    i:longint;

begin
  for i:=low(kbdchange) to high(kbdchange) do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     fpIoctl(stdinputhandle,KDGKBENT,@entry);
     e^.oldval:=entry.kb_value;
     entry.kb_table:=e^.oldtab;
     entry.kb_index:=e^.oldidx;
     fpioctl(stdinputhandle,KDGKBENT,@entry);
     e^.newval:=entry.kb_value;
   end;
  {Save old escape code.}
  entry.kb_index:=1;
  entry.kb_table:=0;
  fpioctl(stdinputhandle,KDGKBENT,@entry);
  oldesc0:=entry.kb_value;
  entry.kb_table:=1;
  fpioctl(stdinputhandle,KDGKBENT,@entry);
  oldesc1:=entry.kb_value;
  entry.kb_table:=2;
  fpioctl(stdinputhandle,KDGKBENT,@entry);
  oldesc2:=entry.kb_value;
  entry.kb_table:=4;
  fpioctl(stdinputhandle,KDGKBENT,@entry);
  oldesc4:=entry.kb_value;
  entry.kb_table:=8;
  fpioctl(stdinputhandle,KDGKBENT,@entry);
  oldesc8:=entry.kb_value;
end;

procedure PatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  sentry : kbsentry;
  i:longint;
begin
  fpIoctl(stdinputhandle,KDGKBMETA,@oldmeta);
  meta:=K_ESCPREFIX;
  fpIoctl(stdinputhandle,KDSKBMETA,@meta);
  for i:=low(kbdchange) to high(kbdchange) do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.newval;
     fpioctl(stdinputhandle,KDSKBENT,@entry);
   end;

  {Map kernel escape key code to symbol F32.}
  entry.kb_index:=1;
  entry.kb_value:=$011f;
  entry.kb_table:=0;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=1;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=2;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=4;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=8;
  fpioctl(stdinputhandle,KDSKBENT,@entry);

  {F32 (the escape key) will generate ^[[0~ .}
  sentry.kb_func:=31;
  sentry.kb_string:=#27'[0~';
  fpioctl(stdinputhandle,KDSKBSENT,@sentry);
end;


procedure UnpatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  i : longint;
begin
  if oldmeta in [K_ESCPREFIX,K_METABIT] then
    fpioctl(stdinputhandle,KDSKBMETA,@oldmeta);
  for i:=low(kbdchange) to high(kbdchange) do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.oldval;
     fpioctl(stdinputhandle,KDSKBENT,@entry);
   end;

  entry.kb_index:=1;
  entry.kb_table:=0;
  entry.kb_value:=oldesc0;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=1;
  entry.kb_value:=oldesc1;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=2;
  entry.kb_value:=oldesc2;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=4;
  entry.kb_value:=oldesc4;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
  entry.kb_table:=8;
  entry.kb_value:=oldesc8;
  fpioctl(stdinputhandle,KDSKBENT,@entry);
end;

{A problem of patching the keyboard is that it no longer works as expected
 when working on another console. So we unpatch it when the user switches
 away.}

const switches:longint=0;

procedure vt_handler(sig:longint);cdecl;

begin
  if vt_switched_away then
    begin
      {Confirm the switch.}
      fpioctl(stdoutputhandle,VT_RELDISP,pointer(2));
      {Switching to program, patch keyboard.}
      patchkeyboard;
    end
  else
    begin
      {Switching away from program, unpatch the keyboard.}
      unpatchkeyboard;
      fpioctl(stdoutputhandle,VT_RELDISP,pointer(1));
    end;
  vt_switched_away:=not vt_switched_away;
  {Clear buffer.}
  intail:=inhead;
end;

procedure install_vt_handler;

var mode:vt_mode;

begin
{  ioctl(vt_fd,KDSETMODE,KD_GRAPHICS);}
    fpioctl(stdoutputhandle,VT_GETMODE,@mode);
    mode.mode:=VT_PROCESS;
    mode.relsig:=SIGUSR1;
    mode.acqsig:=SIGUSR1;
    vt_switched_away:=false;
    fpsignal(SIGUSR1,@vt_handler);
    fpioctl(stdoutputhandle,VT_SETMODE,@mode);
end;
{$endif}

function ttyRecvChar:char;

var Readed,i : longint;

begin
  {Buffer empty? Yes, input from stdin}
  if (InHead=InTail) then
    begin
      {Calc Amount of Chars to Read}
      i:=InSize-InHead;
      if InTail>InHead then
        i:=InTail-InHead;
      {Read}
      repeat
        Readed:=fpRead(StdInputHandle,InBuf[InHead],i);
      until readed<>-1;
      {Increase Counters}
      inc(InHead,Readed);
      {Wrap if End has Reached}
      if InHead>=InSize then
        InHead:=0;
    end;
  {Check Buffer}
  ttyRecvChar:=InBuf[InTail];
  inc(InTail);
  if InTail>=InSize then
    InTail:=0;
end;

procedure PushKey(Ch:char);
var
  Tmp : Longint;
begin
  Tmp:=KeyPut;
  Inc(KeyPut);
  If KeyPut>=KeyBufferSize Then
   KeyPut:=0;
  If KeyPut<>KeySend Then
   KeyBuffer[Tmp]:=Ch
  Else
   KeyPut:=Tmp;
End;


function PopKey:char;
begin
  If KeyPut<>KeySend Then
   begin
     PopKey:=KeyBuffer[KeySend];
     Inc(KeySend);
     If KeySend>=KeyBufferSize Then
      KeySend:=0;
   End
  Else
   PopKey:=#0;
End;


procedure PushExt(b:byte);
begin
  PushKey(#0);
  PushKey(chr(b));
end;


const
  AltKeyStr  : string[38]='qwertyuiopasdfghjklzxcvbnm1234567890-=';
  AltCodeStr : string[38]=#016#017#018#019#020#021#022#023#024#025#030#031#032#033#034#035#036#037#038+
                          #044#045#046#047#048#049#050#120#121#122#123#124#125#126#127#128#129#130#131;
function FAltKey(ch:char):byte;
var
  Idx : longint;
begin
  Idx:=Pos(ch,AltKeyStr);
  if Idx>0 then
   FAltKey:=byte(AltCodeStr[Idx])
  else
   FAltKey:=0;
End;


{ This one doesn't care about keypresses already processed by readkey  }
{ and waiting in the KeyBuffer, only about waiting keypresses at the   }
{ TTYLevel (including ones that are waiting in the TTYRecvChar buffer) }
function sysKeyPressed: boolean;
var
  fdsin : tfdSet;
begin
  if (inhead<>intail) then
   sysKeyPressed:=true
  else
   begin
     fpFD_ZERO(fdsin);
     fpFD_SET(StdInputHandle,fdsin);
     sysKeypressed:=(fpSelect(StdInputHandle+1,@fdsin,nil,nil,0)>0);
   end;
end;

function KeyPressed:Boolean;
begin
  Keypressed := (KeySend<>KeyPut) or sysKeyPressed;
End;


const
  LastMouseEvent : TMouseEvent =
  (
    Buttons : 0;
    X : 0;
    Y : 0;
    Action : 0;
  );

  procedure GenMouseEvent;
  var MouseEvent: TMouseEvent;
      ch : char;
      fdsin : tfdSet;
  begin
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle,fdsin);
    Fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
    { Other bits are used for Shift, Meta and Ctrl modifiers PM }
    case (ord(ch)-ord(' ')) and 3  of
      0 : {left button press}
        MouseEvent.buttons:=1;
      1 : {middle button pressed }
        MouseEvent.buttons:=2;
      2 : { right button pressed }
        MouseEvent.buttons:=4;
      3 : { no button pressed };
      end;
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.x:=Ord(ch)-ord(' ')-1;
     if inhead=intail then
      fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.y:=Ord(ch)-ord(' ')-1;
     if (MouseEvent.buttons<>0) then
       MouseEvent.action:=MouseActionDown
     else
       begin
         if (LastMouseEvent.Buttons<>0) and
            ((LastMouseEvent.X<>MouseEvent.X) or (LastMouseEvent.Y<>MouseEvent.Y)) then
           begin
             MouseEvent.Action:=MouseActionMove;
             MouseEvent.Buttons:=LastMouseEvent.Buttons;
{$ifdef DebugMouse}
             Writeln(system.stderr,' Mouse Move (',MouseEvent.X,',',MouseEvent.Y,')');
{$endif DebugMouse}
             PutMouseEvent(MouseEvent);
             MouseEvent.Buttons:=0;
           end;
         MouseEvent.Action:=MouseActionUp;
       end;
     PutMouseEvent(MouseEvent);
{$ifdef DebugMouse}
     if MouseEvent.Action=MouseActionDown then
       Write(system.stderr,'Button down : ')
     else
       Write(system.stderr,'Button up : ');
     Writeln(system.stderr,'buttons = ',MouseEvent.Buttons,' (',MouseEvent.X,',',MouseEvent.Y,')');
{$endif DebugMouse}
     LastMouseEvent:=MouseEvent;
  end;

type
  Tprocedure = procedure;

  PTreeElement = ^TTreeElement;
  TTreeElement = record
    Next,Parent,Child :  PTreeElement;
    CanBeTerminal : boolean;
    char : byte;
    ScanValue : byte;
    CharValue : byte;
    SpecialHandler : Tprocedure;
  end;

var
  RootTree : Array[0..255] of PTreeElement;

procedure FreeElement (PT:PTreeElement);
var next : PTreeElement;
begin
  while PT <> nil do
  begin
    FreeElement(PT^.Child);
    next := PT^.Next;
    dispose(PT);
    PT := next;
  end;
end;

procedure FreeTree;
var i : integer;
begin
  for i := low(RootTree) to high(RootTree) do
  begin
    FreeElement(RootTree[i]);
    RootTree[i] := nil;
  end;
end;

function NewPTree(ch : byte;Pa : PTreeElement) : PTreeElement;
var PT : PTreeElement;
begin
  New(PT);
  FillChar(PT^,SizeOf(TTreeElement),#0);
  PT^.char:=ch;
  PT^.Parent:=Pa;
  if Assigned(Pa) and (Pa^.Child=nil) then
    Pa^.Child:=PT;
  NewPTree:=PT;
end;

function DoAddSequence(const St : String; AChar,AScan :byte) : PTreeElement;
var
  CurPTree,NPT : PTreeElement;
  c : byte;
  i : longint;
begin
  if St='' then
    begin
      DoAddSequence:=nil;
      exit;
    end;
  CurPTree:=RootTree[ord(st[1])];
  if CurPTree=nil then
    begin
      CurPTree:=NewPTree(ord(st[1]),nil);
      RootTree[ord(st[1])]:=CurPTree;
    end;
  for i:=2 to Length(St) do
    begin
      NPT:=CurPTree^.Child;
      c:=ord(St[i]);
      if NPT=nil then
        NPT:=NewPTree(c,CurPTree);
      CurPTree:=nil;
      while assigned(NPT) and (NPT^.char<c) do
        begin
          CurPTree:=NPT;
          NPT:=NPT^.Next;
        end;

      if assigned(NPT) and (NPT^.char=c) then
        CurPTree:=NPT
      else
        begin
          if CurPTree=nil then
            begin
              NPT^.Parent^.child:=NewPTree(c,NPT^.Parent);
              CurPTree:=NPT^.Parent^.Child;
              CurPTree^.Next:=NPT;
            end
          else
            begin
              CurPTree^.Next:=NewPTree(c,CurPTree^.Parent);
              CurPTree:=CurPTree^.Next;
              CurPTree^.Next:=NPT;
            end;
        end;
    end;
  if CurPTree^.CanBeTerminal then
    begin
      { here we have a conflict !! }
      { maybe we should claim }
      with CurPTree^ do
        begin
{$ifdef DEBUG}
          if (ScanValue<>AScan) or (CharValue<>AChar) then
            Writeln(system.stderr,'key "',st,'" changed value');
          if (ScanValue<>AScan) then
            Writeln(system.stderr,'Scan was ',ScanValue,' now ',AScan);
          if (CharValue<>AChar) then
            Writeln(system.stderr,'Char was ',chr(CharValue),' now ',chr(AChar));
{$endif DEBUG}
          ScanValue:=AScan;
          CharValue:=AChar;
        end;
    end
  else with CurPTree^ do
    begin
      CanBeTerminal:=True;
      ScanValue:=AScan;
      CharValue:=AChar;
    end;
  DoAddSequence:=CurPTree;
end;


procedure AddSequence(const St : String; AChar,AScan :byte);
begin
  DoAddSequence(St,AChar,AScan);
end;

{ Returns the Child that as c as char if it exists }
function FindChild(c : byte;Root : PTreeElement) : PTreeElement;
var
  NPT : PTreeElement;
begin
  if not assigned(Root) then
    begin
      FindChild:=nil;
      exit;
    end;
  NPT:=Root^.Child;
  while assigned(NPT) and (NPT^.char<c) do
    NPT:=NPT^.Next;
  if assigned(NPT) and (NPT^.char=c) then
    FindChild:=NPT
  else
    FindChild:=nil;
end;

function AddSpecialSequence(const St : string;Proc : Tprocedure) : PTreeElement;
var
  NPT : PTreeElement;
begin
  NPT:=DoAddSequence(St,0,0);
  NPT^.SpecialHandler:=Proc;
  AddSpecialSequence:=NPT;
end;

function FindSequence(const St : String;var AChar,AScan :byte) : boolean;
var
  NPT : PTreeElement;
  I : longint;
begin
  FindSequence:=false;
  AChar:=0;
  AScan:=0;
  if St='' then
    exit;
  NPT:=RootTree[ord(St[1])];
  if not assigned(NPT) then
    exit;
  for i:=2 to Length(St) do
    begin
      NPT:=FindChild(ord(St[i]),NPT);
      if not assigned(NPT) then
        exit;
    end;
  if not NPT^.CanBeTerminal then
    exit
  else
    begin
      FindSequence:=true;
      AScan:=NPT^.ScanValue;
      AChar:=NPT^.CharValue;
    end;
end;

procedure LoadDefaultSequences;
begin
  AddSpecialSequence(#27'[M',@GenMouseEvent);
  {Unix backspace/delete hell... Is #127 a backspace or delete?}
  if copy(fpgetenv('TERM'),1,4)='cons' then
    begin
      {FreeBSD is until now only terminal that uses it for delete.}
      DoAddSequence(#127,0,kbDel);        {Delete}
      DoAddSequence(#27#127,0,kbAltDel);  {Alt+delete}
    end
  else
    begin
      DoAddSequence(#127,8,0);            {Backspace}
      DoAddSequence(#27#127,0,kbAltBack); {Alt+backspace}
    end;
  { all Esc letter }
  DoAddSequence(#27'A',0,kbAltA);
  DoAddSequence(#27'a',0,kbAltA);
  DoAddSequence(#27'B',0,kbAltB);
  DoAddSequence(#27'b',0,kbAltB);
  DoAddSequence(#27'C',0,kbAltC);
  DoAddSequence(#27'c',0,kbAltC);
  DoAddSequence(#27'D',0,kbAltD);
  DoAddSequence(#27'd',0,kbAltD);
  DoAddSequence(#27'E',0,kbAltE);
  DoAddSequence(#27'e',0,kbAltE);
  DoAddSequence(#27'F',0,kbAltF);
  DoAddSequence(#27'f',0,kbAltF);
  DoAddSequence(#27'G',0,kbAltG);
  DoAddSequence(#27'g',0,kbAltG);
  DoAddSequence(#27'H',0,kbAltH);
  DoAddSequence(#27'h',0,kbAltH);
  DoAddSequence(#27'I',0,kbAltI);
  DoAddSequence(#27'i',0,kbAltI);
  DoAddSequence(#27'J',0,kbAltJ);
  DoAddSequence(#27'j',0,kbAltJ);
  DoAddSequence(#27'K',0,kbAltK);
  DoAddSequence(#27'k',0,kbAltK);
  DoAddSequence(#27'L',0,kbAltL);
  DoAddSequence(#27'l',0,kbAltL);
  DoAddSequence(#27'M',0,kbAltM);
  DoAddSequence(#27'm',0,kbAltM);
  DoAddSequence(#27'N',0,kbAltN);
  DoAddSequence(#27'n',0,kbAltN);
  DoAddSequence(#27'O',0,kbAltO);
  DoAddSequence(#27'o',0,kbAltO);
  DoAddSequence(#27'P',0,kbAltP);
  DoAddSequence(#27'p',0,kbAltP);
  DoAddSequence(#27'Q',0,kbAltQ);
  DoAddSequence(#27'q',0,kbAltQ);
  DoAddSequence(#27'R',0,kbAltR);
  DoAddSequence(#27'r',0,kbAltR);
  DoAddSequence(#27'S',0,kbAltS);
  DoAddSequence(#27's',0,kbAltS);
  DoAddSequence(#27'T',0,kbAltT);
  DoAddSequence(#27't',0,kbAltT);
  DoAddSequence(#27'U',0,kbAltU);
  DoAddSequence(#27'u',0,kbAltU);
  DoAddSequence(#27'V',0,kbAltV);
  DoAddSequence(#27'v',0,kbAltV);
  DoAddSequence(#27'W',0,kbAltW);
  DoAddSequence(#27'w',0,kbAltW);
  DoAddSequence(#27'X',0,kbAltX);
  DoAddSequence(#27'x',0,kbAltX);
  DoAddSequence(#27'Y',0,kbAltY);
  DoAddSequence(#27'y',0,kbAltY);
  DoAddSequence(#27'Z',0,kbAltZ);
  DoAddSequence(#27'z',0,kbAltZ);
  DoAddSequence(#27'-',0,kbAltMinus);
  DoAddSequence(#27'=',0,kbAltEqual);
  DoAddSequence(#27'0',0,kbAlt0);
  DoAddSequence(#27'1',0,kbAlt1);
  DoAddSequence(#27'2',0,kbAlt2);
  DoAddSequence(#27'3',0,kbAlt3);
  DoAddSequence(#27'4',0,kbAlt4);
  DoAddSequence(#27'5',0,kbAlt5);
  DoAddSequence(#27'6',0,kbAlt6);
  DoAddSequence(#27'7',0,kbAlt7);
  DoAddSequence(#27'8',0,kbAlt8);
  DoAddSequence(#27'9',0,kbAlt9);

  DoAddSequence(#27'[[A',0,kbF1);           {linux,konsole,xterm}
  DoAddSequence(#27'[[B',0,kbF2);           {linux,konsole,xterm}
  DoAddSequence(#27'[[C',0,kbF3);           {linux,konsole,xterm}
  DoAddSequence(#27'[[D',0,kbF4);           {linux,konsole,xterm}
  DoAddSequence(#27'[[E',0,kbF5);           {linux,konsole}
  DoAddSequence(#27'[0~',0,kbEsc);          {if linux keyboard patched, escape
                                             returns this}
  DoAddSequence(#27'[1~',0,kbHome);         {linux}
  DoAddSequence(#27'[2~',0,kbIns);          {linux,Eterm}
  DoAddSequence(#27'[3~',0,kbDel);          {linux,Eterm}
  DoAddSequence(#27'[4~',0,kbEnd);          {linux,Eterm}
  DoAddSequence(#27'[5~',0,kbPgUp);         {linux,Eterm}
  DoAddSequence(#27'[6~',0,kbPgDn);         {linux,Eterm}
  DoAddSequence(#27'[7~',0,kbHome);         {Eterm}
  DoAddSequence(#27'[11~',0,kbF1);          {Eterm}
  DoAddSequence(#27'[12~',0,kbF2);          {Eterm}
  DoAddSequence(#27'[13~',0,kbF3);          {Eterm}
  DoAddSequence(#27'[14~',0,kbF4);          {Eterm}
  DoAddSequence(#27'[15~',0,kbF5);          {xterm,Eterm,gnome}
  DoAddSequence(#27'[17~',0,kbF6);          {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[18~',0,kbF7);          {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[19~',0,kbF8);          {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[20~',0,kbF9);          {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[21~',0,kbF10);         {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[23~',0,kbF11);         {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[24~',0,kbF12);         {linux,xterm,Eterm,konsole,gnome}
  DoAddSequence(#27'[25~',0,kbShiftF3);     {linux}
  DoAddSequence(#27'[26~',0,kbShiftF4);     {linux}
  DoAddSequence(#27'[28~',0,kbShiftF5);     {linux}
  DoAddSequence(#27'[29~',0,kbShiftF6);     {linux}
  DoAddSequence(#27'[31~',0,kbShiftF7);     {linux}
  DoAddSequence(#27'[32~',0,kbShiftF8);     {linux}
  DoAddSequence(#27'[33~',0,kbShiftF9);     {linux}
  DoAddSequence(#27'[34~',0,kbShiftF10);    {linux}
  DoAddSequence(#27'[2;2~',0,kbShiftIns);   {should be the code, but shift+ins
                                             is paste X clipboard in many
                                             terminal emulators :(}
  DoAddSequence(#27'[3;2~',0,kbShiftDel);   {xterm,konsole}
  DoAddSequence(#27'[11;2~',0,kbShiftF1);   {konsole in vt420pc mode}
  DoAddSequence(#27'[12;2~',0,kbShiftF2);   {konsole in vt420pc mode}
  DoAddSequence(#27'[13;2~',0,kbShiftF3);   {konsole in vt420pc mode}
  DoAddSequence(#27'[14;2~',0,kbShiftF4);   {konsole in vt420pc mode}
  DoAddSequence(#27'[15;2~',0,kbShiftF5);   {xterm}
  DoAddSequence(#27'[17;2~',0,kbShiftF6);   {xterm}
  DoAddSequence(#27'[18;2~',0,kbShiftF7);   {xterm}
  DoAddSequence(#27'[19;2~',0,kbShiftF8);   {xterm}
  DoAddSequence(#27'[20;2~',0,kbShiftF9);   {xterm}
  DoAddSequence(#27'[21;2~',0,kbShiftF10);  {xterm}
  DoAddSequence(#27'[23;2~',0,kbShiftF11);  {xterm}
  DoAddSequence(#27'[24;2~',0,kbShiftF12);  {xterm}
  DoAddSequence(#27'[2;5~',0,kbCtrlIns);    {xterm}
  DoAddSequence(#27'[3;5~',0,kbCtrlDel);    {xterm}
  DoAddSequence(#27#27'[[A',0,kbAltF1);
  DoAddSequence(#27#27'[[B',0,kbAltF2);
  DoAddSequence(#27#27'[[C',0,kbAltF3);
  DoAddSequence(#27#27'[[D',0,kbAltF4);
  DoAddSequence(#27#27'[[E',0,kbAltF5);
  DoAddSequence(#27#27'[17~',0,kbAltF6);
  DoAddSequence(#27#27'[18~',0,kbAltF7);
  DoAddSequence(#27#27'[19~',0,kbAltF8);
  DoAddSequence(#27#27'[20~',0,kbAltF9);
  DoAddSequence(#27#27'[21~',0,kbAltF10);
  DoAddSequence(#27#27'[23~',0,kbAltF11);
  DoAddSequence(#27#27'[24~',0,kbAltF12);
  DoAddSequence(#27'[A',0,kbUp);            {linux,FreeBSD}
  DoAddSequence(#27'[B',0,kbDown);          {linux,FreeBSD}
  DoAddSequence(#27'[C',0,kbRight);         {linux,FreeBSD}
  DoAddSequence(#27'[D',0,kbLeft);          {linux,FreeBSD}
  DoAddSequence(#27'[F',0,kbEnd);           {FreeBSD}
  DoAddSequence(#27'[G',0,kbPgUp);          {FreeBSD}
  DoAddSequence(#27'[H',0,kbHome);          {FreeBSD}
  DoAddSequence(#27'[H',0,kbPgdn);          {FreeBSD}
  DoAddSequence(#27'[M',0,kbF1);            {FreeBSD}
  DoAddSequence(#27'[N',0,kbF2);            {FreeBSD}
  DoAddSequence(#27'[O',0,kbF3);            {FreeBSD}
  DoAddSequence(#27'[P',0,kbF4);            {FreeBSD}
  DoAddSequence(#27'[Q',0,kbF5);            {FreeBSD}
  DoAddSequence(#27'[R',0,kbF6);            {FreeBSD}
  DoAddSequence(#27'[S',0,kbF7);            {FreeBSD}
  DoAddSequence(#27'[T',0,kbF8);            {FreeBSD}
  DoAddSequence(#27'[U',0,kbF9);            {FreeBSD}
  DoAddSequence(#27'[V',0,kbF10);           {FreeBSD}
  DoAddSequence(#27'[W',0,kbF11);           {FreeBSD}
  DoAddSequence(#27'[X',0,kbF12);           {FreeBSD}
  DoAddSequence(#27'[Z',0,kbShiftTab);

  DoAddSequence(#27'[1;2A',0,kbShiftUp);    {xterm}
  DoAddSequence(#27'[1;2B',0,kbShiftDown);  {xterm}
  DoAddSequence(#27'[1;2C',0,kbShiftRight); {xterm}
  DoAddSequence(#27'[1;2D',0,kbShiftLeft);  {xterm}
  DoAddSequence(#27'[1;2F',0,kbShiftEnd);   {xterm}
  DoAddSequence(#27'[1;2H',0,kbShiftHome);  {xterm}

  DoAddSequence(#27'[1;5A',0,kbCtrlUp);     {xterm}
  DoAddSequence(#27'[1;5B',0,kbCtrlDown);   {xterm}
  DoAddSequence(#27'[1;5C',0,kbCtrlRight);  {xterm}
  DoAddSequence(#27'[1;5D',0,kbCtrlLeft);   {xterm}
  DoAddSequence(#27'[1;5F',0,kbCtrlEnd);    {xterm}
  DoAddSequence(#27'[1;5H',0,kbCtrlHome);   {xterm}
  DoAddSequence(#27#27'[A',0,kbAltUp);
  DoAddSequence(#27#27'[B',0,kbAltDown);
  DoAddSequence(#27#27'[D',0,kbAltLeft);
  DoAddSequence(#27#27'[C',0,kbAltRight);
  DoAddSequence(#27#27'[5~',0,kbAltPgUp);
  DoAddSequence(#27#27'[6~',0,kbAltPgDn);
  DoAddSequence(#27#27'[4~',0,kbAltEnd);
  DoAddSequence(#27#27'[1~',0,kbAltHome);
  DoAddSequence(#27#27'[2~',0,kbAltIns);
  DoAddSequence(#27#27'[3~',0,kbAltDel);
  DoAddSequence(#27'OA',0,kbUp);            {xterm}
  DoAddSequence(#27'OB',0,kbDown);          {xterm}
  DoAddSequence(#27'OC',0,kbRight);         {xterm}
  DoAddSequence(#27'OD',0,kbLeft);          {xterm}
  DoAddSequence(#27'OF',0,kbHome);          {some xterm configurations}
  DoAddSequence(#27'OH',0,kbEnd);           {some xterm configurations}
  DoAddSequence(#27'OP',0,kbF1);            {vt100,gnome,konsole}
  DoAddSequence(#27'OQ',0,kbF2);            {vt100,gnome,konsole}
  DoAddSequence(#27'OR',0,kbF3);            {vt100,gnome,konsole}
  DoAddSequence(#27'OS',0,kbF4);            {vt100,gnome,konsole}
  DoAddSequence(#27'Ot',0,kbF5);            {vt100}
  DoAddSequence(#27'Ou',0,kbF6);            {vt100}
  DoAddSequence(#27'Ov',0,kbF7);            {vt100}
  DoAddSequence(#27'Ol',0,kbF8);            {vt100}
  DoAddSequence(#27'Ow',0,kbF9);            {vt100}
  DoAddSequence(#27'Ox',0,kbF10);           {vt100}
  DoAddSequence(#27'Oy',0,kbF11);           {vt100}
  DoAddSequence(#27'Oz',0,kbF12);           {vt100}
  DoAddSequence(#27'O2P',0,kbShiftF1);      {konsole,xterm}
  DoAddSequence(#27'O2Q',0,kbShiftF2);      {konsole,xterm}
  DoAddSequence(#27'O2R',0,kbShiftF3);      {konsole,xterm}
  DoAddSequence(#27'O2S',0,kbShiftF4);      {konsole,xterm}
  DoAddSequence(#27#27'OP',0,kbAltF1);
  DoAddSequence(#27#27'OQ',0,kbAltF2);
  DoAddSequence(#27#27'OR',0,kbAltF3);
  DoAddSequence(#27#27'OS',0,kbAltF4);
  DoAddSequence(#27#27'Ot',0,kbAltF5);
  DoAddSequence(#27#27'Ou',0,kbAltF6);
  DoAddSequence(#27#27'Ov',0,kbAltF7);
  DoAddSequence(#27#27'Ol',0,kbAltF8);
  DoAddSequence(#27#27'Ow',0,kbAltF9);
  DoAddSequence(#27#27'Ox',0,kbAltF10);
  DoAddSequence(#27#27'Oy',0,kbAltF11);
  DoAddSequence(#27#27'Oz',0,kbAltF12);
  DoAddSequence(#27#27'OA',0,kbAltUp);
  DoAddSequence(#27#27'OB',0,kbAltDown);
  DoAddSequence(#27#27'OC',0,kbAltRight);
  DoAddSequence(#27#27'OD',0,kbAltLeft);
  { xterm default values }
  { xterm alternate default values }
  { ignored sequences }
  DoAddSequence(#27'[?1;0c',0,0);
  DoAddSequence(#27'[?1l',0,0);
  DoAddSequence(#27'[?1h',0,0);
  DoAddSequence(#27'[?1;2c',0,0);
  DoAddSequence(#27'[?7l',0,0);
  DoAddSequence(#27'[?7h',0,0);
end;

function RawReadKey:char;
var
  fdsin    : tfdSet;
begin
  {Check Buffer first}
  if KeySend<>KeyPut then
   begin
     RawReadKey:=PopKey;
     exit;
   end;
  {Wait for Key}
  if not sysKeyPressed then
   begin
     fpFD_ZERO (fdsin);
     fpFD_SET (StdInputHandle,fdsin);
     fpSelect (StdInputHandle+1,@fdsin,nil,nil,nil);
   end;
  RawReadKey:=ttyRecvChar;
end;


function RawReadString : String;
var
  ch : char;
  fdsin : tfdSet;
  St : String;
begin
  St:=RawReadKey;
  fpFD_ZERO (fdsin);
  fpFD_SET (StdInputHandle,fdsin);
  Repeat
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     if SysKeyPressed then
       ch:=ttyRecvChar
     else
       ch:=#0;
     if ch<>#0 then
       St:=St+ch;
  Until ch=#0;
  RawReadString:=St;
end;


function ReadKey(var IsAlt : boolean):char;
var
  ch       : char;
  is_delay : boolean;
  fdsin    : tfdSet;
  store    : array [0..8] of char;
  arrayind : byte;
  NPT,NNPT : PTreeElement;
  procedure GenMouseEvent;
  var MouseEvent: TMouseEvent;
  begin
    Fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
    case ch of
      #32 : {left button pressed }
        MouseEvent.buttons:=1;
      #33 : {middle button pressed }
        MouseEvent.buttons:=2;
      #34 : { right button pressed }
        MouseEvent.buttons:=4;
      #35 : { no button pressed };
      end;
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.x:=Ord(ch)-ord(' ')-1;
     if inhead=intail then
      fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.y:=Ord(ch)-ord(' ')-1;
     if (MouseEvent.buttons<>0) then
       MouseEvent.action:=MouseActionDown
     else
       begin
         if (LastMouseEvent.Buttons<>0) and
            ((LastMouseEvent.X<>MouseEvent.X) or (LastMouseEvent.Y<>MouseEvent.Y)) then
           begin
             MouseEvent.Action:=MouseActionMove;
             MouseEvent.Buttons:=LastMouseEvent.Buttons;
             PutMouseEvent(MouseEvent);
             MouseEvent.Buttons:=0;
           end;
         MouseEvent.Action:=MouseActionUp;
       end;
     PutMouseEvent(MouseEvent);
     LastMouseEvent:=MouseEvent;
  end;

    procedure RestoreArray;
      var
        i : byte;
      begin
        for i:=0 to arrayind-1 do
          PushKey(store[i]);
      end;

begin
  IsAlt:=false;
{Check Buffer first}
  if KeySend<>KeyPut then
   begin
     ReadKey:=PopKey;
     exit;
   end;
{Wait for Key}
  if not sysKeyPressed then
   begin
     fpFD_ZERO (fdsin);
     fpFD_SET (StdInputHandle,fdsin);
     fpSelect (StdInputHandle+1,@fdsin,nil,nil,nil);
   end;
  ch:=ttyRecvChar;
  NPT:=RootTree[ord(ch)];
  if not assigned(NPT) then
    PushKey(ch)
  else
    begin
     fpFD_ZERO(fdsin);
     fpFD_SET(StdInputHandle,fdsin);
     store[0]:=ch;
     arrayind:=1;
      while assigned(NPT) and syskeypressed do
        begin
          if inhead=intail then
            fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
          ch:=ttyRecvChar;
          NNPT:=FindChild(ord(ch),NPT);
          if assigned(NNPT) then
            begin
              NPT:=NNPT;
              if NPT^.CanBeTerminal and
                 assigned(NPT^.SpecialHandler) then
                break;
            End;
          if ch<>#0 then
            begin
              store[arrayind]:=ch;
              inc(arrayind);
            end;
          if not assigned(NNPT) then
            begin
              if ch<>#0 then
                begin
                  { Put that unused char back into InBuf }
                  If InTail=0 then
                    InTail:=InSize-1
                  else
                    Dec(InTail);
                  InBuf[InTail]:=ch;
                end;
              break;
            end;
        end;
      if assigned(NPT) and NPT^.CanBeTerminal then
        begin
          if assigned(NPT^.SpecialHandler) then
            begin
              NPT^.SpecialHandler;
              PushExt(0);
            end
          else if NPT^.CharValue<>0 then
            PushKey(chr(NPT^.CharValue))
          else if NPT^.ScanValue<>0 then
            PushExt(NPT^.ScanValue);
        end
      else
        RestoreArray;
   end
{$ifdef logging}
       writeln(f);
{$endif logging}
    ;
  ReadKey:=PopKey;
End;

{$ifdef linux}
function ShiftState:byte;
var
  arg,
  shift : longint;
begin
  shift:=0;
  arg:=6;
  if fpioctl(StdInputHandle,TIOCLINUX,@arg)=0 then
   begin
     if (arg and 8)<>0 then
      shift:=kbAlt;
     if (arg and 4)<>0 then
      inc(shift,kbCtrl);
     { 2 corresponds to AltGr so set both kbAlt and kbCtrl PM }
     if (arg and 2)<>0 then
      shift:=shift or (kbAlt or kbCtrl);
     if (arg and 1)<>0 then
      inc(shift,kbShift);
   end;
  ShiftState:=shift;
end;

procedure force_linuxtty;

var s:string[15];
{    st:stat;}
    handle:sizeint;
    thistty:string;

begin
  is_console:=false;
  if vcs_device<>-1 then
    begin
       { running on a tty, find out whether locally or remotely }
      thistty:=ttyname(stdinputhandle);
      if (copy(thistty,1,8)<>'/dev/tty') or not (thistty[9] in ['0'..'9']) then
        begin
          {Running from Midnight Commander or something... Bypass it.}
          str(vcs_device,s);
          handle:=fpopen('/dev/tty'+s,O_RDWR);
          fpioctl(stdinputhandle,TIOCNOTTY,nil);
          {This will currently only work when the user is root :(}
          fpioctl(handle,TIOCSCTTY,nil);
          if errno<>0 then
            exit;
          fpclose(stdinputhandle);
          fpclose(stdoutputhandle);
          fpclose(stderrorhandle);
          fpdup2(handle,stdinputhandle);
          fpdup2(handle,stdoutputhandle);
          fpdup2(handle,stderrorhandle);
          fpclose(handle);
        end;
      is_console:=true;
    end;
end;
{$endif linux}

{ Exported functions }

procedure SysInitKeyboard;
begin
  SetRawMode(true);
{$ifdef logging}
     assign(f,'keyboard.log');
     rewrite(f);
{$endif logging}
{$ifdef linux}
  force_linuxtty;
  prepare_patching;
  patchkeyboard;
  if is_console then
    install_vt_handler
  else
    begin
{$endif}
      { default for Shift prefix is ^ A}
      if ShiftPrefix = 0 then
        ShiftPrefix:=1;
      {default for Alt prefix is ^Z }
      if AltPrefix=0 then
        AltPrefix:=26;
      { default for Ctrl Prefix is ^W }
      if CtrlPrefix=0 then
        CtrlPrefix:=23;
{$ifdef linux}
    end;
{$endif}
  LoadDefaultSequences;
{  LoadTerminfoSequences;}
end;


procedure SysDoneKeyboard;
begin
{$ifdef linux}
  unpatchkeyboard;
{$endif linux}
  SetRawMode(false);

  FreeTree;
{$ifdef logging}
  close(f);
{$endif logging}
end;


function SysGetKeyEvent: TKeyEvent;

  function EvalScan(b:byte):byte;
  const
    DScan:array[0..31] of byte = (
      $39, $02, $28, $04, $05, $06, $08, $28,
      $0A, $0B, $09, $0D, $33, $0C, $34, $35,
      $0B, $02, $03, $04, $05, $06, $07, $08,
      $09, $0A, $27, $27, $33, $0D, $34, $35);
   LScan:array[0..31] of byte = (
      $29, $1E, $30, $2E, $20, $12, $21, $22,
      $23, $17, $24, $25, $26, $32, $31, $18,
      $19, $10, $13, $1F, $14, $16, $2F, $11,
      $2D, $15, $2C, $1A, $2B, $1B, $29, $0C);
  begin
    if (b and $E0)=$20  { digits / leters } then
     EvalScan:=DScan[b and $1F]
    else
     case b of
      $08:EvalScan:=$0E; { backspace }
      $09:EvalScan:=$0F; { TAB }
      $0D:EvalScan:=$1C; { CR }
      $1B:EvalScan:=$01; { esc }
      $40:EvalScan:=$03; { @ }
      $5E:EvalScan:=$07; { ^ }
      $60:EvalScan:=$29; { ` }
     else
      EvalScan:=LScan[b and $1F];
     end;
  end;

  function EvalScanZ(b:byte):byte;
  begin
    EvalScanZ:=b;
    if b in [$3B..$44] { F1..F10 -> Alt-F1..Alt-F10} then
     EvalScanZ:=b+$2D;
  end;

const
   {kbHome, kbUp, kbPgUp,Missing, kbLeft,
    kbCenter, kbRight, kbAltGrayPlus, kbend,
    kbDown, kbPgDn, kbIns, kbDel }
  CtrlArrow : array [kbHome..kbDel] of byte =
   {($77,$8d,$84,$8e,$73,$8f,$74,$90,$75,$91,$76);}
   (kbCtrlHome,kbCtrlUp,kbCtrlPgUp,kbNoKey,kbCtrlLeft,
    kbCtrlCenter,kbCtrlRight,kbAltGrayPlus,kbCtrlEnd,
    kbCtrlDown,kbCtrlPgDn,kbCtrlIns,kbCtrlDel);
  AltArrow : array [kbHome..kbDel] of byte =
   (kbAltHome,kbAltUp,kbAltPgUp,kbNoKey,kbAltLeft,
    kbCenter,kbAltRight,kbAltGrayPlus,kbAltEnd,
    kbAltDown,kbAltPgDn,kbAltIns,kbAltDel);
  ShiftArrow : array [kbShiftUp..kbShiftEnd] of byte =
   (kbUp,kbLeft,kbRight,kbDown,kbHome,kbEnd);

var
  MyScan:byte;
  MyChar : char;
  EscUsed,AltPrefixUsed,CtrlPrefixUsed,ShiftPrefixUsed,IsAlt,Again : boolean;
  SState:byte;

begin {main}
  MyChar:=Readkey(IsAlt);
  MyScan:=ord(MyChar);
{$ifdef linux}
  if is_console then
    SState:=ShiftState
  else
{$endif}
    Sstate:=0;
  CtrlPrefixUsed:=false;
  AltPrefixUsed:=false;
  ShiftPrefixUsed:=false;
  EscUsed:=false;
  if IsAlt then
    SState:=SState or kbAlt;
  repeat
    again:=false;
    if Mychar=#0 then
      begin
        MyScan:=ord(ReadKey(IsAlt));
        if myscan=$01 then
          mychar:=#27;
        { Handle Ctrl-<x>, but not AltGr-<x> }
        if ((SState and kbCtrl)<>0) and ((SState and kbAlt) = 0)  then
          case MyScan of
            kbHome..kbDel : { cArrow }
              MyScan:=CtrlArrow[MyScan];
            kbF1..KbF10 : { cF1-cF10 }
              MyScan:=MyScan+kbCtrlF1-kbF1;
            kbF11..KbF12 : { cF11-cF12 }
              MyScan:=MyScan+kbCtrlF11-kbF11;
          end
        { Handle Alt-<x>, but not AltGr }
        else if ((SState and kbAlt)<>0) and ((SState and kbCtrl) = 0) then
          case MyScan of
            kbHome..kbDel : { AltArrow }
              MyScan:=AltArrow[MyScan];
            kbF1..KbF10 : { aF1-aF10 }
              MyScan:=MyScan+kbAltF1-kbF1;
            kbF11..KbF12 : { aF11-aF12 }
              MyScan:=MyScan+kbAltF11-kbF11;
          end
        else if (SState and kbShift)<>0 then
          case MyScan of
            kbIns: MyScan:=kbShiftIns;
            kbDel: MyScan:=kbShiftDel;
            kbF1..KbF10 : { sF1-sF10 }
              MyScan:=MyScan+kbShiftF1-kbF1;
            kbF11..KbF12 : { sF11-sF12 }
              MyScan:=MyScan+kbShiftF11-kbF11;
          end;
        if myscan in [kbShiftUp..kbShiftEnd] then
          begin
            myscan:=ShiftArrow[myscan];
            sstate:=sstate or kbshift;
          end;
        if myscan=kbAltBack then
          sstate:=sstate or kbalt;
        if (MyChar<>#0) or (MyScan<>0) or (SState<>0) then
          SysGetKeyEvent:=$3000000 or ord(MyChar) or (MyScan shl 8) or (SState shl 16)
        else
          SysGetKeyEvent:=0;
        exit;
      end
    else if MyChar=#27 then
      begin
        if EscUsed then
          SState:=SState and not kbAlt
        else
          begin
            SState:=SState or kbAlt;
            Again:=true;
            EscUsed:=true;
          end;
      end
    else if (AltPrefix<>0) and (MyChar=chr(AltPrefix)) then
      begin { ^Z - replace Alt for Linux OS }
        if AltPrefixUsed then
          begin
            SState:=SState and not kbAlt;
          end
        else
          begin
            AltPrefixUsed:=true;
            SState:=SState or kbAlt;
            Again:=true;
          end;
      end
    else if (CtrlPrefix<>0) and (MyChar=chr(CtrlPrefix)) then
      begin
        if CtrlPrefixUsed then
          SState:=SState and not kbCtrl
        else
          begin
            CtrlPrefixUsed:=true;
            SState:=SState or kbCtrl;
            Again:=true;
          end;
      end
    else if (ShiftPrefix<>0) and (MyChar=chr(ShiftPrefix)) then
      begin
        if ShiftPrefixUsed then
          SState:=SState and not kbShift
        else
          begin
            ShiftPrefixUsed:=true;
            SState:=SState or kbShift;
            Again:=true;
          end;
      end;
    if not again then
      begin
        MyScan:=EvalScan(ord(MyChar));
        if ((SState and kbAlt)<>0) and ((SState and kbCtrl) = 0) then
          begin
            if MyScan in [$02..$0D] then
              inc(MyScan,$76);
            MyChar:=chr(0);
          end
        else if (SState and kbShift)<>0 then
          if MyChar=#9 then
            begin
              MyChar:=#0;
              MyScan:=kbShiftTab;
            end;
      end
    else
      begin
        MyChar:=Readkey(IsAlt);
        MyScan:=ord(MyChar);
        if IsAlt then
          SState:=SState or kbAlt;
      end;
    until not Again;
  if (MyChar<>#0) or (MyScan<>0) or (SState<>0) then
    SysGetKeyEvent:=$3000000 or ord(MyChar) or (MyScan shl 8) or (SState shl 16)
  else
    SysGetKeyEvent:=0;
end;


function SysPollKeyEvent: TKeyEvent;
var
  KeyEvent : TKeyEvent;
begin
  if keypressed then
    begin
      KeyEvent:=SysGetKeyEvent;
      PutKeyEvent(KeyEvent);
      SysPollKeyEvent:=KeyEvent
    end
  else
    SysPollKeyEvent:=0;
end;


function SysGetShiftState  : Byte;
begin
{$ifdef linux}
  if is_console then
    SysGetShiftState:=ShiftState
  else
{$else}
    SysGetShiftState:=0;
{$endif}
end;


procedure RestoreStartMode;
begin
  TCSetAttr(1,TCSANOW,StartTio);
end;


const
  SysKeyboardDriver : TKeyboardDriver = (
    InitDriver : @SysInitKeyBoard;
    DoneDriver : @SysDoneKeyBoard;
    GetKeyevent : @SysGetKeyEvent;
    PollKeyEvent : @SysPollKeyEvent;
    GetShiftState : @SysGetShiftState;
    TranslateKeyEvent : Nil;
    TranslateKeyEventUnicode : Nil;
  );

begin
  SetKeyBoardDriver(SysKeyBoardDriver);
  TCGetAttr(1,StartTio);
end.
