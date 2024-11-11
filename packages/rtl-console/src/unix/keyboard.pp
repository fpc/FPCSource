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
{$IFNDEF FPC_DOTTEDUNITS}
unit keyboard;
{$ENDIF FPC_DOTTEDUNITS}

{$inline on}

{*****************************************************************************}
                                  interface
{*****************************************************************************}

{$i keybrdh.inc}

const
  AltPrefix : byte = 0;
  ShiftPrefix : byte = 0;
  CtrlPrefix : byte = 0;

type
  Tprocedure = procedure;

  PTreeElement = ^TTreeElement;
  TTreeElement = record
    Next,Parent,Child :  PTreeElement;
    CanBeTerminal : boolean;
    AnsiChar : byte;
    ScanValue : byte;
    CharValue : byte;
    ShiftValue : TEnhancedShiftState;
    SpecialHandler : Tprocedure;
  end;

function RawReadKey:AnsiChar;
function RawReadString : ShortString;
function KeyPressed : Boolean;
procedure AddSequence(const St : ShortString; AChar,AScan :byte);inline;
function FindSequence(const St : ShortString;var AChar, Ascan : byte) : boolean;
procedure RestoreStartMode;

function AddSpecialSequence(const St : Shortstring;Proc : Tprocedure) : PTreeElement; platform;


{*****************************************************************************}
                               implementation
{*****************************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Console.Mouse,  System.Strings,System.Console.Unixkvmbase,
  UnixApi.TermIO,UnixApi.Base
  {$ifdef Linux},LinuxApi.Vcs{$endif};
{$ELSE FPC_DOTTEDUNITS}
uses
  Mouse,  Strings,unixkvmbase,
  termio,baseUnix
  {$ifdef linux},linuxvcs{$endif};
{$ENDIF FPC_DOTTEDUNITS}

{$i keyboard.inc}

var OldIO,StartTio : TermIos;
    Utf8KeyboardInputEnabled: Boolean;
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
  KeyBuffer : Array[0..KeyBufferSize-1] of TEnhancedKeyEvent;
  KeyPut,
  KeySend   : longint;

  PendingEnhancedKeyEvent: TEnhancedKeyEvent;

{ Buffered Input routines }
const
  InSize=256;
var
  InBuf  : array [0..InSize-1] of AnsiChar;
{  InCnt,}
  InHead,
  InTail : longint;

{$i keyscan.inc}

var kitty_keys_yes : boolean;  {one of two have to be true}
    kitty_keys_no : boolean;
    isKittyKeys : boolean;

const
      kbAltCenter = kbCtrlCenter;  {there is no true DOS scancode for Alt+Center (Numpad "5") reusing Ctrl+Center}

      double_esc_hack_enabled : boolean = false;

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

var Tio:Termios;

begin
  TCGetAttr(0,Tio);
  if b then
   begin
     {Standard output now needs #13#10.}
     settextlineending(output,#13#10);
     OldIO:=Tio;
     CFMakeRaw(Tio);
   end
  else
    begin
      Tio := OldIO;
      {Standard output normally needs just a linefeed.}
      settextlineending(output,#10);
    end;
  TCsetattr(0,TCSANOW,Tio);
end;

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
    kb_string:array[0..511] of AnsiChar;
  end;
  vt_mode=packed record
    mode,          {vt mode}
    waitv:byte;    {if set, hang on writes if not active}
    relsig,        {signal to raise on release req}
    acqsig,        {signal to raise on acquisition}
    frsig:word;    {unused (set to 0)}
 end;

const
  kbdchange:array[0..35] of chgentry=(
    {This prevents the alt+function keys from switching consoles.
     We code the F1..F12 sequences into ALT+F1..ALT+F12, we check
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
    (tab:8; idx:$57; oldtab:0; oldidx:$57; oldval:0; newval:0),
    (tab:8; idx:$58; oldtab:0; oldidx:$58; oldval:0; newval:0),
    {This prevents the shift+function keys outputting strings, so
     the kernel will send the codes for the non-shifted function
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
    (tab:1; idx:$57; oldtab:0; oldidx:$57; oldval:0; newval:0),
    (tab:1; idx:$58; oldtab:0; oldidx:$58; oldval:0; newval:0),
    {This maps ctrl+function keys outputting strings to the regular
     F1..F12 keys also, because they no longer produce an ASCII
     output at all in most modern linux keymaps. We obtain the
     shift state separately.}
    (tab:4; idx:$3b; oldtab:0; oldidx:$3b; oldval:0; newval:0),
    (tab:4; idx:$3c; oldtab:0; oldidx:$3c; oldval:0; newval:0),
    (tab:4; idx:$3d; oldtab:0; oldidx:$3d; oldval:0; newval:0),
    (tab:4; idx:$3e; oldtab:0; oldidx:$3e; oldval:0; newval:0),
    (tab:4; idx:$3f; oldtab:0; oldidx:$3f; oldval:0; newval:0),
    (tab:4; idx:$40; oldtab:0; oldidx:$40; oldval:0; newval:0),
    (tab:4; idx:$41; oldtab:0; oldidx:$41; oldval:0; newval:0),
    (tab:4; idx:$42; oldtab:0; oldidx:$42; oldval:0; newval:0),
    (tab:4; idx:$43; oldtab:0; oldidx:$43; oldval:0; newval:0),
    (tab:4; idx:$44; oldtab:0; oldidx:$44; oldval:0; newval:0),
    (tab:4; idx:$57; oldtab:0; oldidx:$57; oldval:0; newval:0),
    (tab:4; idx:$58; oldtab:0; oldidx:$58; oldval:0; newval:0)
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

var entry : kbentry;
    i:longint;

begin
  for i:=low(kbdchange) to high(kbdchange) do
   with kbdchange[i] do
     begin
       entry.kb_table:=tab;
       entry.kb_index:=idx;
       fpIoctl(stdinputhandle,KDGKBENT,@entry);
       oldval:=entry.kb_value;
       entry.kb_table:=oldtab;
       entry.kb_index:=oldidx;
       fpioctl(stdinputhandle,KDGKBENT,@entry);
       newval:=entry.kb_value;
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
  entry : kbentry;
  sentry : kbsentry;
  i:longint;
begin
  fpIoctl(stdinputhandle,KDGKBMETA,@oldmeta);
  meta:=K_ESCPREFIX;
  fpIoctl(stdinputhandle,KDSKBMETA,@meta);
  for i:=low(kbdchange) to high(kbdchange) do
    with kbdchange[i] do
      begin
        entry.kb_table:=tab;
        entry.kb_index:=idx;
        entry.kb_value:=newval;
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
  entry : kbentry;
  i : longint;
begin
  if oldmeta in [K_ESCPREFIX,K_METABIT] then
    fpioctl(stdinputhandle,KDSKBMETA,@oldmeta);
  for i:=low(kbdchange) to high(kbdchange) do
    with kbdchange[i] do
      begin
        entry.kb_table:=tab;
        entry.kb_index:=idx;
        entry.kb_value:=oldval;
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

function ttyRecvChar:AnsiChar;

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

{ returns an already read character back into InBuf }
procedure PutBackIntoInBuf(ch: AnsiChar);
begin
  If InTail=0 then
    InTail:=InSize-1
  else
    Dec(InTail);
  InBuf[InTail]:=ch;
end;

procedure PushKey(const Ch:TEnhancedKeyEvent);
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


function PopKey:TEnhancedKeyEvent;
begin
  If KeyPut<>KeySend Then
   begin
     PopKey:=KeyBuffer[KeySend];
     Inc(KeySend);
     If KeySend>=KeyBufferSize Then
      KeySend:=0;
   End
  Else
   PopKey:=NilEnhancedKeyEvent;
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

  procedure GenFakeReleaseEvent(MouseEvent : TMouseEvent);
  begin
    MouseEvent.action := MouseActionUp;
    MouseEvent.buttons := 0;
    PutMouseEvent(MouseEvent);
  end;

  procedure GenMouseEvent;
  { format: CSI M char1 charX charY
       char1 - button nr and state
       charX - mouse X (if multi byte format then 1 or 2 chars)
       charY - mouse Y (if multi byte format then 1 or 2 chars)
  }
  var MouseEvent: TMouseEvent;
      ch : AnsiChar;
      fdsin : tfdSet;
      buttonval:byte;
      x,y,x1 : word;
      notMultiByte : boolean;
      NeedMouseRelease:boolean;
      addButtMove : byte;
  begin
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle,fdsin);
{    Fillchar(MouseEvent,SizeOf(TMouseEvent),#0);}
    MouseEvent.buttons:=0;
    if inhead=intail then
      fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
    ch:=ttyRecvChar;
    buttonval:=byte(ch);
    if ch in [#$c2,#$c3] then
    begin
      {xterm multibyte}
      addButtMove:=(byte(ch) and 1) shl 6;
      if inhead=intail then
        fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
      ch:=ttyRecvChar;
      buttonval:=byte(ch) or addButtMove;
    end;
    NeedMouseRelease:=false;
    { Other bits are used for Shift, Meta and Ctrl modifiers PM }
    buttonval:=buttonval and %11100111;
    {bits 0..1: button status
     bit  5   : mouse movement while button down.
     bit  6   : interpret button 1 as button 4
                interpret button 2 as button 5}
    case buttonval of
      %00100000,%01000000 : {left button pressed,moved}
        MouseEvent.buttons:=1;
      %00100001,%01000001 : {middle button pressed,moved }
        MouseEvent.buttons:=2;
      %00100010,%01000010 : { right button pressed,moved }
        MouseEvent.buttons:=4;
      %00100011,%01000011 : { no button pressed,moved }
        MouseEvent.buttons:=0;
      %01100000: { button 4 pressed }
          MouseEvent.buttons:=MouseButton4;
      %10000000: { rxvt - button 4 move }
          MouseEvent.buttons:=0;  {rxvt does not release button keeps moving it, fake as no button press move}
      %01100001: { button 5 pressed }
          MouseEvent.buttons:=MouseButton5;
      %10000001: { rxvt - button 5 move }
          MouseEvent.buttons:=0;
      %10100000,%11000000 : { xterm - button 6 pressed,moved }
          MouseEvent.buttons:=MouseXButton1;
      %01100100 : { rxvt - button 6 pressed, have to add fake release }
          begin MouseEvent.buttons:=MouseXButton1; NeedMouseRelease:=true; end;
      %10000100 : { rxvt - button 6 move }
          MouseEvent.buttons:=0;
      %10100001,%11000001 : { xterm - button 7 pressed,moved }
          MouseEvent.buttons:=MouseXButton2;
      %01100101 : { rxvt - button 7 pressed, have to add fake release }
          begin MouseEvent.buttons:=MouseXButton2; NeedMouseRelease:=true; end;
      %10000101: { rxvt - button 7 move }
          MouseEvent.buttons:=0;
    end;
     notMultiByte:=false;
     {mouse X}
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     x:=byte(ch);
     x1:=x;
     {mouse Y}
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     y:=byte(ch);
     {decide if this is a single byte or a multi byte mouse report format}
     if (x in [127..193]) or (x=0) then
       notMultiByte:=true
     else
     if x >= 194 then
     begin
       if ch in [#$80..#$bf] then  {probably multibyte}
         x1:=128+(byte(ch)-128)+(x-194)*($bf-$80+1)
       else notMultiByte:=true;
     end;
     if y < 128 then
       notMultiByte:=true;
     {probability is high for multi byte format and we have extra character in line to read}
     if not notMultiByte and sysKeyPressed then
     begin
       if inhead=intail then
         fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
       ch:=ttyRecvChar;
       if ch > ' ' then
       begin
         {we are sure, it is a multi byte mouse report format}
         x:=x1; {new mouse X}
         y:=byte(ch); {new mouse Y}
         if (y <> 0 ) and sysKeyPressed and (y >= 194) then
         begin
           if inhead=intail then
             fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
           ch:=ttyRecvChar;
           y:=128+(byte(ch)-128)+(y-194)*($bf-$80+1); {multibyte mouse Y}
         end;
       end else PutBackIntoInBuf(ch);
     end;
     if (x=0) or (y=0) then exit; {single byte format hit its limts, no mouse event}
     MouseEvent.x:=x-32-1;
     MouseEvent.y:=y-32-1;
     mouseevent.action:=MouseActionMove;
     if (lastmouseevent.buttons=0) and (mouseevent.buttons<>0) then
       MouseEvent.action:=MouseActionDown;
     if (lastmouseevent.buttons<>0) and (mouseevent.buttons=0) then
       MouseEvent.action:=MouseActionUp;
(*
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
*)
     PutMouseEvent(MouseEvent);
     if (MouseEvent.buttons and (8+16)) <> 0 then // 'M' escape sequence cannot map button 4&5 release, so fake one.
       GenFakeReleaseEvent(MouseEvent);
     if NeedMouseRelease then
       GenFakeReleaseEvent(MouseEvent);
{$ifdef DebugMouse}
     if MouseEvent.Action=MouseActionDown then
       Write(system.stderr,'Button down : ')
     else
       Write(system.stderr,'Button up : ');
     Writeln(system.stderr,'buttons = ',MouseEvent.Buttons,' (',MouseEvent.X,',',MouseEvent.Y,')');
{$endif DebugMouse}
     LastMouseEvent:=MouseEvent;
  end;


  { The Extended/SGR 1006 mouse protocol, supported by xterm 277 and newer.
    Message format: Esc [<0;123;456M  - mouse button press
                or: Esc [<0;123;456m  - mouse button release
    Advantages:
      - can report X and Y coordinates larger than 223
      - mouse release event informs us of *which* mouse button was released, so
        we can track buttons more accurately
      - messages use a different prefix (Esc [< instead of Esc [M) than the
        regular mouse event messages, so there's no need to detect if the
        terminal supports it - we can always try to enable it and then be
        prepared to handle both types of messages }
  procedure GenMouseEvent_ExtendedSGR1006;
  var MouseEvent: TMouseEvent;
      ch : AnsiChar;
      fdsin : tfdSet;
      buttonval: LongInt;
      tempstr: shortstring;
      code: LongInt;
      X, Y: LongInt;
      ButtonMask: Word;
  begin
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle,fdsin);

    { read buttonval }
    tempstr:='';
    repeat
      if inhead=intail then
        fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
      ch:=ttyRecvChar;
      if (ch>='0') and (ch<='9') then
        tempstr:=tempstr+ch
      else if ch<>';' then
        exit;
    until ch=';';
    Val(tempstr,buttonval,code);

    { read X }
    tempstr:='';
    repeat
      if inhead=intail then
        fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
      ch:=ttyRecvChar;
      if (ch>='0') and (ch<='9') then
        tempstr:=tempstr+ch
      else if ch<>';' then
        exit;
    until ch=';';
    Val(tempstr,X,code);

    { read Y }
    tempstr:='';
    repeat
      if inhead=intail then
        fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
      ch:=ttyRecvChar;
      if (ch>='0') and (ch<='9') then
        tempstr:=tempstr+ch
      else if (ch<>'M') and (ch<>'m') then
        exit;
    until (ch='M') or (ch='m');
    Val(tempstr,Y,code);

{$ifdef DebugMouse}
    Writeln(System.StdErr, 'SGR1006:', buttonval:3, X:5, Y:5, ' ', ch);
{$endif DebugMouse}

    { let's range check X and Y just in case }
    if (X<(Low(MouseEvent.X)+1)) or (X>(High(MouseEvent.X)+1)) then
      exit;
    if (Y<(Low(MouseEvent.Y)+1)) or (Y>(High(MouseEvent.Y)+1)) then
      exit;
    case buttonval and (67 or 128) of
      0 : {left button press}
        ButtonMask:=1;
      1 : {middle button pressed }
        ButtonMask:=2;
      2 : { right button pressed }
        ButtonMask:=4;
      3 : { no button pressed }
        ButtonMask:=0;
      64: { button 4 pressed }
        ButtonMask:=8;
      65: { button 5 pressed }
        ButtonMask:=16;
      128: { button browse back }
        ButtonMask:=MouseXButton1;
      129: { button browse forward }
        ButtonMask:=MouseXButton2;
    end;
    MouseEvent.X:=X-1;
    MouseEvent.Y:=Y-1;
    if (buttonval and 32)<>0 then
    begin
      MouseEvent.Action:=MouseActionMove;
      MouseEvent.Buttons:=LastMouseEvent.Buttons;
    end
    else
    begin
      if ch='M' then
      begin
        MouseEvent.Action:=MouseActionDown;
        MouseEvent.Buttons:=LastMouseEvent.Buttons or ButtonMask;
      end
      else
      begin
        MouseEvent.Action:=MouseActionUp;
        MouseEvent.Buttons:=LastMouseEvent.Buttons and not ButtonMask;
      end;
    end;
    PutMouseEvent(MouseEvent);
    if (ButtonMask and (8+16)) <> 0 then // 'M' escape sequence cannot map button 4&5 release, so fake one.
    begin
      MouseEvent.Action:=MouseActionUp;
      MouseEvent.Buttons:=LastMouseEvent.Buttons and not ButtonMask;
      PutMouseEvent(MouseEvent);
    end;
    LastMouseEvent:=MouseEvent;
  end;

var roottree:array[AnsiChar] of PTreeElement;

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

var i:AnsiChar;

begin
  for i:=low(roottree) to high(roottree) do
    begin
      FreeElement(RootTree[i]);
      roottree[i]:=nil;
    end;
end;

function NewPTree(ch : byte;Pa : PTreeElement) : PTreeElement;
begin
  newPtree:=allocmem(sizeof(Ttreeelement));
  newPtree^.AnsiChar:=ch;
  newPtree^.Parent:=Pa;
  if Assigned(Pa) and (Pa^.Child=nil) then
    Pa^.Child:=newPtree;
end;

function DoAddSequence(const St : shortstring; AChar,AScan :byte; const AShift: TEnhancedShiftState) : PTreeElement;
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
  CurPTree:=RootTree[st[1]];
  if CurPTree=nil then
    begin
      CurPTree:=NewPTree(ord(st[1]),nil);
      RootTree[st[1]]:=CurPTree;
    end;
  for i:=2 to Length(St) do
    begin
      NPT:=CurPTree^.Child;
      c:=ord(St[i]);
      if NPT=nil then
        NPT:=NewPTree(c,CurPTree);
      CurPTree:=nil;
      while assigned(NPT) and (NPT^.AnsiChar<c) do
        begin
          CurPTree:=NPT;
          NPT:=NPT^.Next;
        end;

      if assigned(NPT) and (NPT^.AnsiChar=c) then
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
            Writeln(system.stderr,'AnsiChar was ',chr(CharValue),' now ',chr(AChar));
{$endif DEBUG}
          ScanValue:=AScan;
          CharValue:=AChar;
          ShiftValue:=AShift;
        end;
    end
  else with CurPTree^ do
    begin
      CanBeTerminal:=True;
      ScanValue:=AScan;
      CharValue:=AChar;
      ShiftValue:=AShift;
    end;
  DoAddSequence:=CurPTree;
end;


procedure AddSequence(const St : shortstring; AChar,AScan :byte);inline;
begin
  DoAddSequence(St,AChar,AScan,[]);
end;

{ Returns the Child that as c as AnsiChar if it exists }
function FindChild(c : byte;Root : PTreeElement) : PTreeElement;
var
  NPT : PTreeElement;
begin
  NPT:=Root^.Child;
  while assigned(NPT) and (NPT^.AnsiChar<c) do
    NPT:=NPT^.Next;
  if assigned(NPT) and (NPT^.AnsiChar=c) then
    FindChild:=NPT
  else
    FindChild:=nil;
end;

function AddSpecialSequence(const St : shortstring;Proc : Tprocedure) : PTreeElement;
var
  NPT : PTreeElement;
begin
  NPT:=DoAddSequence(St,0,0,[]);
  NPT^.SpecialHandler:=Proc;
  AddSpecialSequence:=NPT;
end;

function FindSequence(const St : shortstring;var AChar,AScan :byte) : boolean;
var
  NPT : PTreeElement;
  i,p : byte;
begin
  FindSequence:=false;
  AChar:=0;
  AScan:=0;
  if St='' then
    exit;
  p:=1;
  {This is a distusting hack for certain even more disgusting xterms: Some of
   them send two escapes for an alt-key. If we wouldn't do this, we would need
   to put a lot of entries twice in the table.}
  if double_esc_hack_enabled and (st[1]=#27) and (st[2]='#27') and
     (st[3] in ['a'..'z','A'..'Z','0'..'9','-','+','_','=']) then
    inc(p);
  NPT:=RootTree[St[p]];

  if npt<>nil then
    begin
      for i:=p+1 to Length(St) do
        begin
          NPT:=FindChild(ord(St[i]),NPT);
          if NPT=nil then
            exit;
        end;
      if NPT^.CanBeTerminal then
        begin
          FindSequence:=true;
          AScan:=NPT^.ScanValue;
          AChar:=NPT^.CharValue;
        end;
    end;
end;

type  key_sequence=packed record
        AnsiChar:0..127;
        scan:byte;
        shift:TEnhancedShiftState;
        st:string[10];
      end;

const key_sequences:array[0..425] of key_sequence=(
       (AnsiChar:0;scan:$39;shift:[essCtrl];st:#0),         { xterm, Ctrl+Space }
       (AnsiChar:0;scan:kbAltA;shift:[essAlt];st:#27'A'),
       (AnsiChar:0;scan:kbAltA;shift:[essAlt];st:#27'a'),
       (AnsiChar:0;scan:kbAltB;shift:[essAlt];st:#27'B'),
       (AnsiChar:0;scan:kbAltB;shift:[essAlt];st:#27'b'),
       (AnsiChar:0;scan:kbAltC;shift:[essAlt];st:#27'C'),
       (AnsiChar:0;scan:kbAltC;shift:[essAlt];st:#27'c'),
       (AnsiChar:0;scan:kbAltD;shift:[essAlt];st:#27'D'),
       (AnsiChar:0;scan:kbAltD;shift:[essAlt];st:#27'd'),
       (AnsiChar:0;scan:kbAltE;shift:[essAlt];st:#27'E'),
       (AnsiChar:0;scan:kbAltE;shift:[essAlt];st:#27'e'),
       (AnsiChar:0;scan:kbAltF;shift:[essAlt];st:#27'F'),
       (AnsiChar:0;scan:kbAltF;shift:[essAlt];st:#27'f'),
       (AnsiChar:0;scan:kbAltG;shift:[essAlt];st:#27'G'),
       (AnsiChar:0;scan:kbAltG;shift:[essAlt];st:#27'g'),
       (AnsiChar:0;scan:kbAltH;shift:[essAlt];st:#27'H'),
       (AnsiChar:0;scan:kbAltH;shift:[essAlt];st:#27'h'),
       (AnsiChar:0;scan:kbAltI;shift:[essAlt];st:#27'I'),
       (AnsiChar:0;scan:kbAltI;shift:[essAlt];st:#27'i'),
       (AnsiChar:0;scan:kbAltJ;shift:[essAlt];st:#27'J'),
       (AnsiChar:0;scan:kbAltJ;shift:[essAlt];st:#27'j'),
       (AnsiChar:0;scan:kbAltK;shift:[essAlt];st:#27'K'),
       (AnsiChar:0;scan:kbAltK;shift:[essAlt];st:#27'k'),
       (AnsiChar:0;scan:kbAltL;shift:[essAlt];st:#27'L'),
       (AnsiChar:0;scan:kbAltL;shift:[essAlt];st:#27'l'),
       (AnsiChar:0;scan:kbAltM;shift:[essAlt];st:#27'M'),
       (AnsiChar:0;scan:kbAltM;shift:[essAlt];st:#27'm'),
       (AnsiChar:0;scan:kbAltN;shift:[essAlt];st:#27'N'),
       (AnsiChar:0;scan:kbAltN;shift:[essAlt];st:#27'n'),
       (AnsiChar:0;scan:kbAltO;shift:[essAlt];st:#27'O'),
       (AnsiChar:0;scan:kbAltO;shift:[essAlt];st:#27'o'),
       (AnsiChar:0;scan:kbAltP;shift:[essAlt];st:#27'P'),
       (AnsiChar:0;scan:kbAltP;shift:[essAlt];st:#27'p'),
       (AnsiChar:0;scan:kbAltQ;shift:[essAlt];st:#27'Q'),
       (AnsiChar:0;scan:kbAltQ;shift:[essAlt];st:#27'q'),
       (AnsiChar:0;scan:kbAltR;shift:[essAlt];st:#27'R'),
       (AnsiChar:0;scan:kbAltR;shift:[essAlt];st:#27'r'),
       (AnsiChar:0;scan:kbAltS;shift:[essAlt];st:#27'S'),
       (AnsiChar:0;scan:kbAltS;shift:[essAlt];st:#27's'),
       (AnsiChar:0;scan:kbAltT;shift:[essAlt];st:#27'T'),
       (AnsiChar:0;scan:kbAltT;shift:[essAlt];st:#27't'),
       (AnsiChar:0;scan:kbAltU;shift:[essAlt];st:#27'U'),
       (AnsiChar:0;scan:kbAltU;shift:[essAlt];st:#27'u'),
       (AnsiChar:0;scan:kbAltV;shift:[essAlt];st:#27'V'),
       (AnsiChar:0;scan:kbAltV;shift:[essAlt];st:#27'v'),
       (AnsiChar:0;scan:kbAltW;shift:[essAlt];st:#27'W'),
       (AnsiChar:0;scan:kbAltW;shift:[essAlt];st:#27'w'),
       (AnsiChar:0;scan:kbAltX;shift:[essAlt];st:#27'X'),
       (AnsiChar:0;scan:kbAltX;shift:[essAlt];st:#27'x'),
       (AnsiChar:0;scan:kbAltY;shift:[essAlt];st:#27'Y'),
       (AnsiChar:0;scan:kbAltY;shift:[essAlt];st:#27'y'),
       (AnsiChar:0;scan:kbAltZ;shift:[essAlt];st:#27'Z'),
       (AnsiChar:0;scan:kbAltZ;shift:[essAlt];st:#27'z'),
       (AnsiChar:0;scan:kbAltMinus;shift:[essAlt];st:#27'-'),
       (AnsiChar:0;scan:kbAltEqual;shift:[essAlt];st:#27'='),
       (AnsiChar:0;scan:kbAlt0;shift:[essAlt];st:#27'0'),
       (AnsiChar:0;scan:kbAlt1;shift:[essAlt];st:#27'1'),
       (AnsiChar:0;scan:kbAlt2;shift:[essAlt];st:#27'2'),
       (AnsiChar:0;scan:kbAlt3;shift:[essAlt];st:#27'3'),
       (AnsiChar:0;scan:kbAlt4;shift:[essAlt];st:#27'4'),
       (AnsiChar:0;scan:kbAlt5;shift:[essAlt];st:#27'5'),
       (AnsiChar:0;scan:kbAlt6;shift:[essAlt];st:#27'6'),
       (AnsiChar:0;scan:kbAlt7;shift:[essAlt];st:#27'7'),
       (AnsiChar:0;scan:kbAlt8;shift:[essAlt];st:#27'8'),
       (AnsiChar:0;scan:kbAlt9;shift:[essAlt];st:#27'9'),

       (AnsiChar:0;scan:kbF1;shift:[];st:#27'[[A'),                   {linux,konsole,xterm}
       (AnsiChar:0;scan:kbF2;shift:[];st:#27'[[B'),                   {linux,konsole,xterm}
       (AnsiChar:0;scan:kbF3;shift:[];st:#27'[[C'),                   {linux,konsole,xterm}
       (AnsiChar:0;scan:kbF4;shift:[];st:#27'[[D'),                   {linux,konsole,xterm}
       (AnsiChar:0;scan:kbF5;shift:[];st:#27'[[E'),                   {linux,konsole}
       (AnsiChar:0;scan:kbF1;shift:[];st:#27'[11~'),                  {Eterm,rxvt}
       (AnsiChar:0;scan:kbF2;shift:[];st:#27'[12~'),                  {Eterm,rxvt}
       (AnsiChar:0;scan:kbF3;shift:[];st:#27'[13~'),                  {Eterm,rxvt}
       (AnsiChar:0;scan:kbF4;shift:[];st:#27'[14~'),                  {Eterm,rxvt}
       (AnsiChar:0;scan:kbF5;shift:[];st:#27'[15~'),                  {xterm,Eterm,gnome,rxvt}
       (AnsiChar:0;scan:kbF6;shift:[];st:#27'[17~'),                  {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF7;shift:[];st:#27'[18~'),                  {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF8;shift:[];st:#27'[19~'),                  {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF9;shift:[];st:#27'[20~'),                  {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF10;shift:[];st:#27'[21~'),                 {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF11;shift:[];st:#27'[23~'),                 {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF12;shift:[];st:#27'[24~'),                 {linux,xterm,Eterm,konsole,gnome,rxvt}
       (AnsiChar:0;scan:kbF1;shift:[];st:#27'[M'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF2;shift:[];st:#27'[N'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF3;shift:[];st:#27'[O'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF4;shift:[];st:#27'[P'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF5;shift:[];st:#27'[Q'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF6;shift:[];st:#27'[R'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF7;shift:[];st:#27'[S'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF8;shift:[];st:#27'[T'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF9;shift:[];st:#27'[U'),                    {FreeBSD}
       (AnsiChar:0;scan:kbF10;shift:[];st:#27'[V'),                   {FreeBSD}
       (AnsiChar:0;scan:kbF11;shift:[];st:#27'[W'),                   {FreeBSD}
       (AnsiChar:0;scan:kbF12;shift:[];st:#27'[X'),                   {FreeBSD}
       (AnsiChar:0;scan:kbF1;shift:[];st:#27'OP'),                    {vt100,gnome,konsole}
       (AnsiChar:0;scan:kbF2;shift:[];st:#27'OQ'),                    {vt100,gnome,konsole}
       (AnsiChar:0;scan:kbF3;shift:[];st:#27'OR'),                    {vt100,gnome,konsole}
       (AnsiChar:0;scan:kbF4;shift:[];st:#27'OS'),                    {vt100,gnome,konsole}
       (AnsiChar:0;scan:kbF5;shift:[];st:#27'Ot'),                    {vt100}
       (AnsiChar:0;scan:kbF6;shift:[];st:#27'Ou'),                    {vt100}
       (AnsiChar:0;scan:kbF7;shift:[];st:#27'Ov'),                    {vt100}
       (AnsiChar:0;scan:kbF8;shift:[];st:#27'Ol'),                    {vt100}
       (AnsiChar:0;scan:kbF9;shift:[];st:#27'Ow'),                    {vt100}
       (AnsiChar:0;scan:kbF10;shift:[];st:#27'Ox'),                   {vt100}
       (AnsiChar:0;scan:kbF11;shift:[];st:#27'Oy'),                   {vt100}
       (AnsiChar:0;scan:kbF12;shift:[];st:#27'Oz'),                   {vt100}
       (AnsiChar:27;scan:kbEsc;shift:[];st:#27'[0~'),                 {if linux keyboard patched, escape
                                                                   returns this}
       (AnsiChar:0;scan:kbAltF5;shift:[essAlt];st:#27#27'OT'),        {pterm}
       (AnsiChar:0;scan:kbF5;shift:[];st:#27'OT'),                    {pterm}
       (AnsiChar:0;scan:kbF6;shift:[];st:#27'OU'),                    {pterm}
       (AnsiChar:0;scan:kbF7;shift:[];st:#27'OV'),                    {pterm}
       (AnsiChar:0;scan:kbF8;shift:[];st:#27'OW'),                    {pterm}
       (AnsiChar:0;scan:kbF9;shift:[];st:#27'OX'),                    {pterm}
       (AnsiChar:0;scan:kbF10;shift:[];st:#27'OY'),                   {pterm}
       (AnsiChar:0;scan:kbF11;shift:[];st:#27'OZ'),                   {pterm}
       (AnsiChar:0;scan:kbF12;shift:[];st:#27'O['),                   {pterm}
       (AnsiChar:0;scan:kbIns;shift:[];st:#27'[2~'),                  {linux,Eterm,rxvt}
       (AnsiChar:0;scan:kbDel;shift:[];st:#27'[3~'),                  {linux,Eterm,rxvt}
       (AnsiChar:0;scan:kbHome;shift:[];st:#27'[1~'),                 {linux}
       (AnsiChar:0;scan:kbHome;shift:[];st:#27'[7~'),                 {Eterm,rxvt}
       (AnsiChar:0;scan:kbHome;shift:[];st:#27'[H'),                  {FreeBSD}
       (AnsiChar:0;scan:kbHome;shift:[];st:#27'OH'),                  {some xterm configurations}
       (AnsiChar:0;scan:kbEnd;shift:[];st:#27'[4~'),                  {linux,Eterm}
       (AnsiChar:0;scan:kbEnd;shift:[];st:#27'[8~'),                  {rxvt}
       (AnsiChar:0;scan:kbEnd;shift:[];st:#27'[F'),                   {FreeBSD}
       (AnsiChar:0;scan:kbEnd;shift:[];st:#27'OF'),                   {some xterm configurations}
       (AnsiChar:0;scan:kbPgUp;shift:[];st:#27'[5~'),                 {linux,Eterm,rxvt}
       (AnsiChar:0;scan:kbPgUp;shift:[];st:#27'[I'),                  {FreeBSD}
       (AnsiChar:0;scan:kbPgDn;shift:[];st:#27'[6~'),                 {linux,Eterm,rxvt}
{$ifdef FREEBSD}
       (AnsiChar:0;scan:kbPgDn;shift:[];st:#27'[G'),                  {FreeBSD, conflicts with linux.
                                                                   Note: new FreeBSD versions seem
                                                                   to use xterm-like sequences, so
                                                                   this one is not needed for them.
                                                                   Todo: resolve conflicting sequences
                                                                   according to the TERM variable,
                                                                   instead of using IFDEFs, this way
                                                                   it'll work over SSH across platforms
                                                                   too.}
{$else FREEBSD}
       (AnsiChar:0;scan:kbCenter;shift:[];st:#27'[G'),                {linux}
{$endif FREEBSD}
       (AnsiChar:0;scan:kbCenter;shift:[];st:#27'[E'),                {xterm,gnome3}
       (AnsiChar:0;scan:kbUp;shift:[];st:#27'[A'),                    {linux,FreeBSD,rxvt}
       (AnsiChar:0;scan:kbDown;shift:[];st:#27'[B'),                  {linux,FreeBSD,rxvt}
       (AnsiChar:0;scan:kbRight;shift:[];st:#27'[C'),                 {linux,FreeBSD,rxvt}
       (AnsiChar:0;scan:kbLeft;shift:[];st:#27'[D'),                  {linux,FreeBSD,rxvt}
       (AnsiChar:0;scan:kbUp;shift:[];st:#27'OA'),                    {xterm}
       (AnsiChar:0;scan:kbDown;shift:[];st:#27'OB'),                  {xterm}
       (AnsiChar:0;scan:kbRight;shift:[];st:#27'OC'),                 {xterm}
       (AnsiChar:0;scan:kbLeft;shift:[];st:#27'OD'),                  {xterm}
(* Already recognized above as F11!
       (AnsiChar:0;scan:kbShiftF1;shift:[essShift];st:#27'[23~'),     {rxvt}
       (AnsiChar:0;scan:kbShiftF2;shift:[essShift];st:#27'[24~'),     {rxvt}
*)
(* These seem to be shifted. Probably something changed with linux's default keymaps.
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'[25~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'[26~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF5;shift:[essShift];st:#27'[28~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF6;shift:[essShift];st:#27'[29~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF7;shift:[essShift];st:#27'[31~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF8;shift:[essShift];st:#27'[32~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF9;shift:[essShift];st:#27'[33~'),     {linux,rxvt}
       (AnsiChar:0;scan:kbShiftF10;shift:[essShift];st:#27'[34~'),    {linux,rxvt}*)
       (AnsiChar:0;scan:kbShiftF1;shift:[essShift];st:#27'[25~'),     {linux}
       (AnsiChar:0;scan:kbShiftF2;shift:[essShift];st:#27'[26~'),     {linux}
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'[28~'),     {linux}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'[29~'),     {linux}
       (AnsiChar:0;scan:kbShiftF5;shift:[essShift];st:#27'[31~'),     {linux}
       (AnsiChar:0;scan:kbShiftF6;shift:[essShift];st:#27'[32~'),     {linux}
       (AnsiChar:0;scan:kbShiftF7;shift:[essShift];st:#27'[33~'),     {linux}
       (AnsiChar:0;scan:kbShiftF8;shift:[essShift];st:#27'[34~'),     {linux}
       (AnsiChar:0;scan:kbShiftF11;shift:[essShift];st:#27'[23$'),    {rxvt}
       (AnsiChar:0;scan:kbShiftF12;shift:[essShift];st:#27'[24$'),    {rxvt}
       (AnsiChar:0;scan:kbShiftF1;shift:[essShift];st:#27'[11;2~'),   {konsole in vt420pc mode}
       (AnsiChar:0;scan:kbShiftF2;shift:[essShift];st:#27'[12;2~'),   {konsole in vt420pc mode}
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'[13;2~'),   {konsole in vt420pc mode,kitty}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'[14;2~'),   {konsole in vt420pc mode}
       (AnsiChar:0;scan:kbShiftF5;shift:[essShift];st:#27'[15;2~'),   {xterm}
       (AnsiChar:0;scan:kbShiftF6;shift:[essShift];st:#27'[17;2~'),   {xterm}
       (AnsiChar:0;scan:kbShiftF7;shift:[essShift];st:#27'[18;2~'),   {xterm}
       (AnsiChar:0;scan:kbShiftF8;shift:[essShift];st:#27'[19;2~'),   {xterm}
       (AnsiChar:0;scan:kbShiftF9;shift:[essShift];st:#27'[20;2~'),   {xterm}
       (AnsiChar:0;scan:kbShiftF10;shift:[essShift];st:#27'[21;2~'),  {xterm}
       (AnsiChar:0;scan:kbShiftF11;shift:[essShift];st:#27'[23;2~'),  {xterm}
       (AnsiChar:0;scan:kbShiftF12;shift:[essShift];st:#27'[24;2~'),  {xterm}
       (AnsiChar:0;scan:kbShiftF1;shift:[essShift];st:#27'O2P'),      {konsole,xterm}
       (AnsiChar:0;scan:kbShiftF2;shift:[essShift];st:#27'O2Q'),      {konsole,xterm}
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'O2R'),      {konsole,xterm}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'O2S'),      {konsole,xterm}
       (AnsiChar:0;scan:kbShiftF1;shift:[essShift];st:#27'[1;2P'),    {xterm,gnome3}
       (AnsiChar:0;scan:kbShiftF2;shift:[essShift];st:#27'[1;2Q'),    {xterm,gnome3}
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'[1;2R'),    {xterm,gnome3}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'[1;2S'),    {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF1;shift:[essCtrl];st:#27'O5P'),        {konsole,xterm}
       (AnsiChar:0;scan:kbCtrlF2;shift:[essCtrl];st:#27'O5Q'),        {konsole,xterm}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl];st:#27'O5R'),        {konsole,xterm}
       (AnsiChar:0;scan:kbCtrlF4;shift:[essCtrl];st:#27'O5S'),        {konsole,xterm}
       (AnsiChar:0;scan:kbCtrlF1;shift:[essCtrl];st:#27'[1;5P'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF2;shift:[essCtrl];st:#27'[1;5Q'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl];st:#27'[1;5R'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF4;shift:[essCtrl];st:#27'[1;5S'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF1;shift:[essCtrl];st:#27'[11;5~'),     {none, but expected}
       (AnsiChar:0;scan:kbCtrlF2;shift:[essCtrl];st:#27'[12;5~'),     {none, but expected}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl];st:#27'[13;5~'),     {kitty}
       (AnsiChar:0;scan:kbCtrlF4;shift:[essCtrl];st:#27'[14;5~'),     {none, but expected}
       (AnsiChar:0;scan:kbCtrlF5;shift:[essCtrl];st:#27'[15;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF6;shift:[essCtrl];st:#27'[17;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF7;shift:[essCtrl];st:#27'[18;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF8;shift:[essCtrl];st:#27'[19;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF9;shift:[essCtrl];st:#27'[20;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF10;shift:[essCtrl];st:#27'[21;5~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlF11;shift:[essCtrl];st:#27'[23;5~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlF12;shift:[essCtrl];st:#27'[24;5~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlF1;shift:[essCtrl];st:#27'[11^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF2;shift:[essCtrl];st:#27'[12^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl];st:#27'[13^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF4;shift:[essCtrl];st:#27'[14^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF5;shift:[essCtrl];st:#27'[15^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF6;shift:[essCtrl];st:#27'[17^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF7;shift:[essCtrl];st:#27'[18^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF8;shift:[essCtrl];st:#27'[19^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF9;shift:[essCtrl];st:#27'[20^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlF10;shift:[essCtrl];st:#27'[21^'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlF11;shift:[essCtrl];st:#27'[23^'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlF12;shift:[essCtrl];st:#27'[24^'),      {rxvt}
       (AnsiChar:0;scan:kbShiftIns;shift:[essShift];st:#27'[2;2~'),   {should be the code, but shift+ins
                                                                   is paste X clipboard in many
                                                                   terminal emulators :(}
       (AnsiChar:0;scan:kbShiftDel;shift:[essShift];st:#27'[3;2~'),   {xterm,konsole}
       (AnsiChar:0;scan:kbCtrlIns;shift:[essCtrl];st:#27'[2;5~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlDel;shift:[essCtrl];st:#27'[3;5~'),     {xterm}
       (AnsiChar:0;scan:kbShiftIns;shift:[essShift];st:#27'[2$'),     {rxvt}
       (AnsiChar:0;scan:kbShiftDel;shift:[essShift];st:#27'[3$'),     {rxvt}
       (AnsiChar:0;scan:kbCtrlIns;shift:[essCtrl];st:#27'[2^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlDel;shift:[essCtrl];st:#27'[3^'),       {rxvt}
       (AnsiChar:0;scan:kbAltF1;shift:[essAlt];st:#27#27'[[A'),
       (AnsiChar:0;scan:kbAltF2;shift:[essAlt];st:#27#27'[[B'),
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27#27'[[C'),
       (AnsiChar:0;scan:kbAltF4;shift:[essAlt];st:#27#27'[[D'),
       (AnsiChar:0;scan:kbAltF5;shift:[essAlt];st:#27#27'[[E'),
       (AnsiChar:0;scan:kbAltF1;shift:[essAlt];st:#27#27'[11~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF2;shift:[essAlt];st:#27#27'[12~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27#27'[13~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF4;shift:[essAlt];st:#27#27'[14~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF5;shift:[essAlt];st:#27#27'[15~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF6;shift:[essAlt];st:#27#27'[17~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF7;shift:[essAlt];st:#27#27'[18~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF8;shift:[essAlt];st:#27#27'[19~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF9;shift:[essAlt];st:#27#27'[20~'),      {rxvt}
       (AnsiChar:0;scan:kbAltF10;shift:[essAlt];st:#27#27'[21~'),     {rxvt}
       (AnsiChar:0;scan:kbAltF11;shift:[essAlt];st:#27#27'[23~'),     {rxvt}
       (AnsiChar:0;scan:kbAltF12;shift:[essAlt];st:#27#27'[24~'),     {rxvt}
       (AnsiChar:0;scan:kbAltF1;shift:[essAlt];st:#27#27'OP'),        {xterm}
       (AnsiChar:0;scan:kbAltF2;shift:[essAlt];st:#27#27'OQ'),        {xterm}
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27#27'OR'),        {xterm}
       (AnsiChar:0;scan:kbAltF4;shift:[essAlt];st:#27#27'OS'),        {xterm}
       (AnsiChar:0;scan:kbAltF5;shift:[essAlt];st:#27#27'Ot'),        {xterm}
       (AnsiChar:0;scan:kbAltF6;shift:[essAlt];st:#27#27'Ou'),        {xterm}
       (AnsiChar:0;scan:kbAltF7;shift:[essAlt];st:#27#27'Ov'),        {xterm}
       (AnsiChar:0;scan:kbAltF8;shift:[essAlt];st:#27#27'Ol'),        {xterm}
       (AnsiChar:0;scan:kbAltF9;shift:[essAlt];st:#27#27'Ow'),        {xterm}
       (AnsiChar:0;scan:kbAltF10;shift:[essAlt];st:#27#27'Ox'),       {xterm}
       (AnsiChar:0;scan:kbAltF11;shift:[essAlt];st:#27#27'Oy'),       {xterm}
       (AnsiChar:0;scan:kbAltF12;shift:[essAlt];st:#27#27'Oz'),       {xterm}
       (AnsiChar:0;scan:kbAltF1;shift:[essAlt];st:#27'[1;3P'),        {xterm,gnome3}
       (AnsiChar:0;scan:kbAltF2;shift:[essAlt];st:#27'[1;3Q'),        {xterm,gnome3}
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27'[1;3R'),        {xterm,gnome3}
       (AnsiChar:0;scan:kbAltF4;shift:[essAlt];st:#27'[1;3S'),        {xterm,gnome3}
       (AnsiChar:0;scan:kbAltF1;shift:[essAlt];st:#27'O3P'),          {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF2;shift:[essAlt];st:#27'O3Q'),          {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27'O3R'),          {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF3;shift:[essAlt];st:#27'[13;3~'),       {kitty}
       (AnsiChar:0;scan:kbAltF4;shift:[essAlt];st:#27'O3S'),          {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF5;shift:[essAlt];st:#27'[15;3~'),       {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF6;shift:[essAlt];st:#27'[17;3~'),       {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF7;shift:[essAlt];st:#27'[18;3~'),       {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF8;shift:[essAlt];st:#27'[19;3~'),       {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF9;shift:[essAlt];st:#27'[20;3~'),       {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF10;shift:[essAlt];st:#27'[21;3~'),      {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF11;shift:[essAlt];st:#27'[23;3~'),      {xterm on FreeBSD}
       (AnsiChar:0;scan:kbAltF12;shift:[essAlt];st:#27'[24;3~'),      {xterm on FreeBSD}

       (AnsiChar:0;scan:kbCtrlF1;shift:[essCtrl,essShift];st:#27'[1;6P'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF2;shift:[essCtrl,essShift];st:#27'[1;6Q'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl,essShift];st:#27'[1;6R'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF3;shift:[essCtrl,essShift];st:#27'[13;6~'),     {kitty}
       (AnsiChar:0;scan:kbCtrlF4;shift:[essCtrl,essShift];st:#27'[1;6S'),      {xterm,gnome3}
       (AnsiChar:0;scan:kbCtrlF5;shift:[essCtrl,essShift];st:#27'[15;6~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF6;shift:[essCtrl,essShift];st:#27'[17;6~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF7;shift:[essCtrl,essShift];st:#27'[18;6~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF8;shift:[essCtrl,essShift];st:#27'[19;6~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF9;shift:[essCtrl,essShift];st:#27'[20;6~'),     {xterm}
       (AnsiChar:0;scan:kbCtrlF10;shift:[essCtrl,essShift];st:#27'[21;6~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlF11;shift:[essCtrl,essShift];st:#27'[23;6~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlF12;shift:[essCtrl,essShift];st:#27'[24;6~'),    {xterm}

       (AnsiChar:0;scan:kbAltF1;shift:[essShift,essAlt];st:#27'[1;4P'),        {xterm}
       (AnsiChar:0;scan:kbAltF2;shift:[essShift,essAlt];st:#27'[1;4Q'),        {xterm}
       (AnsiChar:0;scan:kbAltF3;shift:[essShift,essAlt];st:#27'[1;4R'),        {xterm}
       (AnsiChar:0;scan:kbAltF3;shift:[essShift,essAlt];st:#27'[13;4~'),       {kitty}
       (AnsiChar:0;scan:kbAltF4;shift:[essShift,essAlt];st:#27'[1;4S'),        {xterm}
       (AnsiChar:0;scan:kbAltF5;shift:[essShift,essAlt];st:#27'[15;4~'),       {xterm}
       (AnsiChar:0;scan:kbAltF6;shift:[essShift,essAlt];st:#27'[17;4~'),       {xterm}
       (AnsiChar:0;scan:kbAltF7;shift:[essShift,essAlt];st:#27'[18;4~'),       {xterm}
       (AnsiChar:0;scan:kbAltF8;shift:[essShift,essAlt];st:#27'[19;4~'),       {xterm}
       (AnsiChar:0;scan:kbAltF9;shift:[essShift,essAlt];st:#27'[20;4~'),       {xterm}
       (AnsiChar:0;scan:kbAltF10;shift:[essShift,essAlt];st:#27'[21;4~'),      {xterm}
       (AnsiChar:0;scan:kbAltF11;shift:[essShift,essAlt];st:#27'[23;4~'),      {xterm}
       (AnsiChar:0;scan:kbAltF12;shift:[essShift,essAlt];st:#27'[24;4~'),      {xterm}

       (AnsiChar:0;scan:KbAltF1;shift:[essCtrl,essShift,essAlt];st:#27'[1;8P'),        {xterm}
       (AnsiChar:0;scan:KbAltF2;shift:[essCtrl,essShift,essAlt];st:#27'[1;8Q'),        {xterm}
       (AnsiChar:0;scan:KbAltF3;shift:[essCtrl,essShift,essAlt];st:#27'[1;8R'),        {xterm}
       (AnsiChar:0;scan:KbAltF3;shift:[essCtrl,essShift,essAlt];st:#27'[13;8~'),       {kitty}
       (AnsiChar:0;scan:KbAltF4;shift:[essCtrl,essShift,essAlt];st:#27'[1;8S'),        {xterm}
       (AnsiChar:0;scan:KbAltF5;shift:[essCtrl,essShift,essAlt];st:#27'[15;8~'),       {xterm}
       (AnsiChar:0;scan:KbAltF6;shift:[essCtrl,essShift,essAlt];st:#27'[17;8~'),       {xterm}
       (AnsiChar:0;scan:KbAltF7;shift:[essCtrl,essShift,essAlt];st:#27'[18;8~'),       {xterm}
       (AnsiChar:0;scan:KbAltF8;shift:[essCtrl,essShift,essAlt];st:#27'[19;8~'),       {xterm}
       (AnsiChar:0;scan:KbAltF9;shift:[essCtrl,essShift,essAlt];st:#27'[20;8~'),       {xterm}
       (AnsiChar:0;scan:KbAltF10;shift:[essCtrl,essShift,essAlt];st:#27'[21;8~'),      {xterm}
       (AnsiChar:0;scan:KbAltF11;shift:[essCtrl,essShift,essAlt];st:#27'[23;8~'),      {xterm}
       (AnsiChar:0;scan:KbAltF12;shift:[essCtrl,essShift,essAlt];st:#27'[24;8~'),      {xterm}

       (AnsiChar:0;scan:kbShiftTab;shift:[essShift];st:#27#9),        {linux - 'Meta_Tab'}
       (AnsiChar:0;scan:kbShiftTab;shift:[essShift];st:#27'[Z'),
       (AnsiChar:0;scan:kbUp;shift:[essShift];st:#27'[1;2A'),    {xterm}
       (AnsiChar:0;scan:kbDown;shift:[essShift];st:#27'[1;2B'),  {xterm}
       (AnsiChar:0;scan:kbRight;shift:[essShift];st:#27'[1;2C'), {xterm}
       (AnsiChar:0;scan:kbLeft;shift:[essShift];st:#27'[1;2D'),  {xterm}
       (AnsiChar:0;scan:kbCenter;shift:[essShift];st:#27'[1;2E'),{xterm}
       (AnsiChar:0;scan:kbPgUp;shift:[essShift];st:#27'[5;2~'),  {fpterm, xterm-compatible sequence (but xterm uses shift+pgup/pgdn for scrollback)}
       (AnsiChar:0;scan:kbPgDn;shift:[essShift];st:#27'[6;2~'),  {fpterm, xterm-compatible sequence (but xterm uses shift+pgup/pgdn for scrollback)}
       (AnsiChar:0;scan:kbUp;shift:[essShift];st:#27'[a'),       {rxvt}
       (AnsiChar:0;scan:kbDown;shift:[essShift];st:#27'[b'),     {rxvt}
       (AnsiChar:0;scan:kbRight;shift:[essShift];st:#27'[c'),    {rxvt}
       (AnsiChar:0;scan:kbLeft;shift:[essShift];st:#27'[d'),     {rxvt}
       (AnsiChar:0;scan:kbEnd;shift:[essShift];st:#27'[1;2F'),   {xterm}
       (AnsiChar:0;scan:kbEnd;shift:[essShift];st:#27'[8$'),     {rxvt}
       (AnsiChar:0;scan:kbHome;shift:[essShift];st:#27'[1;2H'),  {xterm}
       (AnsiChar:0;scan:kbHome;shift:[essShift];st:#27'[7$'),    {rxvt}
       (AnsiChar:0;scan:kbShiftIns;shift:[essShift];st:#27'Op'), {rxvt - on numpad}
       (AnsiChar:0;scan:kbShiftDel;shift:[essShift];st:#27'On'), {rxvt - on numpad}

       (AnsiChar:0;scan:KbCtrlUp;shift:[essCtrl,essShift];st:#27'[1;6A'),    {xterm}
       (AnsiChar:0;scan:KbCtrlDown;shift:[essCtrl,essShift];st:#27'[1;6B'),  {xterm}
       (AnsiChar:0;scan:KbCtrlRight;shift:[essCtrl,essShift];st:#27'[1;6C'), {xterm, xfce4}
       (AnsiChar:0;scan:KbCtrlLeft;shift:[essCtrl,essShift];st:#27'[1;6D'),  {xterm, xfce4}
       (AnsiChar:0;scan:KbCtrlCenter;shift:[essCtrl,essShift];st:#27'[1;6E'),{xterm}
       (AnsiChar:0;scan:KbCtrlHome;shift:[essCtrl,essShift];st:#27'[1;6H'),  {xterm}
       (AnsiChar:0;scan:KbCtrlEnd;shift:[essCtrl,essShift];st:#27'[1;6F'),   {xterm}
       (AnsiChar:0;scan:KbCtrlIns;shift:[essCtrl,essShift];st:#27'[2;6~'),   {xterm}
       (AnsiChar:0;scan:KbCtrlDel;shift:[essCtrl,essShift];st:#27'[3;6~'),   {xterm}
       (AnsiChar:0;scan:kbCtrlPgUp;shift:[essCtrl,essShift];st:#27'[5;6~'),  {fpterm, xterm-compatible sequence (but xterm uses shift+pgup/pgdn for scrollback)}
       (AnsiChar:0;scan:kbCtrlPgDn;shift:[essCtrl,essShift];st:#27'[6;6~'),  {fpterm, xterm-compatible sequence (but xterm uses shift+pgup/pgdn for scrollback)}

       (AnsiChar:0;scan:kbCtrlPgDn;shift:[essCtrl];st:#27'[6;5~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlPgUp;shift:[essCtrl];st:#27'[5;5~'),    {xterm}
       (AnsiChar:0;scan:kbCtrlUp;shift:[essCtrl];st:#27'[1;5A'),      {xterm}
       (AnsiChar:0;scan:kbCtrlDown;shift:[essCtrl];st:#27'[1;5B'),    {xterm}
       (AnsiChar:0;scan:kbCtrlRight;shift:[essCtrl];st:#27'[1;5C'),   {xterm}
       (AnsiChar:0;scan:kbCtrlLeft;shift:[essCtrl];st:#27'[1;5D'),    {xterm}
       (AnsiChar:0;scan:kbCtrlCenter;shift:[essCtrl];st:#27'[1;5E'),  {xterm}
       (AnsiChar:0;scan:kbCtrlUp;shift:[essCtrl];st:#27'[Oa'),        {rxvt}
       (AnsiChar:0;scan:kbCtrlDown;shift:[essCtrl];st:#27'[Ob'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlRight;shift:[essCtrl];st:#27'[Oc'),     {rxvt}
       (AnsiChar:0;scan:kbCtrlLeft;shift:[essCtrl];st:#27'[Od'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlEnd;shift:[essCtrl];st:#27'[1;5F'),     {xterm}
       (AnsiChar:0;scan:kbCtrlEnd;shift:[essCtrl];st:#27'[8^'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlHome;shift:[essCtrl];st:#27'[1;5H'),    {xterm}
       (AnsiChar:0;scan:kbCtrlHome;shift:[essCtrl];st:#27'[7^'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlPgUp;shift:[essCtrl];st:#27'[5^'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlPgDn;shift:[essCtrl];st:#27'[6^'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlUp;shift:[essCtrl];st:#27'Oa'),         {rxvt}
       (AnsiChar:0;scan:kbCtrlDown;shift:[essCtrl];st:#27'Ob'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlLeft;shift:[essCtrl];st:#27'Od'),       {rxvt}
       (AnsiChar:0;scan:kbCtrlRight;shift:[essCtrl];st:#27'Oc'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlPgUp;shift:[essCtrl,essShift];st:#27'[5@'),     {rxvt}
       (AnsiChar:0;scan:kbCtrlPgDn;shift:[essCtrl,essShift];st:#27'[6@'),     {rxvt}
       (AnsiChar:0;scan:kbCtrlEnd;shift:[essCtrl,essShift];st:#27'[8@'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlHome;shift:[essCtrl,essShift];st:#27'[7@'),     {rxvt}
       (AnsiChar:0;scan:kbCtrlIns;shift:[essCtrl,essShift];st:#27'[2@'),      {rxvt}
       (AnsiChar:0;scan:kbCtrlDel;shift:[essCtrl,essShift];st:#27'[3@'),      {rxvt}


       (AnsiChar:0;scan:kbAltUp;shift:[essAlt];st:#27#27'[A'),        {rxvt}
       (AnsiChar:0;scan:kbAltDown;shift:[essAlt];st:#27#27'[B'),      {rxvt}
       (AnsiChar:0;scan:kbAltLeft;shift:[essAlt];st:#27#27'[D'),      {rxvt}
       (AnsiChar:0;scan:kbAltRight;shift:[essAlt];st:#27#27'[C'),     {rxvt}
       (AnsiChar:0;scan:kbAltUp;shift:[essShift,essAlt];st:#27#27'[a'),        {rxvt}
       (AnsiChar:0;scan:kbAltDown;shift:[essShift,essAlt];st:#27#27'[b'),      {rxvt}
       (AnsiChar:0;scan:kbAltLeft;shift:[essShift,essAlt];st:#27#27'[d'),      {rxvt}
       (AnsiChar:0;scan:kbAltRight;shift:[essShift,essAlt];st:#27#27'[c'),     {rxvt}
{$ifdef HAIKU}
       (AnsiChar:0;scan:kbAltUp;shift:[essAlt];st:#27#27'OA'),
       (AnsiChar:0;scan:kbAltDown;shift:[essAlt];st:#27#27'OB'),
       (AnsiChar:0;scan:kbAltRight;shift:[essAlt];st:#27#27'OC'),
{$else}
       (AnsiChar:0;scan:kbAltUp;shift:[essAlt];st:#27'OA'),
       (AnsiChar:0;scan:kbAltDown;shift:[essAlt];st:#27'OB'),
       (AnsiChar:0;scan:kbAltRight;shift:[essAlt];st:#27'OC'),
{$endif}
       (AnsiChar:0;scan:kbAltLeft;shift:[essAlt];st:#27#27'OD'),
       (AnsiChar:0;scan:kbAltPgUp;shift:[essAlt];st:#27#27'[5~'),     {rxvt}
       (AnsiChar:0;scan:kbAltPgDn;shift:[essAlt];st:#27#27'[6~'),     {rxvt}
       (AnsiChar:0;scan:kbAltEnd;shift:[essAlt];st:#27#27'[4~'),
       (AnsiChar:0;scan:kbAltEnd;shift:[essAlt];st:#27#27'[8~'),      {rxvt}
       (AnsiChar:0;scan:kbAltHome;shift:[essAlt];st:#27#27'[1~'),
       (AnsiChar:0;scan:kbAltHome;shift:[essAlt];st:#27#27'[7~'),     {rxvt}
       (AnsiChar:0;scan:kbAltIns;shift:[essAlt];st:#27#27'[2~'),      {rxvt}
       (AnsiChar:0;scan:kbAltDel;shift:[essAlt];st:#27#27'[3~'),      {rxvt}
       (AnsiChar:0;scan:kbAltPgUp;shift:[essShift,essAlt];st:#27#27'[5$'),     {rxvt}
       (AnsiChar:0;scan:kbAltPgDn;shift:[essShift,essAlt];st:#27#27'[6$'),     {rxvt}
       (AnsiChar:0;scan:kbAltEnd;shift:[essShift,essAlt];st:#27#27'[8$'),      {rxvt}
       (AnsiChar:0;scan:kbAltHome;shift:[essShift,essAlt];st:#27#27'[7$'),     {rxvt}
       (AnsiChar:0;scan:kbAltIns;shift:[essShift,essAlt];st:#27#27'[2$'),      {rxvt}
       (AnsiChar:0;scan:kbAltDel;shift:[essShift,essAlt];st:#27#27'[3$'),      {rxvt}
       (AnsiChar:0;scan:kbAltPgUp;shift:[essCtrl,essShift,essAlt];st:#27#27'[5@'),     {rxvt}
       (AnsiChar:0;scan:kbAltPgDn;shift:[essCtrl,essShift,essAlt];st:#27#27'[6@'),     {rxvt}
       (AnsiChar:0;scan:kbAltEnd;shift:[essCtrl,essShift,essAlt];st:#27#27'[8@'),      {rxvt}
       (AnsiChar:0;scan:kbAltHome;shift:[essCtrl,essShift,essAlt];st:#27#27'[7@'),     {rxvt}
       (AnsiChar:0;scan:kbAltIns;shift:[essCtrl,essShift,essAlt];st:#27#27'[2@'),      {rxvt}
       (AnsiChar:0;scan:kbAltDel;shift:[essCtrl,essShift,essAlt];st:#27#27'[3@'),      {rxvt}

       (AnsiChar:0;scan:KbAltUp;shift:[essAlt];st:#27'[1;3A'),        {xterm}
       (AnsiChar:0;scan:KbAltDown;shift:[essAlt];st:#27'[1;3B'),      {xterm}
       (AnsiChar:0;scan:KbAltRight;shift:[essAlt];st:#27'[1;3C'),     {xterm}
       (AnsiChar:0;scan:KbAltLeft;shift:[essAlt];st:#27'[1;3D'),      {xterm}
       (AnsiChar:0;scan:KbAltCenter;shift:[essAlt];st:#27'[1;3E'),    {xterm}
       (AnsiChar:0;scan:KbAltHome;shift:[essAlt];st:#27'[1;3H'),      {xterm}
       (AnsiChar:0;scan:KbAltEnd;shift:[essAlt];st:#27'[1;3F'),       {xterm}
       (AnsiChar:0;scan:KbAltIns;shift:[essAlt];st:#27'[2;3~'),       {xterm}
       (AnsiChar:0;scan:KbAltDel;shift:[essAlt];st:#27'[3;3~'),       {xterm}
       (AnsiChar:0;scan:kbAltPgUp;shift:[essAlt];st:#27'[5;3~'),      {xterm}
       (AnsiChar:0;scan:kbAltPgDn;shift:[essAlt];st:#27'[6;3~'),      {xterm}

       (AnsiChar:0;scan:kbAltUp;shift:[essShift,essAlt];st:#27'[1;4A'),      {xterm}
       (AnsiChar:0;scan:kbAltDown;shift:[essShift,essAlt];st:#27'[1;4B'),    {xterm}
       (AnsiChar:0;scan:kbAltRight;shift:[essShift,essAlt];st:#27'[1;4C'),   {xterm}
       (AnsiChar:0;scan:kbAltLeft;shift:[essShift,essAlt];st:#27'[1;4D'),    {xterm}
       (AnsiChar:0;scan:kbAltCenter;shift:[essShift,essAlt];st:#27'[1;4E'),  {xterm}
       (AnsiChar:0;scan:kbAltHome;shift:[essShift,essAlt];st:#27'[1;4H'),    {xterm}
       (AnsiChar:0;scan:kbAltEnd;shift:[essShift,essAlt];st:#27'[1;4F'),     {xterm}
       (AnsiChar:0;scan:kbAltIns;shift:[essShift,essAlt];st:#27'[2;4~'),     {xterm}
       (AnsiChar:0;scan:kbAltDel;shift:[essShift,essAlt];st:#27'[3;4~'),     {xterm}
       (AnsiChar:0;scan:kbAltPgUp;shift:[essShift,essAlt];st:#27'[5;4~'),    {xterm}
       (AnsiChar:0;scan:kbAltPgDn;shift:[essShift,essAlt];st:#27'[6;4~'),    {xterm}

       (AnsiChar:0;scan:KbAltIns;shift:[essCtrl,essAlt];st:#27'[2;7~'),     {xterm}
       (AnsiChar:0;scan:KbAltDel;shift:[essCtrl,essAlt];st:#27'[3;7~'),     {xterm (Del on numpad)}
       (AnsiChar:0;scan:KbAltPgUp;shift:[essCtrl,essAlt];st:#27'[5;7~'),    {xterm}
       (AnsiChar:0;scan:KbAltPgDn;shift:[essCtrl,essAlt];st:#27'[6;7~'),    {xterm}

       (AnsiChar:0;scan:KbAltUp;shift:[essCtrl,essShift,essAlt];st:#27'[1;8A'),      {xterm}
       (AnsiChar:0;scan:KbAltDown;shift:[essCtrl,essShift,essAlt];st:#27'[1;8B'),    {xterm}
       (AnsiChar:0;scan:KbAltRight;shift:[essCtrl,essShift,essAlt];st:#27'[1;8C'),   {xterm}
       (AnsiChar:0;scan:KbAltLeft;shift:[essCtrl,essShift,essAlt];st:#27'[1;8D'),    {xterm}
       (AnsiChar:0;scan:KbAltCenter;shift:[essCtrl,essShift,essAlt];st:#27'[1;8E'),  {xterm}
       (AnsiChar:0;scan:KbAltHome;shift:[essCtrl,essShift,essAlt];st:#27'[1;8H'),    {xterm}
       (AnsiChar:0;scan:KbAltEnd;shift:[essCtrl,essShift,essAlt];st:#27'[1;8F'),     {xterm}
       (AnsiChar:0;scan:KbAltIns;shift:[essCtrl,essShift,essAlt];st:#27'[2;8~'),     {xterm}
       (AnsiChar:0;scan:KbAltDel;shift:[essCtrl,essShift,essAlt];st:#27'[3;8~'),     {xterm}
       (AnsiChar:0;scan:KbAltPgUp;shift:[essCtrl,essShift,essAlt];st:#27'[5;8~'),    {xterm}
       (AnsiChar:0;scan:KbAltPgDn;shift:[essCtrl,essShift,essAlt];st:#27'[6;8~'),    {xterm}

  { xterm default values }
  { xterm alternate default values }
  { ignored sequences }
       (AnsiChar:0;scan:0;shift:[];st:#27'[?1;0c'),
       (AnsiChar:0;scan:0;shift:[];st:#27'[?1l'),
       (AnsiChar:0;scan:0;shift:[];st:#27'[?1h'),
       (AnsiChar:0;scan:0;shift:[];st:#27'[?1;2c'),
       (AnsiChar:0;scan:0;shift:[];st:#27'[?7l'),
       (AnsiChar:0;scan:0;shift:[];st:#27'[?7h')
      );

      {those are rxvt specific, due to conflict can not put in main array }
const rxvt_key_sequences:array[0..7] of key_sequence=(
       (AnsiChar:0;scan:kbShiftF3;shift:[essShift];st:#27'[25~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF4;shift:[essShift];st:#27'[26~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF5;shift:[essShift];st:#27'[28~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF6;shift:[essShift];st:#27'[29~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF7;shift:[essShift];st:#27'[31~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF8;shift:[essShift];st:#27'[32~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF9;shift:[essShift];st:#27'[33~'),     {rxvt,pterm}
       (AnsiChar:0;scan:kbShiftF10;shift:[essShift];st:#27'[34~')     {rxvt,pterm}
       );

type TTerm = (trNone,trCons,trEterm,trGnome,trKonsole,trRxvt,trScreen,trXterm,trLinux);

function detect_terminal:TTerm;
const terminals:array[TTerm] of string[7]=('None'#0,'cons','eterm','gnome',
                                                'konsole','rxvt','screen',
                                                'xterm','linux');
var term:string;
    i,t:TTerm;
begin
  detect_terminal:=trNone;
  t:=trNone;
  term:=fpgetenv('TERM');
  for i:=low(terminals) to high(terminals) do
    if copy(term,1,length(terminals[i]))=terminals[i] then
      begin
        t:=i;
        break;
      end;
  if t=trXterm then
    begin
      {Rxvt sets TERM=xterm and COLORTERM=rxvt. Gnome does something similar.}
      term:=fpgetenv('COLORTERM');
      for i:=low(terminals) to high(terminals) do
        if copy(term,1,length(terminals[i]))=terminals[i] then
          begin
            t:=i;
            break;
          end;
    end;
  detect_terminal:=t;
end;

type  TKeyByte = array [0..23] of byte;
      PKeyByte = ^TKeyByte;

const cSunKey : TKeyByte =(224,225,226,227,228,229,230,231,232,233,192,193,
                                2,3,214,220,216,222,218,197,0,0,0,0);
const cKb : TKeyByte = (kbF1,kbF2,kbF3,kbF4,kbF5,kbF6,kbF7,kbF8,kbF9,kbF10,kbF11,kbF12,
                        kbIns,kbDel,KbHome,KbEnd,KbPgUp,KbPgDn,KbCenter,{kbMenu}0,KbUp,KbLeft,KbRight,KbDown);
const cKbCtrl : TKeyByte = (kbCtrlF1,kbCtrlF2,kbCtrlF3,kbCtrlF4,kbCtrlF5,kbCtrlF6,kbCtrlF7,kbCtrlF8,kbCtrlF9,kbCtrlF10,kbCtrlF11,kbCtrlF12,
                        kbCtrlIns,kbCtrlDel,KbCtrlHome,KbCtrlEnd,KbCtrlPgUp,KbCtrlPgDn,KbCtrlCenter,{kbCtrlMenu}0,KbCtrlUp,KbCtrlLeft,KbCtrlRight,KbCtrlDown);
const cKbAlt : TKeyByte = (kbAltF1,kbAltF2,kbAltF3,kbAltF4,kbAltF5,kbAltF6,kbAltF7,kbAltF8,kbAltF9,kbAltF10,kbAltF11,kbAltF12,
                        kbAltIns,kbAltDel,KbAltHome,KbAltEnd,KbAltPgUp,KbAltPgDn,KbAltCenter,{kbAltMenu}0,KbAltUp,KbAltLeft,KbAltRight,KbAltDown);

const cSunKeyEnd : array [0..7] of string [3] = ('z',';2z',';3z',';4z',';5z',';6z',';7z',';8z');
      cKeyScanCode : array [0..7] of PKeyByte  = (@cKb,@cKb,@cKbAlt,@cKbAlt,@cKbCtrl,@cKbCtrl,@cKbAlt,@cKbAlt);
      cKeyShift : array [0..7] of TEnhancedShiftState=(
       {}     [],
       {2}    [essShift],
       {3}    [essAlt],
       {4}    [essShift,essAlt],
       {5}    [essCtrl],
       {6}    [essCtrl,essShift],
       {7}    [essCtrl,essAlt],
       {8}    [essCtrl,essShift,essAlt]
      );

procedure sunKeySquences;
{    generate Sun function-key escape squences
     format:  CSI number z
              CSI number ; modifier z
}
var st, zt : string[15];
    AnsiChar : byte;
    scan : byte;
    shift:TEnhancedShiftState;
    i,n : dword;
    kb : PKeyByte;
begin
  AnsiChar:=0;
  for n:=0 to 7 do
  begin
    kb:=cKeyScanCode[n];
    shift:=cKeyShift[n];
    zt:=cSunKeyEnd[n];
    for i:=0 to 23-5 do
    begin
      str(cSunKey[i],st);
      st:=#27'['+st+zt;
      scan:=kb^[i];
      DoAddSequence(st,AnsiChar,scan,shift);
    end;
  end;
end;

procedure PushUnicodeKey (k: TEnhancedKeyEvent; UnicodeCodePoint : longint;  ReplacementAsciiChar:AnsiChar);
begin
  if UnicodeCodePoint<=$FFFF then
    begin
      { Code point is in the Basic Multilingual Plane (BMP)
        -> encode as single WideChar }
      k.UnicodeChar:=WideChar(UnicodeCodePoint);
      if UnicodeCodePoint<=127 then
        k.AsciiChar:=Chr(UnicodeCodePoint)
      else
        k.AsciiChar:=ReplacementAsciiChar;
      PushKey(k);
    end
  else if UnicodeCodePoint<=$10FFFF then
    begin
      { Code point from the Supplementary Planes (U+010000..U+10FFFF)
        -> encode as a surrogate pair of WideChars (as in UTF-16) }
      k.UnicodeChar:=WideChar(((UnicodeCodePoint-$10000) shr 10)+$D800);
      k.AsciiChar:=ReplacementAsciiChar;
      PushKey(k);
      k.UnicodeChar:=WideChar(((UnicodeCodePoint-$10000) and %1111111111)+$DC00);
      PushKey(k);
    end;
end;


const           {lookup tables: nKey, modifier -> ScanCode, KeyChar }
    cAltAscii   : array [0..127] of AnsiChar = (
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00);

    cCtrlAscii   : array [0..127] of AnsiChar = (
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$08,#$00,#$00,#$00,#$00,#$0a,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$1b,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,
      #$00,#$01,#$02,#$03,#$04,#$05,#$06,#$00,#$00,#$09,#$0a,#$0b,#$0c,#$0d,#$0e,#$0f,
      #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1a,#$1b,#$1c,#$1d,#$1e,#$1f,
      #$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0a,#$0b,#$0c,#$0d,#$0e,#$0f,
      #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1a,#$1b,#$1c,#$1d,#$1e,#$7f);

    cShiftAscii   : array [0..127] of AnsiChar = (
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$08,#$00,#$00,#$00,#$00,#$0d,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$00,#$1b,#$00,#$00,#$00,#$00,
      #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$00,#$28,#$29,#$2a,#$2b,#$00,#$2d,#$2e,#$2f,
      #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3a,#$00,#$3c,#$00,#$3e,#$3f,
      #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4a,#$4b,#$4c,#$4d,#$4e,#$4f,
      #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5a,#$00,#$00,#$00,#$5e,#$5f,
      #$00,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6a,#$6b,#$6c,#$6d,#$6e,#$6f,
      #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7a,#$7b,#$7c,#$7d,#$7e,#$08);

    cAscii   : array [0..127] of AnsiChar = (
      #$00,#$00,#$00,#$00,#$04,#$00,#$00,#$00,#$08,#$09,#$00,#$00,#$00,#$0d,#$00,#$00,
      #$00,#$00,#$00,#$00,#$00,#$15,#$00,#$00,#$00,#$00,#$00,#$1b,#$00,#$00,#$00,#$00,
      #$20,#$00,#$00,#$00,#$00,#$00,#$00,#$27,#$00,#$00,#$2a,#$2b,#$2c,#$2d,#$2e,#$2f,
      #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$00,#$3b,#$3c,#$3d,#$00,#$00,
      #$00,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4a,#$4b,#$4c,#$4d,#$4e,#$4f,
      #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5a,#$5b,#$5c,#$5d,#$00,#$00,
      #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6a,#$6b,#$6c,#$6d,#$6e,#$6f,
      #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7a,#$00,#$00,#$00,#$00,#$08);

    cScanValue  : array [0..127] of byte = (
       $fe, $00, $00, $00, $20, $00, $00, $00, $0e, $0f, $00, $00, $00, $1c, $00, $00,
       $00, $00, $00, $00, $00, $16, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00,
       $39, $00, $00, $00, $00, $00, $00, $28, $00, $00, $37, $0d, $33, $0c, $34, $35,
       $0b, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $00, $27, $33, $0d, $00, $00,
       $00, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $00, $00,
       $2b, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $00, $00, $00, $00, $0e);

    cShiftScanValue  : array [0..127] of byte = (
       $fe, $00, $00, $00, $00, $00, $00, $00, $0e, $0f, $00, $00, $00, $1c, $00, $00,
       $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00,
       $39, $02, $28, $04, $05, $06, $08, $00, $0a, $0b, $09, $0d, $00, $0c, $53, $35,
       $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $27, $00, $33, $00, $34, $35,
       $03, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $00, $00, $00, $07, $0c,
       $00, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $29, $0e);

    cAltScanValue  : array [0..127] of byte = (
       $fe, $00, $00, $00, $00, $00, $00, $00, $0e, $00, $00, $00, $00, $1c, $00, $00,
       $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00,
       $39, $78, $28, $7a, $7b, $7c, $7e, $28, $80, $81, $7f, $83, $33, $82, $34, $35,
       $81, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f, $80, $27, $27, $33, $83, $34, $35,
       $79, $1e, $30, $2e, $20, $12, $21, $00, $00, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $7d, $82,
       $2b, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $29, $0e);

    cCtrlScanValue  : array [0..127] of byte = (
       $fe, $00, $00, $00, $00, $00, $00, $00, $0e, $94, $00, $00, $00, $1c, $00, $00,
       $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00,
       $39, $02, $28, $04, $05, $06, $08, $28, $0a, $0b, $96, $0d, $33, $0c, $34, $35,
       $0b, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $27, $27, $33, $0d, $34, $35,
       $03, $1e, $30, $2e, $20, $12, $21, $00, $00, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $07, $0c,
       $29, $1e, $30, $2e, $20, $12, $21, $22, $23, $17, $24, $25, $26, $32, $31, $18,
       $19, $10, $13, $1f, $14, $16, $2f, $11, $2d, $15, $2c, $1a, $2b, $1b, $29, $0e);


procedure buildKeyEvent(modifier:dword; nKey:dword);
var k : TEnhancedKeyEvent;
    SState: TEnhancedShiftState;
    ScanValue : byte;
begin
  k:=NilEnhancedKeyEvent;
  AltPrefix := 0;
  ShiftPrefix := 0;
  CtrlPrefix := 0;

  { Shift states}
  if modifier =  0 then modifier:=1;
  modifier:=modifier-1;

  SState:=[];
  if (modifier and 1)>0 then SState:=SState+[essShift];
  if (modifier and 2)>0 then SState:=SState+[essAlt];
  if (modifier and 4)>0 then SState:=SState+[essCtrl];
  k.ShiftState:=SState;

  if nKey < 128 then
  begin
    if essAlt in SState then
       k.AsciiChar:=cAltAscii[nKey]
    else if essCtrl in SState then
       k.AsciiChar:=cCtrlAscii[nKey]
    else if essShift in SState then
      k.AsciiChar:=cShiftAscii[nKey]
    else
      k.AsciiChar:=cAscii[nKey];

    if essAlt in SState then
       ScanValue :=cAltScanValue[nKey]
    else if essCtrl in SState then
       ScanValue :=cCtrlScanValue[nKey]
    else if essShift in SState then
      ScanValue :=cShiftScanValue[nKey]
    else
      ScanValue :=cScanValue[nKey];

    k.UnicodeChar := WideChar(k.AsciiChar);
    k.VirtualScanCode := (ScanValue shl 8) or Ord(k.AsciiChar);

    PushKey(k);
    if byte(k.AsciiChar) = 27 then PushKey(k);
  end else
    PushUnicodeKey (k,nKey,'?');
end;

procedure xterm_ModifyOtherKeys;
{ format: CSI 27 ; modifier ; number ~ }
var ch : AnsiChar;
    fdsin : tfdSet;
    st: string[31];
    modifier : dword;
    nKey : dword;
    nr : byte;
    i : dword;
begin
  fpFD_ZERO(fdsin);
  fpFD_SET(StdInputHandle,fdsin);

  nr:=0;
  modifier:=1;
  nKey:=0;
  st:='0';
  repeat
     if inhead=intail then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     if ch in [';','~'] then
     begin
       if nr = 0 then val(st,modifier,i);
       if nr = 1 then val(st,nKey,i);
       inc(nr);
       st:='0';
     end else
     begin
       if not (ch in ['0'..'9']) then break;
       st:=st+ch;
     end;
  until ch='~';

  {test for validity}
  if ch<>'~' then exit;
  if nr<> 2  then exit;

  buildKeyEvent(modifier,nKey);
end;


procedure LoadDefaultSequences;

var i:cardinal;

begin
  AddSpecialSequence(#27'[M',@GenMouseEvent);
  AddSpecialSequence(#27'[<',@GenMouseEvent_ExtendedSGR1006);
  if not isKittyKeys then
    AddSpecialSequence(#27'[27;',@xterm_ModifyOtherKeys);

  {Unix backspace/delete hell... Is #127 a backspace or delete?}
  if copy(fpgetenv('TERM'),1,4)='cons' then
    begin
      {FreeBSD is until now only terminal that uses it for delete.}
      DoAddSequence(#127,0,kbDel,[]);              {Delete}
      DoAddSequence(#27#127,0,kbAltDel,[essAlt]);  {Alt+delete}
    end
  else
    begin
      DoAddSequence(#127,8,0,[]);                  {Backspace}
      DoAddSequence(#27#127,0,kbAltBack,[essAlt]); {Alt+backspace}
    end;
  { all Esc letter }
  for i:=low(key_sequences) to high(key_sequences) do
    with key_sequences[i] do
      DoAddSequence(st,AnsiChar,scan,shift);
  if detect_terminal in [trRxvt] then
  begin  {rxvt specific escape sequences}
    for i:=low(rxvt_key_sequences) to high(rxvt_key_sequences) do
    with rxvt_key_sequences[i] do
      DoAddSequence(st,AnsiChar,scan,shift);
  end;
  sunKeySquences;
end;

function RawReadKey:AnsiChar;
var
  fdsin    : tfdSet;
begin
  {Check Buffer first}
{  if KeySend<>KeyPut then
   begin
     RawReadKey:=PopKey;
     exit;
   end;}
  {Wait for Key}
  if not sysKeyPressed then
   begin
     fpFD_ZERO (fdsin);
     fpFD_SET (StdInputHandle,fdsin);
     fpSelect (StdInputHandle+1,@fdsin,nil,nil,nil);
   end;
  RawReadKey:=ttyRecvChar;
end;


function RawReadString : shortstring;
var
  ch : AnsiChar;
  fdsin : tfdSet;
  St : shortstring;
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
     if length(st)=255 then break;
  Until ch=#0;
  RawReadString:=St;
end;


{$ifdef linux}
function ShiftState:byte;

var arg:longint;

begin
  shiftstate:=0;
  arg:=6;
  if fpioctl(StdInputHandle,TIOCLINUX,@arg)=0 then
   begin
     if (arg and 8)<>0 then
      shiftstate:=kbAlt;
     if (arg and 4)<>0 then
      inc(shiftstate,kbCtrl);
     { 2 corresponds to AltGr so set both kbAlt and kbCtrl PM }
     if (arg and 2)<>0 then
      shiftstate:=shiftstate or (kbAlt or kbCtrl);
     if (arg and 1)<>0 then
      inc(shiftstate,kbShift);
   end;
end;

function EnhShiftState:TEnhancedShiftState;
const
  KG_SHIFT     = 0;
  KG_CTRL      = 2;
  KG_ALT       = 3;
  KG_ALTGR     = 1;
  KG_SHIFTL    = 4;
  KG_KANASHIFT = 4;
  KG_SHIFTR    = 5;
  KG_CTRLL     = 6;
  KG_CTRLR     = 7;
  KG_CAPSSHIFT = 8;
var
  arg: longint;
begin
  EnhShiftState:=[];
  arg:=6;
  if fpioctl(StdInputHandle,TIOCLINUX,@arg)=0 then
   begin
     if (arg and (1 shl KG_ALT))<>0 then
       Include(EnhShiftState,essAlt);
     if (arg and (1 shl KG_CTRL))<>0 then
       Include(EnhShiftState,essCtrl);
     if (arg and (1 shl KG_CTRLL))<>0 then
       Include(EnhShiftState,essLeftCtrl);
     if (arg and (1 shl KG_CTRLR))<>0 then
       Include(EnhShiftState,essRightCtrl);
     if (arg and (1 shl KG_ALTGR))<>0 then
       Include(EnhShiftState,essAltGr);
     if (arg and (1 shl KG_SHIFT))<>0 then
       Include(EnhShiftState,essShift);
     if (arg and (1 shl KG_SHIFTL))<>0 then
       Include(EnhShiftState,essLeftShift);
     if (arg and (1 shl KG_SHIFTR))<>0 then
       Include(EnhShiftState,essRightShift);
   end;
end;

procedure force_linuxtty;

var s:string[15];
    handle:sizeint;
    thistty:shortstring;

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


function DetectUtf8ByteSequenceStart(ch: AnsiChar): LongInt;
begin
  if Ord(ch)<128 then
    DetectUtf8ByteSequenceStart:=1
  else if (Ord(ch) and %11100000)=%11000000 then
    DetectUtf8ByteSequenceStart:=2
  else if (Ord(ch) and %11110000)=%11100000 then
    DetectUtf8ByteSequenceStart:=3
  else if (Ord(ch) and %11111000)=%11110000 then
    DetectUtf8ByteSequenceStart:=4
  else
    DetectUtf8ByteSequenceStart:=0;
end;


function IsValidUtf8ContinuationByte(ch: AnsiChar): Boolean;
begin
  IsValidUtf8ContinuationByte:=(Ord(ch) and %11000000)=%10000000;
end;

const  cKeysUnicodePrivateBase = 57344; { unicode private area starts here}

      kCapsLock = 14;
      kScrollLock = 15;
      kNumLock = 16;
      kPrintScreen = 17;
      kPauseBreak = 18;
      kMenu = 19;

      {Numpad keys}
      kDecimal = 65;
      kDivide = 66;
      kMultiple = 67;
      kMinuss = 68;
      kPluss = 69;
      kEnter = 70;
      kEqual = 71;
      kSeperator = 72;
      kLeft = 73;
      kRight = 74;
      kUp = 75;
      kDown = 76;
      kPgUp = 77;
      kPgDown = 78;
      kHome = 79;
      kEnd = 80;
      kIns = 81;
      kDel = 82;
      kMiddle = 83;

      {modifyers}
      kShiftLeft = 97;
      kCtrlLeft = 98;
      kAltLeft = 99;
      kSuperLeft = 100;

      kShiftRight = 103;
      kCtrlRight = 104;
      kAltRight = 105;
      kSuperRight  = 106;
      kAltGr = 109;


const { lookup tables for ScanCodes of numpad keys}
      cKP_ScanVal : array [kDecimal..kMiddle] of byte = (
      { kDecimal = 65;   } $53,
      { kDivide = 66;    } $e0,
      { kMultiple = 67;  } $37,
      { kMinuss = 68;    } $4a,
      { kPluss = 69;     } $4e,
      { kEnter = 70;     } $e0,
      { kEqual = 71;     } $f0, {none}
      { kSeperator = 72; } $f1, {none}
      { kLeft = 73;      } $4b,
      { kRight = 74;     } $4d,
      { kUp = 75;        } $48,
      { kDown = 76;      } $50,
      { kPgUp = 77;      } $49,
      { kPgDown = 78;    } $51,
      { kHome = 79;      } $47,
      { kEnd = 80;       } $4f,
      { kIns = 81;       } $52,
      { kDel = 82;       } $53,
      { kMiddle = 83;    } $4c);

const cKP_CtrlScanVal : array [kDecimal..kMiddle] of byte = (
      { kDecimal = 65;   } $93,
      { kDivide = 66;    } $95,
      { kMultiple = 67;  } $96,
      { kMinuss = 68;    } $8e,
      { kPluss = 69;     } $90,
      { kEnter = 70;     } $e0,
      { kEqual = 71;     } $f0, {none}
      { kSeperator = 72; } $f1, {none}
      { kLeft = 73;      } $73,
      { kRight = 74;     } $74,
      { kUp = 75;        } $8d,
      { kDown = 76;      } $91,
      { kPgUp = 77;      } $84,
      { kPgDown = 78;    } $76,
      { kHome = 79;      } $77,
      { kEnd = 80;       } $75,
      { kIns = 81;       } $92,
      { kDel = 82;       } $93,
      { kMiddle = 83;    } $8f);

const cKP_AltScanVal : array [kDecimal..kMiddle] of byte = (
      { kDecimal = 65;   } $93,
      { kDivide = 66;    } $a4,
      { kMultiple = 67;  } $37,
      { kMinuss = 68;    } $4a,
      { kPluss = 69;     } $4e,
      { kEnter = 70;     } $a6,
      { kEqual = 71;     } $f0, {none}
      { kSeperator = 72; } $f1, {none}
      { kLeft = 73;      } $9b,
      { kRight = 74;     } $9d,
      { kUp = 75;        } $98,
      { kDown = 76;      } $a0,
      { kPgUp = 77;      } $99,
      { kPgDown = 78;    } $a1,
      { kHome = 79;      } $97,
      { kEnd = 80;       } $9f,
      { kIns = 81;       } $92,
      { kDel = 82;       } $93,
      { kMiddle = 83;    } $8f);

procedure buildKeyPadEvent(modifier:dword; nKey:dword; ch : AnsiChar);
var k : TEnhancedKeyEvent;
    SState: TEnhancedShiftState;
    ScanValue : byte;
begin
  k:=NilEnhancedKeyEvent;
  AltPrefix := 0;
  ShiftPrefix := 0;
  CtrlPrefix := 0;

  { Shift states}
  if modifier =  0 then modifier:=1;
  modifier:=modifier-1;

  SState:=[];
  if (modifier and 1)>0 then SState:=SState+[essShift];
  if (modifier and 2)>0 then SState:=SState+[essAlt];
  if (modifier and 4)>0 then SState:=SState+[essCtrl];
  k.ShiftState:=SState;

  if nKey < 128 then
  begin
    if nKey = kEnter then
    begin
       if essAlt in SState then
          ch:=#0
       else if essCtrl in SState then
          ch:=#$0a
       else if essShift in SState then
         ch:=#$0d
       else
         ch:=#$0d;
    end;
    if essAlt in SState then
       k.AsciiChar:=#0
    else if essCtrl in SState then
       k.AsciiChar:=ch
    else if essShift in SState then
      k.AsciiChar:=ch
    else
      k.AsciiChar:=ch;

    if essAlt in SState then
       ScanValue :=cKP_AltScanVal[nKey]
    else if essCtrl in SState then
       ScanValue :=cKP_CtrlScanVal[nKey]
    else if essShift in SState then
      ScanValue :=cKP_ScanVal[nKey]
    else
      ScanValue :=cKP_ScanVal[nKey];

    k.UnicodeChar := WideChar(k.AsciiChar);
    k.VirtualScanCode := (ScanValue shl 8) or Ord(k.AsciiChar);

    PushKey(k);
  end;
end;

function RemoveShiftState(AShiftState:TEnhancedShiftState; toRemoveState,aState,toTestState:TEnhancedShiftStateElement):TEnhancedShiftState;
{ remove state toRemoveState and                                  }
{ remove state aState if AShiftState does not contain toTestState }
begin
  AShiftState:=AShiftState-[toRemoveState];
  if not (toTestState in AShiftState) then
    AShiftState:=AShiftState-[aState];
  RemoveShiftState:=AShiftState;
end;

var LastShiftState, CurrentShiftState : TEnhancedShiftState;

function GetLastShiftState : byte;
{ get fake shift state or current shift state for kitty keys }
var State : byte;
begin
  State:=0;
  if isKittyKeys then
  begin
    LastShiftState:=CurrentShiftState;
    if essLeftShift in LastShiftState then
      inc(state,kbLeftShift);
    if essRightShift in LastShiftState then
      inc(state,kbRightShift);
    if (essShift in LastShiftState) and (not ((essRightShift in LastShiftState) or (essLeftShift in LastShiftState))) then
      inc(state,kbShift); {this for super rare case when shift state key press was not recived (maybe that is impossible)}
  end else
  if essShift in LastShiftState then
    inc(state,kbShift);
  if essCtrl in LastShiftState then
    inc(state,kbCtrl);
  if essAlt in LastShiftState then
    inc(state,kbAlt);
  GetLastShiftState:=State;
end;

procedure UpdateCurrentShiftState(nKey:longint; kbDown:byte);
begin
  {current shift state changes}
  if kbDown <3 then
  begin {state key down}
    case nKey of
      kShiftLeft  : CurrentShiftState:=CurrentShiftState +[essShift,essLeftShift];
      kShiftRight : CurrentShiftState:=CurrentShiftState +[essShift,essRightShift];
      kCtrlLeft   : CurrentShiftState:=CurrentShiftState +[essCtrl,essLeftCtrl];
      kCtrlRight  : CurrentShiftState:=CurrentShiftState +[essCtrl,essRightCtrl];
      kAltRight   : CurrentShiftState:=CurrentShiftState +[essAlt,essRightAlt];
      kAltLeft    : CurrentShiftState:=CurrentShiftState +[essAlt,essLeftAlt];
      kAltGr      : CurrentShiftState:=CurrentShiftState +[essAltGr];
    end;
  end else
  begin {state key up}
    case nKey of
      kShiftLeft  : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essLeftShift,essShift,essRightShift);
      kShiftRight : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essRightShift,essShift,essLeftShift);
      kCtrlLeft   : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essLeftCtrl,essCtrl,essRightCtrl);
      kCtrlRight  : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essRightCtrl,essCtrl,essLeftCtrl);
      kAltRight   : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essRightAlt,essAlt,essLeftAlt);
      kAltLeft    : CurrentShiftState:=RemoveShiftState(CurrentShiftState,essLeftAlt,essAlt,essRightAlt);
      kAltGr      : CurrentShiftState:=CurrentShiftState -[essAltGr];
    end;
  end;
end;

procedure UpdateShiftStateWithModifier(modifier:longint);
{ Sanity double check. In case if there is no generated shift state key release (shortcut key intercepted by OS or terminal). }
{ Make sure on key press there is correct current shift state }
begin
  { Shift states}
  if modifier =  0 then modifier:=1;
  modifier:=modifier-1;
  if (modifier and 1)>0 then
  begin if not (essShift in CurrentShiftState) then CurrentShiftState:=CurrentShiftState+[essShift];
  end else if (essShift in CurrentShiftState) then CurrentShiftState:=CurrentShiftState-[essLeftShift,essShift,essRightShift];
  if (modifier and 2)>0 then
  begin  if not (essAlt in CurrentShiftState) then CurrentShiftState:=CurrentShiftState+[essAlt];
  end else if (essAlt in CurrentShiftState) then CurrentShiftState:=CurrentShiftState-[essLeftAlt,essAlt,essRightAlt];
  if (modifier and 4)>0 then
  begin if not (essCtrl in CurrentShiftState) then CurrentShiftState:=CurrentShiftState+[essCtrl];
  end else if (essCtrl in CurrentShiftState) then CurrentShiftState:=CurrentShiftState-[essRightCtrl,essCtrl,essLeftCtrl];
end;


function ReadKey:TEnhancedKeyEvent;
const
  ReplacementAsciiChar='?';
var
  store    : array [0..31] of AnsiChar;
  arrayind : byte;
  SState: TEnhancedShiftState;

    procedure DecodeKittyKey(var k :TEnhancedKeyEvent; var NPT : PTreeElement);
    var i : dword;
        wc: wideChar;
        ch: AnsiChar;
        st:string[15];
        escStr:string[15];
        asIs:string[15];
        unicodeCodePoint : longint;
        z : longint;
        NNPT : PTreeElement;

        enh: array[0..11] of longint;

        iE : dword;
        kbDown : byte;
        nKey : longint;
        modifier: longint;
    begin   {
         if arrayind>0 then
         for i:= 0 to arrayind-1 do
         begin
            write(hexstr(byte(store[i]),2),' ');
         end;}
         iE:=0;
         fillchar(enh,sizeof(enh),$ff);
         enh[3]:=1;
         enh[4]:=1;
         st:='';
         asIs:='';

         if arrayind > 2 then
         for i:= 2 to arrayind-1 do
         begin
            ch:=store[i];
            asIs:=asIs+ch;
            if ch in ['0'..'9'] then st:=st+ch else
            begin
               if length(st)>0 then
               begin
                  val(st,unicodeCodePoint,z);
                  enh[iE]:=unicodeCodePoint;
               end;
               st:='';
               if ch =';' then begin iE:=((iE div 3)+1)*3; end else
               if ch =':' then inc(iE);
               if not (ch in [';',':']) then break;
            end;
         end;

         nKey:=enh[0];
         modifier:=((enh[3]-1)and 7)+1;
         kbDown:=enh[4];

         unicodeCodePoint:=enh[6];
         if unicodeCodePoint < 0 then
            unicodeCodePoint:=enh[0];
         enh[5]:=modifier;

         escStr:='';
         ch:=store[arrayind-1];

         if (ch='E') and (enh[6]>0) then ch:='u'; {this is one exception (numlock off, Shift+Center (numpad 5))}

         case ch of
            '~': begin
                   if kbDown<3 then
                   begin
                     str(nKey,st);
                     escStr:='_['+st;
                     if modifier>1 then
                     begin
                       str(modifier,st);
                       escStr:=escStr+';'+st;
                     end;
                     escStr:=escStr+ch;
                     for i:=2 to length(escStr) do
                     begin
                       ch:=escStr[i];
                       NPT:=FindChild(ord(ch),NPT);
                       if not assigned(NPT) then
                       begin
                         break;
                       end;
                     end;
                   end else NPT:=nil;
                 end;

            'A','B','C','D','E','F','H','P','Q','S':
                 begin
                   if kbDown<3 then
                   begin
                     escStr:='_[';
                     if modifier>1 then
                     begin
                       str(modifier,st);
                       escStr:=escStr+'1;'+st;
                     end;
                     escStr:=escStr+ch;
                     for i:=2 to length(escStr) do
                     begin
                       ch:=escStr[i];
                       NPT:=FindChild(ord(ch),NPT);
                       if not assigned(NPT) then
                       begin
                         break;
                       end;
                     end;
                   end else NPT:=nil;
                 end;

         otherwise
               NPT:=nil;
         end;

         UpdateShiftStateWithModifier(modifier);
         if kbDown =3 then arrayind:=0; {release keys are ignored}

         if not assigned(NPT) and (ch='u') then
         begin
           if (unicodeCodePoint >=57344) and (unicodeCodePoint<=63743) then
           begin
             {function keys have been pressed}
             arrayind:=0;
             nKey:=unicodeCodePoint-cKeysUnicodePrivateBase;
             if (nKey >=kShiftLeft) and (nKey<=kAltGr) then
               UpdateCurrentShiftState(nKey,kbDown);
             if (nKey < 128) and (kbDown <3) then
             begin
               if nKey = 60 then nKey:= kMiddle; {KP_5 -> KP_BEGIN}
               if nKey in [kDecimal..kMiddle] then
               begin
                 buildKeyPadEvent(modifier,nKey,#0);
                 exit;
               end else exit;
             end else
             {ignore...}
             exit;
           end;

           if kbDown =3 then exit; {key up... ignored}

           nKey:=unicodeCodePoint;

           if nKey>-1 then
              buildKeyEvent(modifier,nKey);
           arrayind:=0;
        end;
    end;

    procedure RestoreArray;
      var
        i : byte;
        k : TEnhancedKeyEvent;
      begin
        if arrayind>0 then
        for i:=0 to arrayind-1 do
        begin
          k := NilEnhancedKeyEvent;
          k.AsciiChar := store[i];
          k.VirtualScanCode := Ord(k.AsciiChar);
          k.ShiftState := SState;
          { todo: how to set the other fields? }
          PushKey(k);
        end;
      end;

    function ReadUtf8(ch: AnsiChar): LongInt;
      const
        ErrorCharacter = $FFFD; { U+FFFD = REPLACEMENT CHARACTER }
      var
        CodePoint: LongInt;
      begin
        ReadUtf8:=ErrorCharacter;
        case DetectUtf8ByteSequenceStart(ch) of
          1: ReadUtf8:=Ord(ch);
          2:begin
              CodePoint:=(Ord(ch) and %00011111) shl 6;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=(Ord(ch) and %00111111) or CodePoint;
              if (CodePoint>=$80) and (CodePoint<=$7FF) then
                ReadUtf8:=CodePoint;
            end;
          3:begin
              CodePoint:=(Ord(ch) and %00001111) shl 12;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=((Ord(ch) and %00111111) shl 6) or CodePoint;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=(Ord(ch) and %00111111) or CodePoint;
              if ((CodePoint>=$800) and (CodePoint<=$D7FF)) or
                 ((CodePoint>=$E000) and (CodePoint<=$FFFF)) then
                ReadUtf8:=CodePoint;
            end;
          4:begin
              CodePoint:=(Ord(ch) and %00000111) shl 18;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=((Ord(ch) and %00111111) shl 12) or CodePoint;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=((Ord(ch) and %00111111) shl 6) or CodePoint;
              ch:=ttyRecvChar;
              if not IsValidUtf8ContinuationByte(ch) then
                exit;
              CodePoint:=(Ord(ch) and %00111111) or CodePoint;
              if (CodePoint>=$10000) and (CodePoint<=$10FFFF) then
                ReadUtf8:=CodePoint;
            end;
        end;
      end;

var
  ch       : AnsiChar;
  fdsin    : tfdSet;
  NPT,NNPT : PTreeElement;
  RootNPT  : PTreeElement;
  FoundNPT : PTreeElement;
  k: TEnhancedKeyEvent;
  UnicodeCodePoint: LongInt;
  i : dword;
begin
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
  k:=NilEnhancedKeyEvent;

{$ifdef linux}
  if is_console then
    SState:=EnhShiftState
  else
{$endif}
    SState:=[];
  k.ShiftState:=SState;
  ch:=ttyRecvChar;
  k.AsciiChar:=ch;
  NPT:=RootTree[ch];
  if not assigned(NPT) then
    begin
      if Utf8KeyboardInputEnabled then
        begin
          UnicodeCodePoint:=ReadUtf8(ch);
          PushUnicodeKey(k,UnicodeCodePoint,ReplacementAsciiChar);
        end
      else
        PushKey(k);
    end
  else
    begin
      fpFD_ZERO(fdsin);
      fpFD_SET(StdInputHandle,fdsin);
      store[0]:=ch;
      arrayind:=1;
      RootNPT:=NPT;
      FoundNPT:=nil;
      if NPT^.CanBeTerminal then FoundNPT:=NPT;

      while {assigned(NPT) and} syskeypressed do
        begin
          if inhead=intail then
            fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
          ch:=ttyRecvChar;
          if (ch=#27) and double_esc_hack_enabled then
            begin
              {This is the same hack as in findsequence; see findsequence for
               explanation.}
              ch:=ttyrecvchar;
              {Alt+O cannot be used in this situation, it can be a function key.}
              if not(ch in ['a'..'z','A'..'N','P'..'Z','0'..'9','-','+','_','=']) then
                begin
                  PutBackIntoInBuf(ch);
                  ch:=#27;
                end
              else
                begin
                  write(#27'[?1036l');
                  double_esc_hack_enabled:=false;
                end;
            end;
          {save char later use }
          store[arrayind]:=ch;
          inc(arrayind);
          if arrayind >= 31 then break;
          {check tree for maching sequence}
          if assigned(NPT) then
            NNPT:=FindChild(ord(ch),NPT);
          if assigned(NNPT) then
            begin
              NPT:=NNPT;
              if NPT^.CanBeTerminal then
              begin
                FoundNPT:=NPT;
                if assigned(NPT^.SpecialHandler) then
                  break;
              end;
            End
          else
            NPT:=nil;  {not found and not looking for anymore, but don't let hope fade... read sequence till the end}
          {check sequnce end conditions}
          if (arrayind>2) and  (ch < #32) then
          begin
            {if two short escape sequences are back to back}
            PutBackIntoInBuf(ch); { rolling back }
            dec(arrayind);
            break;
          end;
          if (arrayind>3) and not (ch in [';',':','0'..'9']) then break; {end of escape sequence}
        end;

        if (arrayind>3) then
          if (ch = 'u'  )   { for sure kitty keys  or }
              or ( isKittyKeys and  not assigned(FoundNPT) ) {probally kitty keys}
              then
            begin
              if not (assigned(FoundNPT) and  assigned(FoundNPT^.SpecialHandler)) then
                begin
                  FoundNPT:=RootNPT;
                  DecodeKittyKey(k,FoundNPT);
                end;
            end;

       NPT:=FoundNPT;
       if assigned(NPT) and NPT^.CanBeTerminal then
        begin
          if assigned(NPT^.SpecialHandler) then
            begin
              NPT^.SpecialHandler;
              k.AsciiChar := #0;
              k.UnicodeChar := WideChar(#0);
              k.VirtualScanCode := 0;
              PushKey(k);
            end
          else if (NPT^.CharValue<>0) or (NPT^.ScanValue<>0) then
            begin
              k.AsciiChar := chr(NPT^.CharValue);
              k.UnicodeChar := WideChar(NPT^.CharValue);
              k.VirtualScanCode := (NPT^.ScanValue shl 8) or Ord(k.AsciiChar);
              k.ShiftState:=k.ShiftState+NPT^.ShiftValue;
              PushKey(k);
            end;
        end
      else
        RestoreArray;
   end;
{$ifdef logging}
       writeln(f);
{$endif logging}
  ReadKey:=PopKey;
End;

procedure KittyKeyAvailability;
var st,zt : shortstring;
    i: integer;
    ch : AnsiChar;
begin
  if (kitty_keys_yes=kitty_keys_no) then {make this test just once}
  begin
    write(#27'[?u');   { request response! }
    write(#27'[c');    { request device status (DA1) to get at least some answer. }
    st:=RawReadString; { read the answer }
    isKittyKeys:=false;
    if length(st)>0 then
    begin
      zt:='';
      for i:=1 to length(st) do
      begin
        ch:=st[i];
        if ch = #27 then
        begin
          if zt = #27'[?31u' then
          begin
            isKittyKeys:=true;   {kitty keys supported and enabled}
          end;
          zt:='';
        end;
        zt:=zt+ch;
      end;
      if zt =#27'[?31u' then
      begin
        isKittyKeys:=true;   {kitty keys supported and enabled}
      end;
      kitty_keys_yes:= isKittyKeys;
      kitty_keys_no := not isKittyKeys;
    end;
  end;
end;

procedure waitAndReadAfterArtifacts;
var st : shortstring;
  timewait,finalparsec : TimeSpec;
  ree : longint;
begin
  if not kitty_keys_yes then exit;
  timewait.tv_sec := 0;
  timewait.tv_nsec := 100000000; {few nano seconds to wait}
  ree:=fpNanoSleep(@timewait,@finalparsec);
  st:='';
  if syskeypressed then st:=RawReadString; {empty key buffer (key realeas might be pending)}
end;

{ Exported functions }

procedure SysInitKeyboard;
begin
  isKittyKeys:=false;
  CurrentShiftState:=[];
  PendingEnhancedKeyEvent:=NilEnhancedKeyEvent;
  Utf8KeyboardInputEnabled:={$IFDEF FPC_DOTTEDUNITS}System.Console.{$ENDIF}UnixKVMBase.UTF8Enabled;
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
      if copy(fpgetenv('TERM'),1,5)='xterm' then
          {The alt key should generate an escape prefix. Save the old setting
           make make it send that escape prefix.}
        begin
          write(#27'[?1036s'#27'[?1036h');
          double_esc_hack_enabled:=true;
        end;
      {kitty_keys_no:=true;}
      isKittyKeys:=kitty_keys_yes;
      if kitty_keys_yes or (kitty_keys_yes=kitty_keys_no) then
         write(#27'[>31u'); { try to set up kitty keys }
      KittyKeyAvailability;
      if not isKittyKeys then
        write(#27'[>4;2m'); { xterm ->  modifyOtherKeys }
{$ifdef linux}
    end;
{$endif}
  LoadDefaultSequences;
{  LoadTerminfoSequences;}
end;


procedure SysDoneKeyboard;
begin
{$ifdef linux}
  if is_console then
  unpatchkeyboard;
{$endif linux}
  if not isKittyKeys then
    write(#27'[>4m'); { xterm -> reset to default modifyOtherKeys }
  if kitty_keys_yes then
  begin
    write(#27'[<u'); {if we have kitty keys, disable them}
    waitAndReadAfterArtifacts;
    isKittyKeys:=false;
  end;

  if copy(fpgetenv('TERM'),1,5)='xterm' then
     {Restore the old alt key behaviour.}
     write(#27'[?1036r');

  SetRawMode(false);

  FreeTree;
{$ifdef logging}
  close(f);
{$endif logging}
end;


function SysGetEnhancedKeyEvent: TEnhancedKeyEvent;

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
   (kbAltHome,kbAltUp,kbAltPgUp,{kbNoKey}$4a,kbAltLeft,
    kbAltCenter,kbAltRight,kbAltGrayPlus,kbAltEnd,
    kbAltDown,kbAltPgDn,kbAltIns,kbAltDel);
var
  MyScan:byte;
  MyChar : AnsiChar;
  MyUniChar: WideChar;
  MyKey: TEnhancedKeyEvent;
  EscUsed,AltPrefixUsed,CtrlPrefixUsed,ShiftPrefixUsed,Again : boolean;
  SState: TEnhancedShiftState;

begin {main}
  if PendingEnhancedKeyEvent<>NilEnhancedKeyEvent then
    begin
      SysGetEnhancedKeyEvent:=PendingEnhancedKeyEvent;
      LastShiftState:=SysGetEnhancedKeyEvent.ShiftState; {to fake shift state later}
      PendingEnhancedKeyEvent:=NilEnhancedKeyEvent;
      exit;
    end;
  SysGetEnhancedKeyEvent:=NilEnhancedKeyEvent;
  MyKey:=ReadKey;
  MyChar:=MyKey.AsciiChar;
  MyUniChar:=MyKey.UnicodeChar;
  MyScan:=MyKey.VirtualScanCode shr 8;
  Sstate:=MyKey.ShiftState;
  CtrlPrefixUsed:=false;
  AltPrefixUsed:=false;
  ShiftPrefixUsed:=false;
  EscUsed:=false;
  repeat
    again:=false;
    if Mychar=#0 then
      begin
        { Handle Ctrl-<x>, but not AltGr-<x> }
        if (essCtrl in SState) and (not (essAlt in SState))  then
          case MyScan of
            kbShiftTab: MyScan := kbCtrlTab;
            kbHome..kbDel : { cArrow }
              MyScan:=CtrlArrow[MyScan];
            kbF1..KbF10 : { cF1-cF10 }
              MyScan:=MyScan+kbCtrlF1-kbF1;
            kbF11..KbF12 : { cF11-cF12 }
              MyScan:=MyScan+kbCtrlF11-kbF11;
          end
        { Handle Alt-<x>, but not AltGr }
        else if (essAlt in SState) and (not (essCtrl in SState)) then
          case MyScan of
            kbShiftTab: MyScan := kbAltTab;
            kbHome..kbDel : { AltArrow }
              MyScan:=AltArrow[MyScan];
            kbF1..KbF10 : { aF1-aF10 }
              MyScan:=MyScan+kbAltF1-kbF1;
            kbF11..KbF12 : { aF11-aF12 }
              MyScan:=MyScan+kbAltF11-kbF11;
          end
        else if essShift in SState then
          case MyScan of
            kbIns: MyScan:=kbShiftIns;
            kbDel: MyScan:=kbShiftDel;
            kbF1..KbF10 : { sF1-sF10 }
              MyScan:=MyScan+kbShiftF1-kbF1;
            kbF11..KbF12 : { sF11-sF12 }
              MyScan:=MyScan+kbShiftF11-kbF11;
          end;
        if myscan=kbAltBack then
          Include(sstate, essAlt);
        if (MyChar<>#0) or (MyUniChar<>WideChar(0)) or (MyScan<>0) or (SState<>[]) then
          begin
            SysGetEnhancedKeyEvent.AsciiChar:=MyChar;
            SysGetEnhancedKeyEvent.UnicodeChar:=MyUniChar;
            SysGetEnhancedKeyEvent.ShiftState:=SState;
            SysGetEnhancedKeyEvent.VirtualScanCode:=(MyScan shl 8) or Ord(MyChar);
          end;
        LastShiftState:=SysGetEnhancedKeyEvent.ShiftState; {to fake shift state later}
        exit;
      end
    else if MyChar=#27 then
      begin
        if EscUsed then
          SState:=SState-[essAlt,essLeftAlt,essRightAlt]
        else
          begin
            Include(SState,essAlt);
            Again:=true;
            EscUsed:=true;
          end;
      end
    else if (AltPrefix<>0) and (MyChar=chr(AltPrefix)) then
      begin { ^Z - replace Alt for Linux OS }
        if AltPrefixUsed then
          SState:=SState-[essAlt,essLeftAlt,essRightAlt]
        else
          begin
            AltPrefixUsed:=true;
            Include(SState,essAlt);
            Again:=true;
          end;
      end
    else if (CtrlPrefix<>0) and (MyChar=chr(CtrlPrefix)) then
      begin
        if CtrlPrefixUsed then
          SState:=SState-[essCtrl,essLeftCtrl,essRightCtrl]
        else
          begin
            CtrlPrefixUsed:=true;
            Include(SState,essCtrl);
            Again:=true;
          end;
      end
    else if (ShiftPrefix<>0) and (MyChar=chr(ShiftPrefix)) then
      begin
        if ShiftPrefixUsed then
          SState:=SState-[essShift,essLeftShift,essRightShift]
        else
          begin
            ShiftPrefixUsed:=true;
            Include(SState,essShift);
            Again:=true;
          end;
      end;
    if again then
      begin
        MyKey:=ReadKey;
        MyChar:=MyKey.AsciiChar;
        MyUniChar:=MyKey.UnicodeChar;
        MyScan:=MyKey.VirtualScanCode shr 8;
      end;
  until not Again;
  if MyScan = 0 then
      MyScan:=EvalScan(ord(MyChar));
  if (essCtrl in SState) and (not (essAlt in SState)) then
    begin
      if (MyChar=#9) and (MyScan <> $17) then
        begin
          MyChar:=#0;
          MyUniChar:=WideChar(0);
          MyScan:=kbCtrlTab;
        end;
    end
  else if (essAlt in SState) and (not (essCtrl in SState)) then
    begin
      if (MyChar=#9) and (MyScan <> $17) then
        begin
          MyChar:=#0;
          MyUniChar:=WideChar(0);
          MyScan:=kbAltTab;
        end
      else if (MyScan <> $17) then
        begin
          if MyScan in [$02..$0D] then
            inc(MyScan,$76);
          MyChar:=chr(0);
          MyUniChar:=WideChar(0);
        end;
    end
  else if essShift in SState then
    if (MyChar=#9) and (MyScan <> $17) then
      begin
        MyChar:=#0;
        MyUniChar:=WideChar(0);
        MyScan:=kbShiftTab;
      end;
  if (MyChar<>#0) or (MyUniChar<>WideChar(0)) or (MyScan<>0) or (SState<>[]) then
    begin
      SysGetEnhancedKeyEvent.AsciiChar:=MyChar;
      SysGetEnhancedKeyEvent.UnicodeChar:=MyUniChar;
      SysGetEnhancedKeyEvent.ShiftState:=SState;
      SysGetEnhancedKeyEvent.VirtualScanCode:=(MyScan shl 8) or Ord(MyChar);
    end;
  LastShiftState:=SysGetEnhancedKeyEvent.ShiftState; {to fake shift state later}
end;


function SysPollEnhancedKeyEvent: TEnhancedKeyEvent;
var
  KeyEvent : TEnhancedKeyEvent;
begin
  if PendingEnhancedKeyEvent<>NilEnhancedKeyEvent then
    SysPollEnhancedKeyEvent:=PendingEnhancedKeyEvent
  else if keypressed then
    begin
      KeyEvent:=SysGetEnhancedKeyEvent;
      PendingEnhancedKeyEvent:=KeyEvent;
      SysPollEnhancedKeyEvent:=KeyEvent;
    end
  else
    SysPollEnhancedKeyEvent:=NilEnhancedKeyEvent;
  LastShiftState:=SysPollEnhancedKeyEvent.ShiftState; {to fake shift state later}
end;


function SysGetShiftState  : Byte;
begin
{$ifdef linux}
  if is_console then
    SysGetShiftState:=ShiftState
  else
{$endif}
    SysGetShiftState:=GetLastShiftState;
end;


procedure RestoreStartMode;
begin
  TCSetAttr(1,TCSANOW,StartTio);
end;


const
  SysKeyboardDriver : TKeyboardDriver = (
    InitDriver : @SysInitKeyBoard;
    DoneDriver : @SysDoneKeyBoard;
    GetKeyevent : Nil;
    PollKeyEvent : Nil;
    GetShiftState : @SysGetShiftState;
    TranslateKeyEvent : Nil;
    TranslateKeyEventUnicode : Nil;
    GetEnhancedKeyEvent : @SysGetEnhancedKeyEvent;
    PollEnhancedKeyEvent : @SysPollEnhancedKeyEvent;
  );

begin
  SetKeyBoardDriver(SysKeyBoardDriver);
  TCGetAttr(1,StartTio);
end.
