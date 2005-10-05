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
interface

{$i keybrdh.inc}

Const
  AltPrefix : byte = 0;
  ShiftPrefix : byte = 0;
  CtrlPrefix : byte = 0;

Function RawReadKey:char;
Function RawReadString : String;
Function KeyPressed : Boolean;
{$ifndef NotUseTree}
Procedure AddSequence(Const St : String; AChar,AScan :byte);
Function FindSequence(Const St : String;var AChar, Ascan : byte) : boolean;
{$endif NotUseTree}
procedure RestoreStartMode;


implementation

uses
  Mouse,
{$ifndef NotUseTree}
  Strings,
  TermInfo,
{$endif NotUseTree}
  termio,baseUnix;

{$i keyboard.inc}

var
  OldIO,StartTio : TermIos;
{$ifdef logging}
  f : text;
{$endif logging}
{$i keyscan.inc}

{$ifdef Unused}
type
   TKeyState = Record
      Normal, Shift, Ctrl, Alt : word;
     end;

Const
  KeyStates : Array[0..255] of TKeyState
    (

    );

{$endif Unused}

Procedure SetRawMode(b:boolean);
Var
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

const
  kbdchanges=10;
  kbdchange:array[1..kbdchanges] of chgentry=(
    (tab:8; idx:$3b; oldtab:0; oldidx:$3b; oldval:0; newval:0),
    (tab:8; idx:$3c; oldtab:0; oldidx:$3c; oldval:0; newval:0),
    (tab:8; idx:$3d; oldtab:0; oldidx:$3d; oldval:0; newval:0),
    (tab:8; idx:$3e; oldtab:0; oldidx:$3e; oldval:0; newval:0),
    (tab:8; idx:$3f; oldtab:0; oldidx:$3f; oldval:0; newval:0),
    (tab:8; idx:$40; oldtab:0; oldidx:$40; oldval:0; newval:0),
    (tab:8; idx:$41; oldtab:0; oldidx:$41; oldval:0; newval:0),
    (tab:8; idx:$42; oldtab:0; oldidx:$42; oldval:0; newval:0),
    (tab:8; idx:$43; oldtab:0; oldidx:$43; oldval:0; newval:0),
    (tab:8; idx:$44; oldtab:0; oldidx:$44; oldval:0; newval:0)
  );
 KDGKBENT=$4B46;
 KDSKBENT=$4B47;
 KDGKBMETA=$4B62;
 KDSKBMETA=$4B63;
 K_ESCPREFIX=$4;
 K_METABIT=$3;

const
  oldmeta : longint = 0;
  meta : longint = 0;

procedure PatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  i : longint;
begin
  fpIoctl(stdinputhandle,KDGKBMETA,@oldmeta);
  meta:=K_ESCPREFIX;
  fpIoctl(stdinputhandle,KDSKBMETA,@meta);
  for i:=1 to kbdchanges do
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
  for i:=1 to kbdchanges do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.newval;
     fpioctl(stdinputhandle,KDSKBENT,@entry);
   end;
end;


procedure UnpatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  i : longint;
begin
  if oldmeta in [K_ESCPREFIX,K_METABIT] then
    fpioctl(stdinputhandle,KDSKBMETA,@oldmeta);
  for i:=1 to kbdchanges do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.oldval;
     fpioctl(stdinputhandle,KDSKBENT,@entry);
   end;
end;



{ Buffered Input routines }
const
  InSize=256;
var
  InBuf  : array [0..InSize-1] of char;
  InCnt,
  InHead,
  InTail : longint;

function ttyRecvChar:char;
var
  Readed,i : longint;
begin
{Buffer Empty? Yes, Input from StdIn}
  if (InHead=InTail) then
   begin
   {Calc Amount of Chars to Read}
     i:=InSize-InHead;
     if InTail>InHead then
      i:=InTail-InHead;
   {Read}
     Readed:=fpRead(StdInputHandle,InBuf[InHead],i);
   {Increase Counters}
     inc(InCnt,Readed);
     inc(InHead,Readed);
   {Wrap if End has Reached}
     if InHead>=InSize then
      InHead:=0;
   end;
{Check Buffer}
  if (InCnt=0) then
   ttyRecvChar:=#0
  else
   begin
     ttyRecvChar:=InBuf[InTail];
     dec(InCnt);
     inc(InTail);
     if InTail>=InSize then
      InTail:=0;
   end;
end;


Const
  KeyBufferSize = 20;
var
  KeyBuffer : Array[0..KeyBufferSize-1] of Char;
  KeyPut,
  KeySend   : longint;

Procedure PushKey(Ch:char);
Var
  Tmp : Longint;
Begin
  Tmp:=KeyPut;
  Inc(KeyPut);
  If KeyPut>=KeyBufferSize Then
   KeyPut:=0;
  If KeyPut<>KeySend Then
   KeyBuffer[Tmp]:=Ch
  Else
   KeyPut:=Tmp;
End;


Function PopKey:char;
Begin
  If KeyPut<>KeySend Then
   Begin
     PopKey:=KeyBuffer[KeySend];
     Inc(KeySend);
     If KeySend>=KeyBufferSize Then
      KeySend:=0;
   End
  Else
   PopKey:=#0;
End;


Procedure PushExt(b:byte);
begin
  PushKey(#0);
  PushKey(chr(b));
end;


const
  AltKeyStr  : string[38]='qwertyuiopasdfghjklzxcvbnm1234567890-=';
  AltCodeStr : string[38]=#016#017#018#019#020#021#022#023#024#025#030#031#032#033#034#035#036#037#038+
                          #044#045#046#047#048#049#050#120#121#122#123#124#125#126#127#128#129#130#131;
Function FAltKey(ch:char):byte;
var
  Idx : longint;
Begin
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
  if (InCnt>0) then
   sysKeyPressed:=true
  else
   begin
     fpFD_ZERO(fdsin);
     fpFD_SET(StdInputHandle,fdsin);
     sysKeypressed:=(fpSelect(StdInputHandle+1,@fdsin,nil,nil,0)>0);
   end;
end;

Function KeyPressed:Boolean;
Begin
  Keypressed := (KeySend<>KeyPut) or sysKeyPressed;
End;


Function IsConsole : Boolean;
var
  ThisTTY: String[30];
begin
  IsConsole:=false;
  { check for tty }
  if (IsATTY(stdinputhandle)=1) then
   begin
     { running on a tty, find out whether locally or remotely }
     ThisTTY:=TTYName(stdinputhandle);
     if (Copy(ThisTTY, 1, 8) = '/dev/tty') and
        (ThisTTY[9] >= '0') and (ThisTTY[9] <= '9') then
       IsConsole:=true;
   end;
end;

Const
  LastMouseEvent : TMouseEvent =
  (
    Buttons : 0;
    X : 0;
    Y : 0;
    Action : 0;
  );

{$ifndef NotUseTree}

  procedure GenMouseEvent;
  var MouseEvent: TMouseEvent;
      ch : char;
      fdsin : tfdSet;
  begin
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle,fdsin);
    Fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
     if InCnt=0 then
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
     if InCnt=0 then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.x:=Ord(ch)-ord(' ')-1;
     if InCnt=0 then
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
  TProcedure = procedure;

  PTreeElement = ^TTreeElement;
  TTreeElement = record
    Next,Parent,Child :  PTreeElement;
    CanBeTerminal : boolean;
    char : byte;
    ScanValue : byte;
    CharValue : byte;
    SpecialHandler : TProcedure;
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

function DoAddSequence(Const St : String; AChar,AScan :byte) : PTreeElement;
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


procedure AddSequence(Const St : String; AChar,AScan :byte);
begin
  DoAddSequence(St,AChar,AScan);
end;

{ Returns the Child that as c as char if it exists }
Function FindChild(c : byte;Root : PTreeElement) : PTreeElement;
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

Function AddSpecialSequence(Const St : string;Proc : TProcedure) : PTreeElement;
var
  NPT : PTreeElement;
begin
  NPT:=DoAddSequence(St,0,0);
  NPT^.SpecialHandler:=Proc;
  AddSpecialSequence:=NPT;
end;

function FindSequence(Const St : String;var AChar,AScan :byte) : boolean;
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

Procedure LoadDefaultSequences;
begin
  AddSpecialSequence(#27'[M',@GenMouseEvent);
  { linux default values, the next setting is
    compatible with xterms from XFree 4.x }
  DoAddSequence(#127,8,0);
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
  { vt100 default values }
  DoAddSequence(#27'[[A',0,kbF1);
  DoAddSequence(#27'[[B',0,kbF2);
  DoAddSequence(#27'[[C',0,kbF3);
  DoAddSequence(#27'[[D',0,kbF4);
  DoAddSequence(#27'[[E',0,kbF5);
  DoAddSequence(#27'[17~',0,kbF6);
  DoAddSequence(#27'[18~',0,kbF7);
  DoAddSequence(#27'[19~',0,kbF8);
  DoAddSequence(#27'[20~',0,kbF9);
  DoAddSequence(#27'[21~',0,kbF10);
  DoAddSequence(#27'[23~',0,kbF11);
  DoAddSequence(#27'[24~',0,kbF12);
  DoAddSequence(#27'[25~',0,kbShiftF3);
  DoAddSequence(#27'[26~',0,kbShiftF4);
  DoAddSequence(#27'[28~',0,kbShiftF5);
  DoAddSequence(#27'[29~',0,kbShiftF6);
  DoAddSequence(#27'[31~',0,kbShiftF7);
  DoAddSequence(#27'[32~',0,kbShiftF8);
  DoAddSequence(#27'[33~',0,kbShiftF9);
  DoAddSequence(#27'[34~',0,kbShiftF10);
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
  DoAddSequence(#27'[A',0,kbUp);
  DoAddSequence(#27'[B',0,kbDown);
  DoAddSequence(#27'[C',0,kbRight);
  DoAddSequence(#27'[D',0,kbLeft);
  DoAddSequence(#27'[F',0,kbEnd);
  DoAddSequence(#27'[H',0,kbHome);
  DoAddSequence(#27'[Z',0,kbShiftTab);
  DoAddSequence(#27'[5~',0,kbPgUp);
  DoAddSequence(#27'[6~',0,kbPgDn);
  DoAddSequence(#27'[4~',0,kbEnd);
  DoAddSequence(#27'[1~',0,kbHome);
  DoAddSequence(#27'[2~',0,kbIns);
  DoAddSequence(#27'[3~',0,kbDel);
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
  DoAddSequence(#27'OP',0,kbF1);
  DoAddSequence(#27'OQ',0,kbF2);
  DoAddSequence(#27'OR',0,kbF3);
  DoAddSequence(#27'OS',0,kbF4);
  DoAddSequence(#27'Ot',0,kbF5);
  DoAddSequence(#27'Ou',0,kbF6);
  DoAddSequence(#27'Ov',0,kbF7);
  DoAddSequence(#27'Ol',0,kbF8);
  DoAddSequence(#27'Ow',0,kbF9);
  DoAddSequence(#27'Ox',0,kbF10);
  DoAddSequence(#27'Oy',0,kbF11);
  DoAddSequence(#27'Oz',0,kbF12);
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
  DoAddSequence(#27'OA',0,kbUp);
  DoAddSequence(#27'OB',0,kbDown);
  DoAddSequence(#27'OC',0,kbRight);
  DoAddSequence(#27'OD',0,kbLeft);
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

function EnterEscapeSeqNdx(Ndx: Word;Char,Scan : byte) : PTreeElement;
var
  P,pdelay: PChar;
  St : string;
begin
  EnterEscapeSeqNdx:=nil;
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not record the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     St:=StrPas(p);
     EnterEscapeSeqNdx:=DoAddSequence(St,Char,Scan);
     if assigned(pdelay) then
       pdelay^:='$';
   end;
end;


Procedure LoadTermInfoSequences;
var
  err : longint;
begin
  if not assigned(cur_term) then
    setupterm(nil, stdoutputhandle, err);
  if not assigned(cur_term_Strings) then
    exit;
  EnterEscapeSeqNdx(key_f1,0,kbF1);
  EnterEscapeSeqNdx(key_f2,0,kbF2);
  EnterEscapeSeqNdx(key_f3,0,kbF3);
  EnterEscapeSeqNdx(key_f4,0,kbF4);
  EnterEscapeSeqNdx(key_f5,0,kbF5);
  EnterEscapeSeqNdx(key_f6,0,kbF6);
  EnterEscapeSeqNdx(key_f7,0,kbF7);
  EnterEscapeSeqNdx(key_f8,0,kbF8);
  EnterEscapeSeqNdx(key_f9,0,kbF9);
  EnterEscapeSeqNdx(key_f10,0,kbF10);
  EnterEscapeSeqNdx(key_f11,0,kbF11);
  EnterEscapeSeqNdx(key_f12,0,kbF12);
  EnterEscapeSeqNdx(key_up,0,kbUp);
  EnterEscapeSeqNdx(key_down,0,kbDown);
  EnterEscapeSeqNdx(key_left,0,kbLeft);
  EnterEscapeSeqNdx(key_right,0,kbRight);
  EnterEscapeSeqNdx(key_ppage,0,kbPgUp);
  EnterEscapeSeqNdx(key_npage,0,kbPgDn);
  EnterEscapeSeqNdx(key_end,0,kbEnd);
  EnterEscapeSeqNdx(key_home,0,kbHome);
  EnterEscapeSeqNdx(key_ic,0,kbIns);
  EnterEscapeSeqNdx(key_dc,0,kbDel);
  EnterEscapeSeqNdx(key_stab,0,kbShiftTab);
  { EnterEscapeSeqNdx(key_,0,kb);
  EnterEscapeSeqNdx(key_,0,kb); }
end;

{$endif not NotUseTree}

Function RawReadKey:char;
Var
  fdsin    : tfdSet;
Begin
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


Function RawReadString : String;
Var
  ch : char;
  fdsin : tfdSet;
  St : String;
Begin
  St:=RawReadKey;
  fpFD_ZERO (fdsin);
  fpFD_SET (StdInputHandle,fdsin);
  Repeat
     if InCnt=0 then
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


Function ReadKey(var IsAlt : boolean):char;
Var
  ch       : char;
{$ifdef NotUseTree}
  OldState : longint;
  State    : longint;
{$endif NotUseTree}
  is_delay : boolean;
  fdsin    : tfdSet;
  store    : array [0..8] of char;
  arrayind : byte;
{$ifndef NotUseTree}
  NPT,NNPT : PTreeElement;
{$else NotUseTree}
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
     if InCnt=0 then
       fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     ch:=ttyRecvChar;
     MouseEvent.x:=Ord(ch)-ord(' ')-1;
     if InCnt=0 then
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
{$endif NotUseTree}

    procedure RestoreArray;
      var
        i : byte;
      begin
        for i:=0 to arrayind-1 do
          PushKey(store[i]);
      end;

Begin
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
{$ifndef NotUseTree}
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
          if (InCnt=0) then
            fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
          ch:=ttyRecvChar;
          NNPT:=FindChild(ord(ch),NPT);
          if assigned(NNPT) then
            Begin
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
                  inc(InCnt);
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
{$else NotUseTree}
{Esc Found ?}
  If (ch=#27) then
   begin
     fpFD_ZERO(fdsin);
     fpFD_SET(StdInputHandle,fdsin);
     State:=1;
     store[0]:=#27;
     arrayind:=1;
{$ifdef logging}
     write(f,'Esc');
{$endif logging}
     if InCnt=0 then
      fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
     while (State<>0) and (sysKeyPressed) do
      begin
        ch:=ttyRecvChar;
        store[arrayind]:=ch;
        inc(arrayind);
{$ifdef logging}
        if ord(ch)>31 then
          write(f,ch)
        else
          write(f,'#',ord(ch):2);
{$endif logging}
        OldState:=State;
        State:=0;
        case OldState of
        1 : begin {Esc}
              case ch of
          'a'..'z',
          '0'..'9',
           '-','=' : PushExt(FAltKey(ch));
          'A'..'N',
          'P'..'Z' : PushExt(FAltKey(chr(ord(ch)+ord('a')-ord('A'))));
               #10 : PushKey(#10);
               #13 : PushKey(#10);
               #27 : begin
                       IsAlt:=True;
                       State:=1;
                     end;
              #127 : PushExt(kbAltDel);
               '[' : State:=2;
               'O' : State:=6;
               else
                 RestoreArray;
               end;
            end;
        2 : begin {Esc[}
              case ch of
               '[' : State:=3;
               'A' : PushExt(kbUp);
               'B' : PushExt(kbDown);
               'C' : PushExt(kbRight);
               'D' : PushExt(kbLeft);
               'F' : PushExt(kbEnd);
               'G' : PushKey('5');
               'H' : PushExt(kbHome);
               'K' : PushExt(kbEnd);
               'M' : State:=13;
               '1' : State:=4;
               '2' : State:=5;
               '3' : State:=12;{PushExt(kbDel)}
               '4' : PushExt(kbEnd);
               '5' : PushExt(73);
               '6' : PushExt(kbPgDn);
               '?' : State:=7;
              else
                RestoreArray;
              end;
              if ch in ['4'..'6'] then
               State:=255;
            end;
        3 : begin {Esc[[}
              case ch of
               'A' : PushExt(kbF1);
               'B' : PushExt(kbF2);
               'C' : PushExt(kbF3);
               'D' : PushExt(kbF4);
               'E' : PushExt(kbF5);
              else
                RestoreArray;
              end;
            end;
        4 : begin {Esc[1}
              case ch of
               '~' : PushExt(kbHome);
               '7' : PushExt(kbF6);
               '8' : PushExt(kbF7);
               '9' : PushExt(kbF8);
              else
                RestoreArray;
              end;
              if (Ch<>'~') then
               State:=255;
            end;
        5 : begin {Esc[2}
              case ch of
               '~' : PushExt(kbIns);
               '0' : pushExt(kbF9);
               '1' : PushExt(kbF10);
               '3' : PushExt($85){F11, but ShiftF1 also !!};
               '4' : PushExt($86){F12, but Shift F2 also !!};
               '5' : PushExt($56){ShiftF3};
               '6' : PushExt($57){ShiftF4};
               '8' : PushExt($58){ShiftF5};
               '9' : PushExt($59){ShiftF6};
              else
                RestoreArray;
              end;
              if (Ch<>'~') then
               State:=255;
            end;
        12 : begin {Esc[3}
              case ch of
               '~' : PushExt(kbDel);
               '1' : PushExt($5A){ShiftF7};
               '2' : PushExt($5B){ShiftF8};
               '3' : PushExt($5C){ShiftF9};
               '4' : PushExt($5D){ShiftF10};
              else
                RestoreArray;
              end;
              if (Ch<>'~') then
               State:=255;
            end;
        6 : begin {EscO Function keys in vt100 mode PM }
              case ch of
               'P' : {F1}PushExt(kbF1);
               'Q' : {F2}PushExt(kbF2);
               'R' : {F3}PushExt(kbF3);
               'S' : {F4}PushExt(kbF4);
               't' : {F5}PushExt(kbF5);
               'u' : {F6}PushExt(kbF6);
               'v' : {F7}PushExt(kbF7);
               'l' : {F8}PushExt(kbF8);
               'w' : {F9}PushExt(kbF9);
               'x' : {F10}PushExt(kbF10);
               'D' : {keyLeft}PushExt($4B);
               'C' : {keyRight}PushExt($4D);
               'A' : {keyUp}PushExt($48);
               'B' : {keyDown}PushExt($50);
              else
                RestoreArray;
              end;
            end;
        7 : begin {Esc[? keys in vt100 mode PM }
              case ch of
               '0' : State:=11;
               '1' : State:=8;
               '7' : State:=9;
              else
                RestoreArray;
              end;
            end;
        8 : begin {Esc[?1 keys in vt100 mode PM }
              case ch of
               'l' : {local mode};
               'h' : {transmit mode};
               ';' : { 'Esc[1;0c seems to be sent by M$ telnet app
                       for no hangup purposes }
                     state:=10;
              else
                RestoreArray;
              end;
            end;
        9 : begin {Esc[?7 keys in vt100 mode PM }
              case ch of
               'l' : {exit_am_mode};
               'h' : {enter_am_mode};
              else
                RestoreArray;
              end;
            end;
        10 : begin {Esc[?1; keys in vt100 mode PM }
              case ch of
               '0' : state:=11;
              else
                RestoreArray;
              end;
             end;
        11 : begin {Esc[?1;0 keys in vt100 mode PM }
              case ch of
               'c' : ;
              else
                RestoreArray;
              end;
             end;
        13 : begin {Esc[M mouse prefix for xterm }
               GenMouseEvent;
             end;
      255 : { just forget this trailing char };
        end;
        if (State<>0) and (InCnt=0) then
         fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
      end;
     if State=1 then
      PushKey(ch);
{$endif NotUseTree}
     if ch='$' then
       begin { '$<XX>' means a delay of XX millisecs }
         is_delay :=false;
         fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
         if (sysKeyPressed) then
           begin
             ch:=ttyRecvChar;
             is_delay:=(ch='<');
             if not is_delay then
               begin
                 PushKey('$');
                 PushKey(ch);
               end
             else
               begin
{$ifdef logging}
                 write(f,'$<');
{$endif logging}
                 fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
                 while (sysKeyPressed) and (ch<>'>') do
                   begin
                     { Should we really repect this delay ?? }
                     ch:=ttyRecvChar;
{$ifdef logging}
                     write(f,ch);
{$endif logging}
                     fpSelect(StdInputHandle+1,@fdsin,nil,nil,10);
                   end;
               end;
           end
         else
           PushKey('$');
       end;
   end
{$ifdef logging}
       writeln(f);
{$endif logging}
{$ifndef NotUseTree}
    ;
  ReadKey:=PopKey;
{$else  NotUseTree}
  else
   Begin
     case ch of
     #127 : PushKey(#8);
     else
      PushKey(ch);
     end;
   End;
  ReadKey:=PopKey;
{$endif NotUseTree}
End;


function ShiftState:byte;
var
{$ifndef BSD}
  arg,
{$endif BSD}
  shift : longint;
begin
  shift:=0;
  {$Ifndef BSD}
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
 {$endif}
  ShiftState:=shift;
end;


{ Exported functions }

procedure SysInitKeyboard;
begin
  SetRawMode(true);
  patchkeyboard;
{$ifdef logging}
     assign(f,'keyboard.log');
     rewrite(f);
{$endif logging}
  if not IsConsole then
    begin
      { default for Shift prefix is ^ A}
      if ShiftPrefix = 0 then
        ShiftPrefix:=1;
      {default for Alt prefix is ^Z }
      if AltPrefix=0 then
        AltPrefix:=26;
      { default for Ctrl Prefix is ^W }
      if CtrlPrefix=0 then
        CtrlPrefix:=23;
    end;
{$ifndef NotUseTree}
  LoadDefaultSequences;
  LoadTerminfoSequences;
{$endif not NotUseTree}
end;


procedure SysDoneKeyboard;
begin
  unpatchkeyboard;
  SetRawMode(false);

{$ifndef NotUseTree}
  FreeTree;
{$endif not NotUseTree}

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
var
  MyScan,
  SState : byte;
  MyChar : char;
  EscUsed,AltPrefixUsed,CtrlPrefixUsed,ShiftPrefixUsed,IsAlt,Again : boolean;
begin {main}
  MyChar:=Readkey(IsAlt);
  MyScan:=ord(MyChar);
  SState:=ShiftState;
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
        { Handle Ctrl-<x>, but not AltGr-<x> }
        if ((SState and kbCtrl)<>0) and ((SState and kbAlt) = 0)  then
         begin
           case MyScan of
             kbHome..kbDel : { cArrow }
               MyScan:=CtrlArrow[MyScan];
             kbF1..KbF10 : { cF1-cF10 }
               MyScan:=MyScan+kbCtrlF1-kbF1;
             kbF11..KbF12 : { cF11-cF12 }
               MyScan:=MyScan+kbCtrlF11-kbF11;
           end;
         end
        { Handle Alt-<x>, but not AltGr }
        else if ((SState and kbAlt)<>0) and ((SState and kbCtrl) = 0) then
         begin
           case MyScan of
             kbHome..kbDel : { AltArrow }
               MyScan:=AltArrow[MyScan];
             kbF1..KbF10 : { aF1-aF10 }
               MyScan:=MyScan+kbAltF1-kbF1;
             kbF11..KbF12 : { aF11-aF12 }
               MyScan:=MyScan+kbAltF11-kbF11;
             end;
         end
        else if (SState and kbShift)<>0 then
         begin
           case MyScan of
             kbIns: MyScan:=kbShiftIns;
             kbDel: MyScan:=kbShiftDel;
             kbF1..KbF10 : { sF1-sF10 }
               MyScan:=MyScan+kbShiftF1-kbF1;
             kbF11..KbF12 : { sF11-sF12 }
               MyScan:=MyScan+kbShiftF11-kbF11;
             end;
         end;
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
  SysGetShiftState:=ShiftState;
end;


procedure RestoreStartMode;
begin
  TCSetAttr(1,TCSANOW,StartTio);
end;


Const
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
