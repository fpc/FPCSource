{
  System independent keyboard interface for linux

  $Id$
}

uses
  Linux;


var
  OldIO : TermIos;
Procedure SetRawMode(b:boolean);
Var
  Tio : Termios;
Begin
  TCGetAttr(1,Tio);
  if b then
   begin
     OldIO:=Tio;
     Tio.c_iflag:=Tio.c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON));
     Tio.c_lflag:=Tio.c_lflag and (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
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

procedure PatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  i : longint;
begin
  for i:=1to kbdchanges do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     Ioctl(stdinputhandle,KDGKBENT,@entry);
     e^.oldval:=entry.kb_value;
     entry.kb_table:=e^.oldtab;
     entry.kb_index:=e^.oldidx;
     ioctl(stdinputhandle,KDGKBENT,@entry);
     e^.newval:=entry.kb_value;
   end;
  for i:=1to kbdchanges do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.newval;
     Ioctl(stdinputhandle,KDSKBENT,@entry);
   end;
end;


procedure UnpatchKeyboard;
var
  e : ^chgentry;
  entry : kbentry;
  i : longint;
begin
  for i:=1to kbdchanges do
   begin
     e:=@kbdchange[i];
     entry.kb_table:=e^.tab;
     entry.kb_index:=e^.idx;
     entry.kb_value:=e^.oldval;
     Ioctl(stdinputhandle,KDSKBENT,@entry);
   end;
end;



{ Buffered Input routines }
const
  InSize=256;
var
  InBuf  : array[0..InSize-1] of char;
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
     Readed:=fdRead(StdInputHandle,InBuf[InHead],i);
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
  fdsin : fdSet;
begin
  if (InCnt>0) then
   sysKeyPressed:=true
  else
   begin
     FD_Zero(fdsin);
     fd_Set(StdInputHandle,fdsin);
     sysKeypressed:=(Select(StdInputHandle+1,@fdsin,nil,nil,0)>0);
   end;
end;

Function KeyPressed:Boolean;
Begin
  Keypressed := (KeySend<>KeyPut) or sysKeyPressed;
End;

Function ReadKey:char;
Var
  ch       : char;
  OldState,
  State    : longint;
  fdsin    : fdSet;
Begin
{Check Buffer first}
  if KeySend<>KeyPut then
   begin
     ReadKey:=PopKey;
     exit;
   end;
{Wait for Key}
  if not sysKeyPressed then
   begin
     FD_Zero (fdsin);
     FD_Set (StdInputHandle,fdsin);
     Select (StdInputHandle+1,@fdsin,nil,nil,nil);
   end;
  ch:=ttyRecvChar;
{Esc Found ?}
  If (ch=#27) then
   begin
     FD_Zero(fdsin);
     fd_Set(StdInputHandle,fdsin);
     State:=1;
     if InCnt=0 then
      Select(StdInputHandle+1,@fdsin,nil,nil,10);
     while (State<>0) and (sysKeyPressed) do
      begin
        ch:=ttyRecvChar;
        OldState:=State;
        State:=0;
        case OldState of
        1 : begin {Esc}
              case ch of
          'a'..'z',
          '0'..'9',
           '-','=' : PushExt(FAltKey(ch));
               #10 : PushKey(#10);
               #13 : PushKey(#10);
              #127 : PushKey(#8);
               '[' : State:=2;
               else
                begin
                  PushKey(ch);
                  PushKey(#27);
                end;
               end;
            end;
        2 : begin {Esc[}
              case ch of
               '[' : State:=3;
               'A' : PushExt(72);
               'B' : PushExt(80);
               'C' : PushExt(77);
               'D' : PushExt(75);
               'G' : PushKey('5');
               'H' : PushExt(71);
               'K' : PushExt(79);
               '1' : State:=4;
               '2' : State:=5;
               '3' : PushExt(83);
               '4' : PushExt(79);
               '5' : PushExt(73);
               '6' : PushExt(81);
              else
               begin
                 PushKey(ch);
                 PushKey('[');
                 PushKey(#27);
               end;
              end;
              if ch in ['3'..'6'] then
               State:=255;
            end;
        3 : begin {Esc[[}
              case ch of
               'A' : PushExt(59);
               'B' : PushExt(60);
               'C' : PushExt(61);
               'D' : PushExt(62);
               'E' : PushExt(63);
              end;
            end;
        4 : begin
              case ch of
               '~' : PushExt(71);
               '7' : PushExt(64);
               '8' : PushExt(65);
               '9' : PushExt(66);
              end;
              if (Ch<>'~') then
               State:=255;
            end;
        5 : begin
              case ch of
               '~' : PushExt(82);
               '0' : pushExt(67);
               '1' : PushExt(68);
               '3' : PushExt(133);
               '4' : PushExt(134);
              end;
              if (Ch<>'~') then
               State:=255;
            end;
      255 : ;
        end;
        if (State<>0) and (InCnt=0) then
         Select(StdInputHandle+1,@fdsin,nil,nil,10);
      end;
     if State=1 then
      PushKey(ch);
   end
  else
   Begin
     case ch of
     #127 : PushKey(#8);
     else
      PushKey(ch);
     end;
   End;
  ReadKey:=PopKey;
End;


function ShiftState:byte;
var
  arg,shift : longint;
begin
  arg:=6;
  shift:=0;
  if IOCtl(StdInputHandle,TIOCLINUX,@arg) then
   begin
     if (arg and (2 or 8))<>0 then
      inc(shift,8);
     if (arg and 4)<>0 then
      inc(shift,4);
     if (arg and 1)<>0 then
      inc(shift,3);
   end;
  ShiftState:=shift;
end;


{ Exported functions }

procedure InitKeyboard;
begin
  SetRawMode(true);
  patchkeyboard;
end;


procedure DoneKeyboard;
begin
  unpatchkeyboard;
  SetRawMode(false);
end;


function GetKeyEvent: TKeyEvent;

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
  CtrlArrow : array [71..81] of byte =
   ($77,$8d,$84,$8e,$73,$8f,$74,$90,$75,$91,$76);
var
  MyScan,
  SState : byte;
  MyChar : char;
begin {main}
  if PendingKeyEvent<>0 then
   begin
     GetKeyEvent:=PendingKeyEvent;
     PendingKeyEvent:=0;
     exit;
   end;

  MyChar:=Readkey;
  MyScan:=ord(MyChar);
  SState:=ShiftState;

  case MyChar of
   #26 : begin { ^Z - replace Alt for Linux OS }
           MyChar:=ReadKey;
           MyScan:=ord(MyChar);
           if MyScan=0 then
            MyScan:=EvalScanZ(ord(ReadKey))
           else
            begin
              MyScan:=EvalScan(ord(MyChar));
              if MyScan in [$02..$0D] then
               inc(MyScan,$76);
              MyChar:=chr(0);
            end;
         end;
    #0 : begin
           MyScan:=ord(ReadKey);
           { Handle Ctrl-<x> }
           if (SState and 4)<>0 then
            begin
              case MyScan of
                71..81 : { cArrow }
                  MyScan:=CtrlArrow[MyScan];
                $3b..$44 : { cF1-cF10 }
                  MyScan:=MyScan+$23;
              end;
            end;
           { Handle Alt-<x> }
           if (SState and 8)<>0 then
            begin
              case MyScan of
                $3b..$44 : { aF1-aF10 }
                  MyScan:=MyScan+$2d;
              end;
            end;
         end;
    else begin
           MyScan:=EvalScan(ord(MyChar));
         end;
  end;
  GetKeyEvent:=$3000000 or ord(MyChar) or (MyScan shl 8) or (SState shl 16);
end;


function PollKeyEvent: TKeyEvent;
begin
  if PendingKeyEvent<>0 then
   exit(PendingKeyEvent);
  if keypressed then
   begin
     { just get the key and place it in the pendingkeyevent }
     PendingKeyEvent:=GetKeyEvent;
     PollKeyEvent:=PendingKeyEvent;
   end
  else
   PollKeyEvent:=0;
end;


function PollShiftStateEvent: TKeyEvent;
begin
  PollShiftStateEvent:=ShiftState shl 16;
end;


{ Function key translation }
type
  TTranslationEntry = packed record
    Min, Max: Byte;
    Offset: Word;
  end;
const
  TranslationTableEntries = 12;
  TranslationTable: array [1..TranslationTableEntries] of TTranslationEntry =
    ((Min: $3B; Max: $44; Offset: kbdF1),   { function keys F1-F10 }
     (Min: $54; Max: $5D; Offset: kbdF1),   { Shift fn keys F1-F10 }
     (Min: $5E; Max: $67; Offset: kbdF1),   { Ctrl fn keys F1-F10 }
     (Min: $68; Max: $71; Offset: kbdF1),   { Alt fn keys F1-F10 }
     (Min: $85; Max: $86; Offset: kbdF11),  { function keys F11-F12 }
     (Min: $87; Max: $88; Offset: kbdF11),  { Shift+function keys F11-F12 }
     (Min: $89; Max: $8A; Offset: kbdF11),  { Ctrl+function keys F11-F12 }
     (Min: $8B; Max: $8C; Offset: kbdF11),  { Alt+function keys F11-F12 }
     (Min:  71; Max:  73; Offset: kbdHome), { Keypad keys kbdHome-kbdPgUp }
     (Min:  75; Max:  77; Offset: kbdLeft), { Keypad keys kbdLeft-kbdRight }
     (Min:  79; Max:  81; Offset: kbdEnd),  { Keypad keys kbdEnd-kbdPgDn }
     (Min: $52; Max: $53; Offset: kbdInsert));

function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
var
  I: Integer;
  ScanCode: Byte;
begin
  if KeyEvent and $03000000 = $03000000 then
   begin
     if KeyEvent and $000000FF <> 0 then
      begin
        TranslateKeyEvent := KeyEvent and $00FFFFFF;
        exit;
      end
     else
      begin
        { This is a function key }
        ScanCode := (KeyEvent and $0000FF00) shr 8;
        for I := 1 to TranslationTableEntries do
         begin
           if (TranslationTable[I].Min <= ScanCode) and (ScanCode <= TranslationTable[I].Max) then
            begin
              TranslateKeyEvent := $02000000 + (KeyEvent and $00FF0000) +
                (ScanCode - TranslationTable[I].Min) + TranslationTable[I].Offset;
              exit;
            end;
         end;
      end;
   end;
  TranslateKeyEvent := KeyEvent;
end;


function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;
begin
  TranslateKeyEventUniCode := KeyEvent;
  ErrorHandler(errKbdNotImplemented, nil);
end;

{
  $Log$
  Revision 1.3  2000-10-04 11:53:31  pierre
   Add TargetEntry and TargetExit (merged)

  Revision 1.2  2000/07/13 11:32:25  michael
  + removed logs

}