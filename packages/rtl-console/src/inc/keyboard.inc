{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  PendingKeyEvent : TKeyEvent;
  CurrentLegacy2EnhancedKeyEventTranslationCodePage: TSystemCodePage;

procedure PutKeyEvent(KeyEvent: TKeyEvent);
begin
  PendingKeyEvent := KeyEvent;
end;

function GetKeyEventFlags(KeyEvent: TKeyEvent): Byte;
begin
  GetKeyEventFlags := (KeyEvent and $FF000000) shr 24;
end;

function GetKeyEventChar(KeyEvent: TKeyEvent): AnsiChar;
begin
  if KeyEvent and $03000000 = $00000000 then
    GetKeyEventChar := Chr(KeyEvent and $000000FF)
   else
    GetKeyEventChar := #0;
end;

function GetKeyEventUniCode(KeyEvent: TKeyEvent): Word;
begin
  if KeyEvent and $03000000 = $01000000 then
    GetKeyEventUniCode := KeyEvent and $0000FFFF
   else
    GetKeyEventUniCode := 0;
end;

function GetKeyEventCode(KeyEvent: TKeyEvent): Word;
begin
  GetKeyEventCode := KeyEvent and $0000FFFF
end;

function GetKeyEventShiftState(KeyEvent: TKeyEvent): Byte;
begin
  GetKeyEventShiftState := (KeyEvent and $00FF0000) shr 16;
end;

function IsFunctionKey(KeyEvent: TKeyEvent): Boolean;
begin
  IsFunctionKey := KeyEvent and $03000000 = $02000000;
end;

Var
  KeyBoardInitialized : Boolean;
  CurrentKeyboardDriver : TKeyboardDriver;

procedure InitKeyboard;

begin
  If Not KeyboardInitialized then
    begin
    CurrentLegacy2EnhancedKeyEventTranslationCodePage := 437;
    If Assigned(CurrentKeyBoardDriver.InitDriver) Then
      CurrentKeyBoardDriver.InitDriver();
    KeyboardInitialized:=True;
    end;
end;

procedure DoneKeyboard;

begin
  If KeyboardInitialized then
    begin
    If Assigned(CurrentKeyBoardDriver.DoneDriver) Then
      CurrentKeyBoardDriver.DoneDriver();
    KeyboardInitialized:=False;
    end;
end;


{ Converts an Extended 101/102-Keyboard function scan code (as returned by int
  16h, AH=10h/11h) to a standard 101/102-Keyboard function scan code (as would
  be returned by int 16h, AH=00h/01h). However, keys that are not returned by
  the standard keyboard function (e.g. F11, F12, etc.) are still let through.
  Only those that return different codes are converted. }
function ConvertExtendedToStandardScanCode(ScanCode: Word): Word;
begin
  if (ScanCode and $FF)=$E0 then
    ConvertExtendedToStandardScanCode:=ScanCode and $FF00
  else
    case ScanCode of
      { Numpad Enter -> Regular Enter }
      $E00D:
        ConvertExtendedToStandardScanCode:=$1C0D;
      { Ctrl + Numpad Enter -> Ctrl + Regular Enter }
      $E00A:
        ConvertExtendedToStandardScanCode:=$1C0A;
      { Numpad '/' -> Regular '/' }
      $E02F:
        ConvertExtendedToStandardScanCode:=$352F;
      else
        ConvertExtendedToStandardScanCode:=ScanCode;
    end;
end;


function ConvertEnhancedToLegacyShiftState(const ShiftState: TEnhancedShiftState): Byte;
begin
  ConvertEnhancedToLegacyShiftState:=0;
  if essAlt in ShiftState then
    ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or kbAlt;
  if essCtrl in ShiftState then
    ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or kbCtrl;
  if essShift in ShiftState then
    begin
      if ([essLeftShift,essRightShift]*ShiftState)=[] then
        ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or kbShift
      else
        begin
          if essLeftShift in ShiftState then
            ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or kbLeftShift;
          if essRightShift in ShiftState then
            ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or kbRightShift;
        end;
    end;
  { AltGr triggers both kbAlt and kbCtrl in the legacy shift state }
  if essAltGr in ShiftState then
    ConvertEnhancedToLegacyShiftState:=ConvertEnhancedToLegacyShiftState or (kbAlt or kbCtrl);
end;


function ConvertToLegacyKeyEvent(const KeyEvent: TEnhancedKeyEvent): TKeyEvent;
begin
  if KeyEvent=NilEnhancedKeyEvent then
    ConvertToLegacyKeyEvent:=0
  else
    ConvertToLegacyKeyEvent:=(kbPhys shl 24) or
      ConvertExtendedToStandardScanCode(KeyEvent.VirtualScanCode) or
      (ConvertEnhancedToLegacyShiftState(KeyEvent.ShiftState) shl 16);
end;


function GetKeyEvent: TKeyEvent;

begin
  if PendingKeyEvent<>0 then
    begin
    GetKeyEvent:=PendingKeyEvent;
    PendingKeyEvent:=0;
    exit;
    end;
  If Assigned(CurrentKeyBoardDriver.GetKeyEvent) Then
    GetKeyEvent:=CurrentKeyBoardDriver.GetKeyEvent()
  else if Assigned(CurrentKeyboardDriver.GetEnhancedKeyEvent) then
    GetKeyEvent:=ConvertToLegacyKeyEvent(CurrentKeyboardDriver.GetEnhancedKeyEvent())
  else
    GetKeyEvent:=0;
end;

function PollKeyEvent: TKeyEvent;

begin
  if PendingKeyEvent<>0 then
   exit(PendingKeyEvent);
  If Assigned(CurrentKeyBoardDriver.PollKeyEvent) Then
    begin
    PollKeyEvent:=CurrentKeyBoardDriver.PollKeyEvent();
    // PollKeyEvent:=PendingKeyEvent;
    // Must be done inside every keyboard specific
    // PollKeyEvent procedure
    // to avoid problems if that procedure is called directly PM
    end
  else if Assigned(CurrentKeyboardDriver.PollEnhancedKeyEvent) then
    PollKeyEvent:=ConvertToLegacyKeyEvent(CurrentKeyboardDriver.PollEnhancedKeyEvent())
  else
    PollKeyEvent:=0;
end;

Function SetKeyboardDriver (Const Driver : TKeyboardDriver) : Boolean;

begin
  If Not KeyBoardInitialized then
    CurrentKeyBoardDriver:=Driver;
  SetKeyboardDriver:=Not KeyBoardInitialized;
end;

Procedure GetKeyboardDriver (Var Driver : TKeyboardDriver);

begin
  Driver:=CurrentKeyBoardDriver;
end;

function PollShiftStateEvent: TKeyEvent;

begin
  If Assigned(CurrentKeyBoardDriver.GetShiftState) then
    PollShiftStateEvent:=CurrentKeyBoardDriver.GetShiftState() shl 16
  else
    PollShiftStateEvent:=0;
end;

function DefaultTranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;

begin
  DefaultTranslateKeyEventUniCode:=KeyEvent;
  ErrorCode:=errKbdNotImplemented;
end;


function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;

begin
  if Assigned(CurrentKeyBoardDriver.TranslateKeyEventUnicode) then
    TranslateKeyEventUnicode:=CurrentKeyBoardDriver.TranslateKeyEventUnicode(KeyEvent)
  else
    TranslateKeyEventUnicode:=DefaultTranslateKeyEventUnicode(KeyEvent);
end;

function ConvertToEnhancedKeyEvent(KeyEvent: TKeyEvent): TEnhancedKeyEvent;
var
  TranslatedKeyEvent: TKeyEvent;
  ShiftState: Byte;
  tmpS: RawByteString;
  tmpUS: UnicodeString;
begin
  ConvertToEnhancedKeyEvent:=NilEnhancedKeyEvent;
  if KeyEvent=0 then
    exit;
  ConvertToEnhancedKeyEvent.ShiftState:=[];
  ShiftState:=GetKeyEventShiftState(KeyEvent);
  if (kbAlt and ShiftState)<>0 then
    Include(ConvertToEnhancedKeyEvent.ShiftState,essAlt);
  if (kbCtrl and ShiftState)<>0 then
    Include(ConvertToEnhancedKeyEvent.ShiftState,essCtrl);
  if (kbShift and ShiftState)<>0 then
    begin
      Include(ConvertToEnhancedKeyEvent.ShiftState,essShift);
      if (kbLeftShift and ShiftState)<>0 then
        Include(ConvertToEnhancedKeyEvent.ShiftState,essLeftShift);
      if (kbRightShift and ShiftState)<>0 then
        Include(ConvertToEnhancedKeyEvent.ShiftState,essRightShift);
    end;
  case GetKeyEventFlags(KeyEvent) of
    kbASCII:
      ConvertToEnhancedKeyEvent.AsciiChar:=GetKeyEventChar(KeyEvent);
    kbUniCode:
      ConvertToEnhancedKeyEvent.UnicodeChar:=WideChar(GetKeyEventUniCode(KeyEvent));
    kbFnKey:
      ConvertToEnhancedKeyEvent.VirtualKeyCode:=GetKeyEventCode(KeyEvent);
    kbPhys:
      ConvertToEnhancedKeyEvent.VirtualScanCode:=KeyEvent and $ffff;
  end;
  TranslatedKeyEvent:=TranslateKeyEvent(KeyEvent);
  case GetKeyEventFlags(TranslatedKeyEvent) of
    kbASCII:
      ConvertToEnhancedKeyEvent.AsciiChar:=GetKeyEventChar(TranslatedKeyEvent);
    kbUniCode:
      ConvertToEnhancedKeyEvent.UnicodeChar:=WideChar(GetKeyEventUniCode(TranslatedKeyEvent));
    kbFnKey:
      ConvertToEnhancedKeyEvent.VirtualKeyCode:=GetKeyEventCode(TranslatedKeyEvent);
    kbPhys:
      ConvertToEnhancedKeyEvent.VirtualScanCode:=TranslatedKeyEvent and $ffff;
  end;
  { todo: set ConvertToEnhancedKeyEvent.Flags }
  if (ConvertToEnhancedKeyEvent.UnicodeChar=WideChar(0)) then
    begin
      if (ConvertToEnhancedKeyEvent.AsciiChar>=#0) and
         (ConvertToEnhancedKeyEvent.AsciiChar<=#127) then
        ConvertToEnhancedKeyEvent.UnicodeChar:=WideChar(ConvertToEnhancedKeyEvent.AsciiChar)
      else
        begin
          SetLength(tmpS, 1);
          tmpS[1]:=ConvertToEnhancedKeyEvent.AsciiChar;
          System.SetCodePage(tmpS,CurrentLegacy2EnhancedKeyEventTranslationCodePage,False);
          tmpUS:=tmpS;
          if Length(tmpUS) = 1 then
            ConvertToEnhancedKeyEvent.UnicodeChar := tmpUS[1];
        end;
    end;
end;

function DefaultGetEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  DefaultGetEnhancedKeyEvent:=ConvertToEnhancedKeyEvent(GetKeyEvent);
end;

function GetEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  if Assigned(CurrentKeyBoardDriver.GetEnhancedKeyEvent) then
    GetEnhancedKeyEvent:=CurrentKeyBoardDriver.GetEnhancedKeyEvent()
  else
    GetEnhancedKeyEvent:=DefaultGetEnhancedKeyEvent;
end;

function DefaultPollEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  DefaultPollEnhancedKeyEvent:=ConvertToEnhancedKeyEvent(PollKeyEvent);
end;

function PollEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  if Assigned(CurrentKeyBoardDriver.PollEnhancedKeyEvent) then
    PollEnhancedKeyEvent:=CurrentKeyBoardDriver.PollEnhancedKeyEvent()
  else
    PollEnhancedKeyEvent:=DefaultPollEnhancedKeyEvent;
end;

operator = (const a, b: TEnhancedKeyEvent) res: Boolean;
begin
  res:=(a.VirtualKeyCode = b.VirtualKeyCode) and
       (a.VirtualScanCode = b.VirtualScanCode) and
       (a.UnicodeChar = b.UnicodeChar) and
       (a.AsciiChar = b.AsciiChar) and
       (a.ShiftState = b.ShiftState) and
       (a.Flags = b.Flags);
end;

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
     (Min: $47; Max: $49; Offset: kbdHome), { Keypad keys kbdHome-kbdPgUp }
     (Min: $4B; Max: $4D; Offset: kbdLeft), { Keypad keys kbdLeft-kbdRight }
     (Min: $4F; Max: $51; Offset: kbdEnd),  { Keypad keys kbdEnd-kbdPgDn }
     (Min: $52; Max: $53; Offset: kbdInsert));


function DefaultTranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;

var
  I: Integer;
  ScanCode: Byte;
begin
  if KeyEvent and $03000000 = $03000000 then
   begin
     if KeyEvent and $000000FF <> 0 then
      begin
        DefaultTranslateKeyEvent := KeyEvent and $00FFFFFF;
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
              DefaultTranslateKeyEvent := $02000000 + (KeyEvent and $00FF0000) +
                Byte(ScanCode - TranslationTable[I].Min) + TranslationTable[I].Offset;
              exit;
            end;
         end;
      end;
   end;
  DefaultTranslateKeyEvent := KeyEvent;
end;

function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;

begin
  if Assigned(CurrentKeyBoardDriver.TranslateKeyEvent) then
    TranslateKeyEvent:=CurrentKeyBoardDriver.TranslateKeyEvent(KeyEvent)
  else
    TranslateKeyEvent:=DefaultTranslateKeyEvent(KeyEvent);
end;

{ ---------------------------------------------------------------------
    KeyEvent to String representation section.
  ---------------------------------------------------------------------}

Procedure AddToString (Var S : shortstring; Const A : shortstring);

begin
  If Length(S)=0 then
    S:=A
  else
    S:=S+' '+A;
end;

Function IntToStr(Int : Longint) : shortstring;

begin
  Str(Int,IntToStr);
end;

Function ShiftStateToString(KeyEvent : TKeyEvent; UseLeftRight : Boolean) : shortstring;

Var
  S : Integer;
  T : shortstring;

begin
  S:=GetKeyEventShiftState(KeyEvent);
  T:='';
  If (S and kbShift)<>0 then
    begin
    if UseLeftRight then
      case (S and kbShift) of
        kbShift      : AddToString(T,SLeftRight[1]+' '+SAnd+' '+SLeftRight[2]);
        kbLeftShift  : AddToString(T,SLeftRight[1]);
        kbRightShift : AddToString(T,SLeftRight[2]);
      end;
    AddToString(T,SShift[1]);
    end;
  If (S and kbCtrl)<>0 Then
    AddToString(T,SShift[2]);
  If (S and kbAlt)<>0 Then
    AddToString(T,SShift[3]);
  ShiftStateToString:=T;
end;

Function FunctionKeyName (KeyCode : Word) : shortstring;

begin
  If ((KeyCode-KbdF1)<$1F) Then
    FunctionKeyName:='F'+IntToStr((KeyCode-KbdF1+1))
  else
    begin
    If (KeyCode-kbdHome)<($2F-$1F) then
      FunctionKeyName:=SKeyPad[(KeyCode-kbdHome)]
    else
      FunctionKeyName:=SUnknownFunctionKey + IntToStr(KeyCode);
    end;
end;

Function KeyEventToString(KeyEvent : TKeyEvent) : shortstring;

Var
  T : shortstring;

begin
  T:=ShiftStateToString(KeyEvent,False);
  Case GetKeyEventFlags(KeyEvent) of
    kbASCII   : AddToString(T,GetKeyEventChar(KeyEvent));
    kbUniCode : AddToString(T,SUniCodeChar+IntToStr(GetKeyEventUniCode(Keyevent)));
    kbFnKey   : AddToString(T,FunctionKeyName(GetKeyEventCode(KeyEvent)));
                // Not good, we need a GetKeyEventScanCode function !!
    kbPhys    : AddToString(T,SScanCode+IntToStr(KeyEvent and $ffff));
  end;
  KeyEventToString:=T;
end;


const
  PrevCtrlBreakHandler: TCtrlBreakHandler = nil;

function KbdCtrlBreakHandler (CtrlBreak: boolean): boolean;
begin
(* Earlier registered handlers (user specific) have priority. *)
  if Assigned (PrevCtrlBreakHandler) then
    if PrevCtrlBreakHandler (CtrlBreak) then
      begin
        KbdCtrlBreakHandler := true;
        Exit;
      end;
(* If Ctrl-Break was pressed, either ignore it or allow default processing. *)
  if CtrlBreak then
    KbdCtrlBreakHandler := false
  else (* Ctrl-C pressed or not possible to distinguish *)
    begin
      PutKeyEvent ((kbCtrl shl 16) or 3);
      KbdCtrlBreakHandler := true;
    end;
end;


procedure SetKbdCtrlBreakHandler;
begin
  PrevCtrlBreakHandler := SysSetCtrlBreakHandler (@KbdCtrlBreakHandler);
  if PrevCtrlBreakHandler = TCtrlBreakHandler (codepointer (-1)) then
    PrevCtrlBreakHandler := nil;
end;


