{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team.

    Keyboard unit for netware

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ 2001/04/16 armin: first version for netware }
unit Keyboard;
interface

{$i keybrdh.inc}

implementation

{$i keyboard.inc}
{$i nwsys.inc}

procedure InitKeyboard;
begin
  PendingKeyEvent := 0;
end;

procedure DoneKeyboard;
begin
end;

function GetKeyEvent: TKeyEvent;
var T : TKeyEvent;
begin
  if PendingKeyEvent<>0 then
  begin
    GetKeyEvent:=PendingKeyEvent;
    PendingKeyEvent:=0;
    exit;
  end;
  T := byte(_getch);
  if T = 0 then
    T := word(_getch) shl 8;
  GetKeyEvent := $03000000 OR T;
end;


function PollKeyEvent: TKeyEvent;
begin
  if PendingKeyEvent<>0 then
   exit(PendingKeyEvent);
  if _kbhit <> 0 then
  begin
    PendingKeyEvent := byte(_getch);
    if PendingKeyEvent = 0 then
      PendingKeyEvent := word(_getch) shl 8;
    PendingKeyEvent := PendingKeyEvent OR $03000000;
    PollKeyEvent := PendingKeyEvent;
  end else
    PollKeyEvent := 0;
end;


function PollShiftStateEvent: TKeyEvent;
begin
  PollShiftStateEvent:=0;
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
  ErrorCode:=errKbdNotImplemented;
end;

end.
