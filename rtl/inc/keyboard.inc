{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure PutKeyEvent(KeyEvent: TKeyEvent);
begin
  PendingKeyEvent := KeyEvent;
end;

function GetKeyEventFlags(KeyEvent: TKeyEvent): Byte;
begin
  GetKeyEventFlags := (KeyEvent and $FF000000) shr 24;
end;

function GetKeyEventChar(KeyEvent: TKeyEvent): Char;
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

{
  $Log$
  Revision 1.1  2001-01-13 11:13:12  peter
    * API 2 RTL

}

