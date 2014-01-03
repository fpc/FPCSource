{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Dummy Mouse unit for netware

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{2001/04/14 armin: first version, only a dummy, i think there is no 'official' way to support
                   a mouse under netware }
unit Mouse;
interface

{$ifdef NOMOUSE}
{$DEFINE NOGPM}
{$ENDIF}

{const
  MouseEventBufSize = 16; }

{$i mouseh.inc}

implementation


procedure PlaceMouseCur(ofs:longint);
begin
end;


procedure InitMouse;
begin
end;


procedure DoneMouse;
begin
end;


function DetectMouse:byte;
begin
  DetectMouse:=0;
end;


procedure ShowMouse;
begin
end;


procedure HideMouse;
begin
end;


function GetMouseX:word;
begin
  GetMouseX:=0;
end;


function GetMouseY:word;
begin
  GetMouseY:=0;
end;


function GetMouseButtons:word;
begin
  GetMouseButtons:=0;
end;


procedure SetMouseXY(x,y:word);
begin
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
end;


procedure PutMouseEvent(const MouseEvent: TMouseEvent);
begin
end;


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  exit(false);
end;

Procedure SetMouseDriver(Const Driver : TMouseDriver);
{ Sets the mouse driver. }
begin
end;

Procedure GetMouseDriver(Var Driver : TMouseDriver);
{ Returns the currently active mouse driver }
begin
end;

end.
