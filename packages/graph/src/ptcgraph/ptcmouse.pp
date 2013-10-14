{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Nikolay Nikolov (nickysn@users.sourceforge.net)
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    This file implements mouse input support for ptcgraph.
    It is similar to the winmouse and msmouse units.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ptcmouse;

{$MODE objfpc}

interface

{ initializes the mouse with the default values for the current screen mode }
function InitMouse: Boolean;

{ shows mouse pointer,text+graphics screen support }
procedure ShowMouse;

{ hides mouse pointer }
procedure HideMouse;

{ reads mouse position in pixels (divide by 8 to get text position in standard
  text mode) and reads the buttons state:
     bit 1 set -> left button pressed
     bit 2 set -> right button pressed
     bit 3 set -> middle button pressed
  Have a look at the example program in the manual to see how you can use this }
procedure GetMouseState(var x, y, buttons: LongInt);

{ returns true if the left button is pressed }
function LPressed: Boolean;

{ returns true if the right button is pressed }
function RPressed: Boolean;

{ returns true if the middle button is pressed }
function MPressed: Boolean;

(*!!!!! the following functions aren't implemented yet:
{ positions the mouse pointer }
procedure SetMousePos(x,y: LongInt);

{ returns at which position "button" was last pressed in x,y and returns the
  number of times this button has been pressed since the last time this
  function was called with "button" as parameter. For button you can use the
  LButton, RButton and MButton constants for resp. the left, right and middle
  button }
function GetLastButtonPress(button: LongInt; var x, y: LongInt): LongInt;

{ returns at which position "button" was last released in x,y and returns the
  number of times this button has been re since the last time. For button
  you can use the LButton, RButton and MButton constants for resp. the left,
  right and middle button
}
function GetLastButtonRelease(button: LongInt; var x, y: LongInt): LongInt;

{ sets mouse's x range, with Min and Max resp. the higest and the lowest
  column (in pixels) in between which the mouse cursor can move }
procedure SetMouseXRange(Min, Max: LongInt);

{ sets mouse's y range, with Min and Max resp. the higest and the lowest
  row (in pixels) in between which the mouse cursor can move}
procedure SetMouseYRange(Min, Max: LongInt);

{ set the window coordinates in which the mouse cursor can move }
procedure SetMouseWindow(x1, y1, x2, y2: LongInt);

{ sets the mouse shape in text mode: background and foreground color and the
  Ascii value with which the character on screen is XOR'ed when the cursor
  moves over it. Set to 0 for a "transparent" cursor}
procedure SetMouseShape(ForeColor, BackColor, Ascii: Byte);

{ sets the mouse ascii in text mode. The difference between this one and
  SetMouseShape, is that the foreground and background colors stay the same
  and that the Ascii code you enter is the character that you will get on
  screen; there's no XOR'ing }
procedure SetMouseAscii(Ascii: Byte);

{ set mouse speed in mickey's/pixel; default: horizontal: 8; vertical: 16 }
procedure SetMouseSpeed(Horizontal, Vertical: LongInt);

{ set a rectangle on screen that mouse will disappear if it is moved into }
procedure SetMouseHideWindow(x1, y1, x2, y2: LongInt);
*)

const
  LButton = 1; { left button   }
  RButton = 2; { right button  }
  MButton = 4; { middle button }

var
  MouseFound: Boolean;

implementation

uses
   ptcgraph, ptc, ptcwrapper;

function InGraphMode: Boolean;
begin
  Result := (PTCWrapperObject <> nil) and (PTCWrapperObject.IsOpen);
end;

var
  MouseX, MouseY: LongInt;
  MouseButtonState: Byte;

procedure GetMouseEvents;
var
  ev: IPTCEvent;
  MouseEv: IPTCMouseEvent;
begin
  if not InGraphMode then
  begin
    MouseX := 0;
    MouseY := 0;
    MouseButtonState := 0;
    exit;
  end;
  repeat
    PTCWrapperObject.NextEvent(ev, False, [PTCMouseEvent]);
    if ev <> nil then
    begin
      case ev.EventType of
        PTCMouseEvent:
          begin
            MouseEv := ev as IPTCMouseEvent;
            MouseX := MouseEv.X;
            MouseY := MouseEv.Y;
            MouseButtonState := 0;
            if PTCMouseButton1 in MouseEv.ButtonState then
              MouseButtonState := MouseButtonState or LButton;
            if PTCMouseButton2 in MouseEv.ButtonState then
              MouseButtonState := MouseButtonState or RButton;
            if PTCMouseButton3 in MouseEv.ButtonState then
              MouseButtonState := MouseButtonState or MButton;
          end;
      end;
    end;
  until ev = nil;
end;

function InitMouse: Boolean;
begin
  GetMouseEvents;
  InitMouse := MouseFound;
end;

procedure ShowMouse;
begin
  GetMouseEvents;
  if InGraphMode then
    PTCWrapperObject.Option('show cursor');
end;

procedure HideMouse;
begin
  GetMouseEvents;
  if InGraphMode then
    PTCWrapperObject.Option('hide cursor');
end;

function LPressed: Boolean;
begin
  GetMouseEvents;
  LPressed := (MouseButtonState and LButton) <> 0;
end;

function RPressed: Boolean;
begin
  GetMouseEvents;
  RPressed := (MouseButtonState and RButton) <> 0;
end;

function MPressed: Boolean;
begin
  GetMouseEvents;
  MPressed := (MouseButtonState and MButton) <> 0;
end;

procedure GetMouseState(var x, y, buttons: LongInt);
begin
  GetMouseEvents;
  x := MouseX;
  y := MouseY;
  buttons := MouseButtonState;
end;

begin
  MouseFound := True;
end.
