{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  { We have an errorcode base of 1030 }
  errMouseBase                    = 1030;
  errMouseInitError               = errMouseBase + 0;
  errMouseNotImplemented          = errMouseBase + 1;

type
  PMouseEvent=^TMouseEvent;
  TMouseEvent=packed record { 8 bytes }
    buttons : word;
    x,y     : word;
    Action  : word;
  end;

const
  MouseActionDown = $0001;                         { Mouse down event }
  MouseActionUp   = $0002;                         { Mouse up event }
  MouseActionMove = $0004;                         { Mouse move event }

  MouseLeftButton   = $01;                         { Left mouse button }
  MouseRightButton  = $02;                         { Right mouse button }
  MouseMiddleButton = $04;                         { Middle mouse button }

  MouseEventBufSize = 16;                { Size of event queue }

var
  MouseIntFlag : Byte;                                { Mouse in int flag }
  MouseButtons : Byte;                                { Mouse button state }
  MouseWhereX,
  MouseWhereY  : Word;                                { Mouse position }

Type
  TMouseDriver = Record
    UseDefaultQueue : Boolean;
    InitDriver : Procedure;
    DoneDriver : Procedure;
    DetectMouse : Function : Byte;
    ShowMouse : Procedure;
    HideMouse : Procedure;
    GetMouseX : Function : Word;
    GetMouseY : Function : Word;
    GetMouseButtons : Function : Word;
    SetMouseXY : procedure (x,y:word);
    GetMouseEvent : procedure (var MouseEvent:TMouseEvent);
    PollMouseEvent : function (var MouseEvent: TMouseEvent):boolean;
    PutMouseEvent : procedure (Const MouseEvent:TMouseEvent);
  end;

procedure InitMouse;
{ Initialize the mouse interface }

procedure DoneMouse;
{ Deinitialize the mouse interface }

function DetectMouse:byte;
{ Detect if a mouse is present, returns the amount of buttons or 0
  if no mouse is found }

procedure ShowMouse;
{ Show the mouse cursor }

procedure HideMouse;
{ Hide the mouse cursor }

function GetMouseX:word;
{ Return the current X position of the mouse }

function GetMouseY:word;
{ Return the current Y position of the mouse }

function GetMouseButtons:word;
{ Return the current button state of the mouse }

procedure SetMouseXY(x,y:word);
{ Place the mouse cursor on x,y }

procedure GetMouseEvent(var MouseEvent:TMouseEvent);
{ Returns the last Mouseevent, and waits for one if not available }

procedure PutMouseEvent(const MouseEvent: TMouseEvent);
{ Adds the given MouseEvent to the input queue. Please note that depending on
  the implementation this can hold only one value (NO FIFOs etc) }

function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;
{ Checks if a Mouseevent is available, and returns it if one is found. If no
  event is pending, it returns 0 }

Procedure SetMouseDriver(Const Driver : TMouseDriver);
{ Sets the mouse driver. }

Procedure GetMouseDriver(Var Driver : TMouseDriver);
{ Returns the currently active mouse driver }

