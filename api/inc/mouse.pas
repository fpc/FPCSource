{
   $Id$

   Mouse unit, part of the portable API for Pascal

   Copyright (c) 1997 Balazs Scheidler (bazsi@balabit.hu)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
unit Mouse;
interface
{$ifdef TP}
  {$G+}
{$endif}

{$i platform.inc}

uses
  ApiComm;

const
  { We have an errorcode base of 1010 }
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

{$ifdef OS_WINDOWS}
  MouseEventBufSize = 255;
{$else OS_WINDOWS}
  MouseEventBufSize = 16;
{$endif OS_WINDOWS}

var
  PendingMouseEvent  : array[0..MouseEventBufSize-1] of TMouseEvent;
  PendingMouseHead,
  PendingMouseTail   : PMouseEvent;
  PendingMouseEvents : byte;

  LastMouseEvent : TMouseEvent;

  MouseIntFlag : Byte;                                { Mouse in int flag }
  MouseButtons : Byte;                                { Mouse button state }
  MouseWhereX,
  MouseWhereY  : Word;                                { Mouse position }


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

{$ifdef go32v2}
{ tells the mouse unit to draw the mouse cursor itself }
procedure DoCustomMouse(b : boolean);
{$endif go32v2}

implementation

{ Include platform dependent routines }

{$i mouse.inc}

{ Platform independent routines }
{$IFNDEF OS2}
procedure PutMouseEvent(const MouseEvent: TMouseEvent);
begin
  if PendingMouseEvents<MouseEventBufSize then
   begin
     PendingMouseTail^:=MouseEvent;
     inc(PendingMouseTail);
     if longint(PendingMouseTail)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
      PendingMouseTail:=@PendingMouseEvent;
      { why isn't this done here ?
        so the win32 version do this by hand:
       inc(PendingMouseEvents); }
   end
  else
end;
{$ENDIF}
end.
{
  $Log$
  Revision 1.5  2000-03-12 15:05:14  peter
    * $G+ is only needed for tp

  Revision 1.4  2000/02/29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.3  2000/02/07 22:54:44  florian
    * custommouse define removed, i.e. code is always active
    * the xor value for the mouse cursor must be $7f instead of $ff

  Revision 1.2  2000/02/06 14:28:19  florian
    * mouse support for vesa resolutions under go32v2, needs currently the define
      custommouse

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.2  1999/12/31 17:25:24  marco


  Added $G+, TP version required it.

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.5  1999/07/29 11:38:56  peter
    * fixed comment for tp7

  Revision 1.4  1999/07/17 22:37:07  florian
    * implemented mouse handling

  Revision 1.3  1999/04/23 17:54:58  hajny
  PutMouseEvent modified for support of two queues in OS/2

  Revision 1.2  1998/12/11 00:13:17  peter
    + SetMouseXY
    * use far for exitproc procedure

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.1  1998/10/28 00:02:07  peter
    + mouse
    + video.clearscreen, video.videobufsize

}