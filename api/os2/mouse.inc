{
  System independent mouse interface for OS/2

  $Id$
}

uses
 Video,
{$IFDEF PPC_VIRTUAL}
 OS2Base;
{$ELSE}
 {$IFDEF PPC_FPC}
 MouCalls, DosCalls;
 {$ELSE}
  {$IFDEF PPC_SPEED}
 BseSub, BseDos;
  {$ELSE}
   {$IFDEF PPC_BPOS2}
 OS2Subs, DosProcs;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF PPC_VIRTUAL}
type
 cardinal = longint;
 TMouEventInfo = MouEventInfo;
 TNoPtrRect = NoPtrRect;
 TPtrLoc = PtrLoc;
 TMouQueInfo = MouQueInfo;
{$ELSE}
 {$IFDEF PPC_SPEED}
type
 cardinal = longword;
 TMouEventInfo = MouEventInfo;
 TNoPtrRect = NoPtrRect;
 TPtrLoc = PtrLoc;
 TMouQueInfo = MouQueInfo;
 {$ELSE}
  {$IFDEF PPC_BPOS2}
type
 cardinal = longint;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

var
 PendingMouseEventOrder: array [0..MouseEventBufSize-1] of cardinal;
 MouseEventOrderHead, MouseEventOrderTail: cardinal;

const
 NoMouse = $FFFF;
 DefaultMouse = 0;
 Handle: word = DefaultMouse;
 HideCounter: cardinal = 0;
 OldEventMask: longint = -1;

procedure InitMouse;
var
 Loc: TPtrLoc;
 SetPrev: boolean;
 SysEvent: TMouEventInfo;
 QI: TMouQueInfo;
 W: word;
begin
 SetPrev := MouGetPtrPos (Loc, DefaultMouse) = 0;
 if MouGetEventMask (W, DefaultMouse) = 0 then OldEventMask := W;
 PendingMouseHead := @PendingMouseEvent;
 PendingMouseTail := @PendingMouseEvent;
 PendingMouseEvents := 0;
 FillChar (LastMouseEvent, SizeOf (TMouseEvent), 0);
 MouseEventOrderTail := 0;
 MouseEventOrderHead := 0;
 HideCounter := 0;
 if MouOpen (nil, Handle) = Error_Mouse_No_Device then Handle := NoMouse else
 begin
  W := Mou_NoWait;
  repeat
   MouGetNumQueEl (QI, Handle);
   if QI.cEvents <> 0 then MouReadEventQue (SysEvent, W, Handle);
  until QI.cEvents = 0;
  W := $FFFF;
  MouSetEventMask (W, Handle);
  if SetPrev then MouSetPtrPos (Loc, Handle);

(*
 It would be possible to issue a MouRegister call here to hook our own mouse
 handler, but such handler would have to be in a DLL and it's questionable,
 whether there would be so many advantages in doing so.
*)

  MouDrawPtr (Handle);
 end;
end;

procedure DoneMouse;
var
 W: word;
begin
 if (Handle <> NoMouse) and (Handle <> DefaultMouse) then
 begin

(*
 If our own mouse handler would be installed in InitMouse, MouDeregister would
 have appeared here.
*)

  HideCounter := 0;
  HideMouse;
  MouClose (Handle);
 end;
 if OldEventMask <> -1 then
 begin
  W := OldEventMask;
  MouSetEventMask (W, 0);
 end;
end;

function DetectMouse:byte;
var
 Buttons: word;
begin
 if MouGetNumButtons (Buttons, DefaultMouse) = 0 then DetectMouse := Buttons
                                                         else DetectMouse := 0;
end;

procedure ShowMouse;
begin
 if Handle <> NoMouse then
 begin
  if HideCounter <> 0 then
  begin
   Dec (HideCounter);
   if HideCounter = 0 then MouDrawPtr (Handle);
  end;
 end;
end;

procedure HideMouse;
var
 PtrRect: TNoPtrRect;
begin
 if Handle <> NoMouse then
 begin
  Inc (HideCounter);
  case HideCounter of
   0: Dec (HideCounter); (* HideCounter overflowed - stay at the maximum *)
   1: begin
       PtrRect.Row := 0;
       PtrRect.Col := 0;
       PtrRect.cRow := Pred (ScreenHeight);
       PtrRect.cCol := Pred (ScreenWidth);
       MouRemovePtr (PtrRect, Handle);
      end;
  end;
 end;
end;

function GetMouseX: word;
var
 Event: TMouseEvent;
begin
 if Handle = NoMouse then GetMouseX := 0 else
 begin
  PollMouseEvent (Event);
  GetMouseX := Event.X;
 end;
end;

function GetMouseY: word;
var
 Event: TMouseEvent;
begin
 if Handle = NoMouse then GetMouseY := 0 else
 begin
  PollMouseEvent (Event);
  GetMouseY := Event.Y;
 end;
end;

procedure GetMouseXY (var X: word; var Y: word);
var
 Loc: TPtrLoc;
begin
 if Handle = NoMouse then
 begin
  X := 0;
  Y := 0;
 end else if MouGetPtrPos (Loc, Handle) <> 0 then
 begin
  X := $FFFF;
  Y := $FFFF;
 end else
 begin
  X := Loc.Col;
  Y := Loc.Row;
 end;
end;

procedure SetMouseXY (X, Y: word);
var
 Loc: TPtrLoc;
begin
 if Handle <> NoMouse then
 begin
  Loc.Row := Y;
  Loc.Col := X;
  MouSetPtrPos (Loc, Handle);
 end;
end;

procedure TranslateEvents (const SysEvent: TMouEventInfo;
                                                       var Event: TMouseEvent);
begin
 Event.Buttons := 0;
 Event.Action := 0;
 if SysEvent.fs and (Mouse_Motion_With_BN1_Down or Mouse_BN1_Down) <> 0 then
                             Event.Buttons := Event.Buttons or MouseLeftButton;
 if SysEvent.fs and (Mouse_Motion_With_BN2_Down or Mouse_BN2_Down) <> 0 then
                            Event.Buttons := Event.Buttons or MouseRightButton;
 if SysEvent.fs and (Mouse_Motion_With_BN3_Down or Mouse_BN3_Down) <> 0 then
                           Event.Buttons := Event.Buttons or MouseMiddleButton;
 Event.X := SysEvent.Col;
 Event.Y := SysEvent.Row;
 if Event.Buttons <> LastMouseEvent.Buttons then
  if (Event.Buttons and MouseLeftButton = 0) and
      (LastMouseEvent.Buttons and MouseLeftButton = MouseLeftButton)
                                   then Event.Action := MouseActionUp else
  if (Event.Buttons and MouseRightButton = 0) and
      (LastMouseEvent.Buttons and MouseRightButton = MouseRightButton)
                                   then Event.Action := MouseActionUp else
  if (Event.Buttons and MouseMiddleButton = 0) and
   (LastMouseEvent.Buttons and MouseMiddleButton = MouseMiddleButton)
    then Event.Action := MouseActionUp
     else Event.Action := MouseActionDown
      else if (Event.X <> LastMouseEvent.X) or (Event.Y <> LastMouseEvent.Y)
                                          then Event.Action := MouseActionMove;
 LastMouseEvent := Event;
end;

procedure NullOrder;
var
 I: cardinal;
begin
 if PendingMouseEvents > 0 then
 begin
  I := MouseEventOrderHead;
  repeat
   PendingMouseEventOrder [I] := 0;
   if I = Pred (MouseEventBufSize) then I := 0 else Inc (I);
  until (I <> MouseEventOrderTail);
 end;
end;

procedure LowerOrder;
var
 I: cardinal;
begin
 if PendingMouseEvents > 0 then
 begin
  I := MouseEventOrderHead;
  repeat
   if PendingMouseEventOrder [I] <> 0 then
   begin
    Dec (PendingMouseEventOrder [I]);
    if I = Pred (MouseEventBufSize) then I := 0 else Inc (I);
   end;
  until (I <> MouseEventOrderTail) or (PendingMouseEventOrder [I] = 0);
 end;
end;

function PollMouseEvent (var MouseEvent: TMouseEvent) :boolean;
var
 SysEvent: TMouEventInfo;
 P, Q: PMouseEvent;
 Event: TMouseEvent;
 WF: word;
 QI: TMouQueInfo;
begin
 if (PendingMouseEvents = 0) or
         (PendingMouseEventOrder [MouseEventOrderHead] <> 0) and
                                  (PendingMouseEvents < MouseEventBufSize) then
 begin
  MouGetNumQueEl (QI, Handle);
  if QI.cEvents = 0 then NullOrder else
  begin
   LowerOrder;
   WF := Mou_NoWait;
   if (MouReadEventQue (SysEvent, WF, Handle) = 0) then
   begin
    if PendingMouseHead = @PendingMouseEvent then
                           P := @PendingMouseEvent [MouseEventBufSize - 1] else
    begin
     P := PendingMouseHead;
     Dec (P);
    end;
    TranslateEvents (SysEvent, P^);
    if P^.Action <> 0 then
    begin
     if PendingMouseEvents < MouseEventBufSize then
     begin
      Q := P;
      WF := Mou_NoWait;
      while (P^.Action = MouseActionMove) and
       (PendingMouseEventOrder [MouseEventOrderHead] <> 0) and
         (MouReadEventQue (SysEvent, WF, Handle) = 0) and
                       ((SysEvent.fs <> 0) or (LastMouseEvent.Buttons <> 0)) do
      begin
       LowerOrder;
       TranslateEvents (SysEvent, Event);
       if Event.Action <> MouseActionMove then
       begin
        if Q = @PendingMouseEvent then
                  Q := @PendingMouseEvent [MouseEventBufSize - 1] else Dec (Q);
        if MouseEventOrderHead = 0 then
                  MouseEventOrderHead := MouseEventBufSize - 1 else
                                                     Dec (MouseEventOrderHead);
        PendingMouseEventOrder [MouseEventOrderHead] := 0;
        Q^ := P^;
        Inc (PendingMouseEvents);
        if MouseEventOrderHead = 0 then
               MouseEventOrderHead := MouseEventBufSize - 1 else
                                                     Dec (MouseEventOrderHead);
        PendingMouseEventOrder [MouseEventOrderHead] := 0;
       end else WF := Mou_NoWait;
       P^ := Event;
      end;
      P := Q;
     end;
     Inc (PendingMouseEvents);
     if MouseEventOrderHead = 0 then
               MouseEventOrderHead := MouseEventBufSize - 1 else
                                                     Dec (MouseEventOrderHead);
     PendingMouseEventOrder [MouseEventOrderHead] := 0;
     PendingMouseHead := P;
    end;
   end else NullOrder;
  end;
 end;
 if PendingMouseEvents <> 0 then
 begin
  MouseEvent := PendingMouseHead^;
  LastMouseEvent := MouseEvent;
  PollMouseEvent := true;
 end else
 begin
  PollMouseEvent := false;
  MouseEvent := LastMouseEvent;
  MouseEvent.Action := 0;
 end;
end;

function GetMouseButtons: word;
var
 Event: TMouseEvent;
begin
 PollMouseEvent (Event);
 GetMouseButtons := Event.Buttons;
end;

procedure GetMouseEvent (var MouseEvent: TMouseEvent);
var
 Event: TMouEventInfo;
begin
 if (PendingMouseEvents = 0) or
                       (PendingMouseEventOrder [MouseEventOrderHead] <> 0) then
 repeat
  DosSleep (1);
  PollMouseEvent (MouseEvent);
 until (PendingMouseEvents <> 0) and
                        (PendingMouseEventOrder [MouseEventOrderHead] = 0) else
 begin
  MouseEvent := PendingMouseHead^;
  LastMouseEvent := MouseEvent;
 end;
 Inc (PendingMouseHead);
 if longint (PendingMouseHead) = longint (@PendingMouseEvent)
      + SizeOf (PendingMouseEvent) then PendingMouseHead := @PendingMouseEvent;
 Inc (MouseEventOrderHead);
 if MouseEventOrderHead = MouseEventBufSize then MouseEventOrderHead := 0;
 Dec (PendingMouseEvents);
end;

procedure PutMouseEvent (const MouseEvent: TMouseEvent);
var
 QI: TMouQueInfo;
begin
 if PendingMouseEvents < MouseEventBufSize then
 begin
  PendingMouseTail^ := MouseEvent;
  Inc (PendingMouseTail);
  if longint (PendingMouseTail) = longint (@PendingMouseEvent) +
        SizeOf (PendingMouseEvent) then PendingMouseTail := @PendingMouseEvent;
  MouGetNumQueEl (QI, Handle);
  PendingMouseEventOrder [MouseEventOrderTail] := QI.cEvents;
  Inc (MouseEventOrderTail);
  if MouseEventOrderTail = MouseEventBufSize then MouseEventOrderTail := 0;
  Inc (PendingMouseEvents);
 end;
end;

{
  $Log$
  Revision 1.3  2000-04-10 15:50:55  hajny
    * Inc (PendingMouseEvents) added to PutMouseEvent

  Revision 1.2  2000/01/09 20:43:04  hajny
    * cardinal = longint removed for FPC

}
