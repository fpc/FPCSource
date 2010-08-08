{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by B'rczi, Gÿbor
    member of the Free Pascal development team

    Support objects for the install program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Scroll;

interface

uses Objects,
     FVConsts,
     Drivers,Views,App;

const
    CScrollBoxBackground = #6;

type
    PScrollBoxBackground = ^TScrollBoxBackground;
    TScrollBoxBackground = object(TBackground)
      function GetPalette: PPalette; virtual;
    end;

    PScrollBox = ^TScrollBox;
    TScrollBox = object(TGroup)
      Delta,Limit: TPoint;
      HScrollBar,VScrollBar: PScrollBar;
      Background: PScrollBoxBackground;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure InitBackground; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      procedure ChangeBounds(var Bounds: TRect); virtual;
      procedure ScrollDraw; virtual;
      procedure ScrollTo(X, Y: Sw_Integer);
      procedure SetLimit(X, Y: Sw_Integer);
      procedure SetState(AState: Word; Enable: Boolean); virtual;
      procedure TrackCursor;
      procedure Draw; virtual;
      function  ClipChilds: boolean; virtual;
      procedure BeforeInsert(P: PView); virtual;
      procedure AfterInsert(P: PView); virtual;
      procedure AfterDelete(P: PView); virtual;
    private
      DrawLock: Byte;
      DrawFlag: Boolean;
      ScrollFlag : boolean;
      procedure CheckDraw;
      procedure UpdateLimits;
      procedure ShiftViews(DX,DY: sw_integer);
    end;

implementation

function TScrollBoxBackground.GetPalette: PPalette;
const P: string[length(CScrollBoxBackground)] = CScrollBoxBackground;
begin
  GetPalette:=@P;
end;

constructor TScrollBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds);
  EventMask:=EventMask or evBroadcast;
  HScrollBar:=AHScrollBar; VScrollBar:=AVScrollBar;
  InitBackground;
  if Assigned(Background) then Insert(Background);
  ReDraw;
end;

procedure TScrollBox.InitBackground;
var R: TRect;
begin
  GetExtent(R);
  New(Background, Init(R,' '));
end;

procedure TScrollBox.HandleEvent(var Event: TEvent);
begin
  if (Event.What=evBroadcast) and (Event.Command=cmCursorChanged) then
    TrackCursor;
   If (Event.What = evBroadcast) AND
     (Event.Command = cmScrollBarChanged) AND         { Scroll bar change }
     Not ScrollFlag AND
     ((Event.InfoPtr = HScrollBar) OR                 { Our scrollbar? }
      (Event.InfoPtr = VScrollBar)) Then ScrollDraw;  { Redraw scroller }
  inherited HandleEvent(Event);
end;

procedure TScrollBox.ChangeBounds(var Bounds: TRect);
begin
  SetBounds(Bounds);
  Inc(DrawLock);
  SetLimit(Limit.X, Limit.Y);
  Dec(DrawLock);
  DrawFlag := False;
  DrawView;
end;

procedure TScrollBox.CheckDraw;
begin
  if (DrawLock = 0) and DrawFlag then
  begin
    DrawFlag := False;
    ReDraw; DrawView;
  end;
end;

procedure TScrollBox.ScrollDraw;
var
  D: TPoint;
begin
  if HScrollBar <> nil then
   D.X := HScrollBar^.Value
  else
   D.X := 0;
  if VScrollBar <> nil then
   D.Y := VScrollBar^.Value
  else
   D.Y := 0;
  if (D.X <> Delta.X) or (D.Y <> Delta.Y) then
   begin
     SetCursor(Cursor.X + Delta.X - D.X, Cursor.Y + Delta.Y - D.Y);
     ScrollTo(D.X,D.Y);
     if DrawLock <> 0 then
      DrawFlag := True
     else
      DrawView;
   end;
end;


procedure TScrollBox.ScrollTo(X, Y: Sw_Integer);
var DX,DY: sw_integer;
    PrevScrollFlag : boolean;
begin
  Inc(DrawLock);
  DX:=Delta.X-X;
  DY:=Delta.Y-Y;
  PrevScrollFlag:=ScrollFlag;
  ScrollFlag:=true;

  if HScrollBar <> nil then
   HScrollBar^.SetValue(X);
  if VScrollBar <> nil then
   VScrollBar^.SetValue(Y);
  ScrollFlag:=PrevScrollFlag;
  ShiftViews(DX,DY);
  Dec(DrawLock);
  CheckDraw;
end;

procedure TScrollBox.ShiftViews(DX,DY: sw_integer);
  procedure DoShift(P: PView);
  begin
    P^.MoveTo(P^.Origin.X+DX,P^.Origin.Y+DY);
  end;
begin
  ForEach(@DoShift);
  Delta.X:=Delta.X-DX;
  Delta.Y:=Delta.Y-DY;
end;

procedure TScrollBox.SetLimit(X, Y: Sw_Integer);
begin
  Limit.X := X;
  Limit.Y := Y;
  Inc(DrawLock);
  if HScrollBar <> nil then
    HScrollBar^.SetParams(HScrollBar^.Value, HScrollBar^.Min, HScrollBar^.Max, HScrollBar^.PgStep, HScrollBar^.ArStep);
  if VScrollBar <> nil then
    VScrollBar^.SetParams(VScrollBar^.Value, VScrollBar^.Min, VScrollBar^.Max, VScrollBar^.PgStep, VScrollBar^.ArStep);
  Dec(DrawLock);
  CheckDraw;
end;

procedure TScrollBox.SetState(AState: Word; Enable: Boolean);
  procedure ShowSBar(SBar: PScrollBar);
  begin
    if (SBar <> nil) then
      if GetState(sfActive + sfSelected) then
        SBar^.Show
      else
        SBar^.Hide;
  end;
var OState: word;
begin
  OState:=State;
  inherited SetState(AState, Enable);
  if AState and (sfActive + sfSelected) <> 0 then
   begin
     ShowSBar(HScrollBar);
     ShowSBar(VScrollBar);
   end;
  if ((OState xor State) and (sfFocused))<>0 then
    TrackCursor;
end;

procedure TScrollBox.TrackCursor;
var V: PView;
    P,ND: TPoint;
begin
  V:=Current;
  if (not Assigned(V)) then Exit;
  P.X:=V^.Origin.X+V^.Cursor.X;
  P.Y:=V^.Origin.Y+V^.Cursor.Y;
  ND:=Delta;
  if (P.X<0) then
    Dec(ND.X,-P.X)
  else
    if (P.X>=Size.X) then
      Inc(ND.X,P.X-(Size.X-1));
  if (P.Y<0) then
    Dec(ND.Y,-P.Y)
  else
    if (P.Y>=Size.Y) then
      Inc(ND.Y,P.Y-(Size.Y-1));
  if (ND.X<>Delta.X) or (ND.Y<>Delta.Y) then
    ScrollTo(ND.X,ND.Y);
end;

function TScrollBox.ClipChilds: boolean;
begin
  ClipChilds:=false;
end;

procedure TScrollBox.BeforeInsert(P: PView);
begin
  if Assigned(P) then
    P^.MoveTo(P^.Origin.X-Delta.X,P^.Origin.Y-Delta.Y);
end;

procedure TScrollBox.AfterInsert(P: PView);
begin
  UpdateLimits;
end;

procedure TScrollBox.AfterDelete(P: PView);
begin
  { UpdateLimits;
    removed because it creates GPF PM }
end;

procedure TScrollBox.Draw;
begin
  inherited Draw;
end;

procedure TScrollBox.UpdateLimits;
var Max: TPoint;

  procedure Check(P: PView);
  var O: TPoint;
  begin
    O.X:=P^.Origin.X+P^.Size.X+Delta.X;
    O.Y:=P^.Origin.Y+P^.Size.Y+Delta.Y;
    if O.X>Max.X then
      Max.X:=O.X;
    if O.Y>Max.Y then
      Max.Y:=O.Y;
  end;

begin
  Max.X:=0; Max.Y:=0;
  ForEach(@Check);
  if (Max.X<>Limit.X) or (Max.Y<>Limit.Y) then
    SetLimit(Max.X,Max.Y);
end;

END.
