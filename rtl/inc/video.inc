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

Const
  LockUpdateScreen : Integer = 0;

Procedure LockScreenUpdate;

begin
  Inc(LockUpdateScreen);
end;

Procedure UnLockScreenUpdate;

begin
  Dec(LockUpdateScreen);
end;

Var 
  CurrentVideoDriver : TVideoDriver;

Const
  VideoInitialized : Boolean = False;
  
Procedure SetVideoDriver (Const Driver : TVideoDriver);
{ Sets the videodriver to be used }
begin
  If Not VideoInitialized then
    CurrentVideoDriver:=Driver;
end;

Procedure GetVideoDriver (Var Driver : TVideoDriver);
{ Retrieves the current videodriver }                                           
begin
  Driver:=CurrentVideoDriver;
end;

{ ---------------------------------------------------------------------
  External functions that use the video driver.
  ---------------------------------------------------------------------}

Procedure InitVideo;

begin
  If Not VideoInitialized then
    begin
    If Assigned(CurrentVideoDriver.InitDriver) then
      CurrentVideoDriver.InitDriver;
    VideoInitialized:=True;  
    end;  
end;

Procedure DoneVideo;

begin
  If VideoInitialized then
    begin
    If Assigned(CurrentVideoDriver.DoneDriver) then
      CurrentVideoDriver.DoneDriver;
    VideoInitialized:=False;  
    end;  
end;

Procedure UpdateScreen (Force : Boolean);

begin
  If (LockUpdateScreen<=0) and
     Assigned(CurrentVideoDriver.UpdateScreen) then
      CurrentVideoDriver.UpdateScreen(Force);  
end;

Procedure ClearScreen;

begin
  If Assigned(CurrentVideoDriver.ClearScreen) then
    CurrentVideoDriver.ClearScreen
  else
    begin
    FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
    UpdateScreen(True);
    // Is this needed ? 
    {
    CurrentX:=1;
    CurrentY:=1;
    }
    end;
end;

Procedure SetCursorType (NewType : Word);

begin
  if Assigned(CurrentVideoDriver.SetCursorType) then
    CurrentVideoDriver.SetCursorType(NewType)
end;

Function GetCursorType : Word;

begin
  if Assigned(CurrentVideoDriver.GetCursorType) then
    GetCursorType:=CurrentVideoDriver.GetCursorType()
  else
    GetCursorType:=0;
end;

procedure SetCursorPos(NewCursorX, NewCursorY: Word);

begin
  If Assigned(CurrentVideoDriver.SetCursorPos) then
    CurrentVideoDriver.SetCursorPos(NewCursorX, NewCursorY)
end;

function GetCapabilities: Word;
begin
  If Assigned(CurrentVideoDriver.GetCapabilities) then
    GetCapabilities:=CurrentVideoDriver.GetCapabilities()
  else
    GetCapabilities:=0;  
end;
    

{ ---------------------------------------------------------------------
    General functions
  ---------------------------------------------------------------------}
  

procedure GetVideoMode(var Mode: TVideoMode);
begin
  Mode.Col := ScreenWidth;
  Mode.Row := ScreenHeight;
  Mode.Color := ScreenColor;
end;



procedure SetVideoMode(Mode: TVideoMode);
var
  P: PVideoModeList;
begin
  P := Modes;
  while (P<>Nil) and ((P^.Row <> Mode.Row) or (P^.Col <> Mode.Col) or (P^.Color<>Mode.Color)) do
    P := P^.Next;
  if P <> nil then begin
    DoneVideo;
    ScreenWidth:=$ffff;
    ScreenHeight:=$ffff;
    P^.VideoModeSelector(PVideoMode(P)^, P^.Params);
    InitVideo;
   end
   else begin
    ErrorHandler(errVioNoSuchMode, @Mode);
  end;
end;

procedure RegisterVideoMode(Col, Row: Word; Color: Boolean; VideoModeSelector: TVideoModeSelector; Params: Longint);
var
  P: PVideoModeList;
begin
  New(P);
  P^.Col := Col;
  P^.Row := Row;
  P^.Color := Color;
  P^.VideoModeSelector := VideoModeSelector;
  P^.Params := Params;
  P^.Next := Modes;
  Modes := P;
end;


procedure UnRegisterVideoModes;
var
  P: PVideoModeList;
begin
  while assigned(modes) do
   begin
     p:=modes;
     modes:=modes^.next;
     dispose(p);
   end;
end;

function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
begin
  ErrorCode := AErrorCode;
  ErrorInfo := AErrorInfo;
  DefaultErrorHandler := errAbort; { return error code }
end;



{
  $Log$
  Revision 1.2  2001-09-21 19:50:18  michael
  + Merged driver support from fixbranch

  Revision 1.1.2.2  2001/09/21 18:42:08  michael
  + Implemented support for custom video drivers.

  Revision 1.1.2.1  2001/01/30 22:21:22  peter
    * move api to rtl

  Revision 1.1  2001/01/13 11:13:12  peter
    * API 2 RTL

}

