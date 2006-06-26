{
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
  If LockUpdateScreen>0 then
    Dec(LockUpdateScreen);
end;

Function GetLockScreenCount : integer;
begin
  GetLockScreenCount:=LockUpdateScreen;
end;

Var
  CurrentVideoDriver : TVideoDriver;
  NextVideoMode      : TVideoMode;

Const
  VideoInitialized : Boolean = False;
  DriverInitialized : Boolean = False;
  NextVideoModeSet  : Boolean = False;

Function SetVideoDriver (Const Driver : TVideoDriver) : Boolean;
{ Sets the videodriver to be used }
begin
  If Not VideoInitialized then
    Begin
    CurrentVideoDriver:=Driver;
      DriverInitialized:=true;
      NextVideoModeSet:=false;
    End;
  SetVideoDriver:=Not VideoInitialized;
end;

Procedure GetVideoDriver (Var Driver : TVideoDriver);
{ Retrieves the current videodriver }
begin
  Driver:=CurrentVideoDriver;
end;

{ ---------------------------------------------------------------------
  External functions that use the video driver.
  ---------------------------------------------------------------------}

Procedure FreeVideoBuf;

begin
  if (VideoBuf<>Nil) then
    begin
    FreeMem(VideoBuf);
    FreeMem(OldVideoBuf);
    VideoBuf:=Nil;
    OldVideoBuf:=Nil;
    VideoBufSize:=0;
    end;
end;

(*
Procedure AssignVideoBuf (OldCols, OldRows : Word);

Var NewVideoBuf,NewOldVideoBuf : PVideoBuf;
    I,C,R,NewVideoBufSize : longint;
    s:word;

begin
  S:=sizeOf(TVideoCell);
  NewVideoBufSize:=ScreenWidth*ScreenHeight*s;
  GetMem(NewVideoBuf,NewVideoBufSize);
  GetMem(NewOldVideoBuf,NewVideoBufSize);
  // Move contents of old videobuffers to new if there are any.
  if (VideoBuf<>Nil) then
    begin
    If (ScreenWidth<OldCols) then
      C:=ScreenWidth
    else
      C:=OldCols;
    If (ScreenHeight<OldRows) then
      R:=ScreenHeight
    else
      R:=OldRows;
    For I:=0 to R-1 do
      begin
      Move(VideoBuf^[I*OldCols],NewVideoBuf^[I*ScreenWidth],S*C);
      Move(OldVideoBuf^[I*OldCols],NewOldVideoBuf^[I*ScreenWidth],S*C);
      end;
    end;
  FreeVideoBuf;
  VideoBufSize:=NewVideoBufSize;
  VideoBuf:=NewVideoBuf;
  OldVideoBuf:=NewOldVideoBuf;
end;
*)
Procedure AssignVideoBuf (OldCols, OldRows : Word);

var NewVideoBuf,NewOldVideoBuf:PVideoBuf;
    old_rowstart,new_rowstart:word;
    NewVideoBufSize : longint;

begin
  NewVideoBufSize:=ScreenWidth*ScreenHeight*sizeof(TVideoCell);
  GetMem(NewVideoBuf,NewVideoBufSize);
  GetMem(NewOldVideoBuf,NewVideoBufSize);
  {Move contents of old videobuffers to new if there are any.}
  if VideoBuf<>nil then
    begin
      if ScreenWidth<OldCols then
        OldCols:=ScreenWidth;
      if ScreenHeight<OldRows then
        OldRows:=ScreenHeight;
      old_rowstart:=0;
      new_rowstart:=0;
      while oldrows>0 do
        begin
          move(VideoBuf^[old_rowstart],NewVideoBuf^[new_rowstart],OldCols*sizeof(TVideoCell));
          move(OldVideoBuf^[old_rowstart],NewOldVideoBuf^[new_rowstart],OldCols*sizeof(TVideoCell));
          inc(old_rowstart,OldCols);
          inc(new_rowstart,ScreenWidth);
          dec(OldRows);
        end;
    end;
  FreeVideoBuf;
  { FreeVideoBuf sets VideoBufSize to 0 }
  VideoBufSize:=NewVideoBufSize;
  VideoBuf:=NewVideoBuf;
  OldVideoBuf:=NewOldVideoBuf;
end;

Procedure InitVideo;

begin
  if not VideoInitialized then
    begin
      if Assigned(CurrentVideoDriver.InitDriver) then
        CurrentVideoDriver.InitDriver;
      if errorcode=viook then
        begin
          VideoInitialized:=true;
          if NextVideoModeSet then
            SetVideoMode(NextVideoMode)
          else
            AssignVideoBuf(0,0);
          ClearScreen;
        end;
    end;
end;


Procedure DoneVideo;

begin
  If VideoInitialized then
    begin
    If Assigned(CurrentVideoDriver.DoneDriver) then
      CurrentVideoDriver.DoneDriver;
    FreeVideoBuf;
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
  // Should this not be the current color ?
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
  If Assigned(CurrentVideoDriver.ClearScreen) then
    CurrentVideoDriver.ClearScreen
  else
    UpdateScreen(True);
  FillWord(OldVideoBuf^,VideoBufSize shr 1,$0720);
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

Function SetVideoMode(Const Mode: TVideoMode) : Boolean;

Var
  OldR,OldC: Word;

begin
  SetVideoMode:=DriverInitialized;
  if not DriverInitialized then
    exit;
  If VideoInitialized then
    begin
      OldC:=ScreenWidth;
      OldR:=ScreenHeight;
      If Assigned(CurrentVideoDriver.SetVideoMode) then
        SetVideoMode:=CurrentVideoDriver.SetVideoMode(Mode)
      else
        SetVideoMode:=False;
      // Assign buffer
      If SetVideoMode then
        AssignVideoBuf(OldC,Oldr);
    end
  else
    begin
      NextVideoMode:=Mode;
      NextVideoModeSet:=true;
    end;
end;


Function GetVideoModeCount : Word;

begin
  If Assigned(CurrentVideoDriver.GetVideoModeCount) then
    GetVideoModeCount:=CurrentVideoDriver.GetVideoModeCount()
  else
    GetVideoModeCount:=1;
end;

Function GetVideoModeData(Index : Word; Var Data: TVideoMode) : Boolean;

begin
  If Assigned(CurrentVideoDriver.GetVideoModeData) then
    GetVideoModeData:=CurrentVideoDriver.GetVideoModeData(Index,Data)
  else
    begin
    GetVideoModeData:=(Index=0);
    If GetVideoModeData then
      GetVideoMode(Data);
    end
end;

function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
begin
  ErrorCode := AErrorCode;
  ErrorInfo := AErrorInfo;
  DefaultErrorHandler := errAbort; { return error code }
end;


