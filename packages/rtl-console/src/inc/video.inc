{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ TEnhancedVideoCell }

operator = (const a,b : TEnhancedVideoCell) res: Boolean;
begin
  res:=(a.Attribute=b.Attribute) and (a.ExtendedGraphemeCluster = b.ExtendedGraphemeCluster);
end;

function TEnhancedVideoCell.GetAttribute: Byte;
begin
  GetAttribute := Byte(FAttributes);
end;

procedure TEnhancedVideoCell.SetAttribute(Attr: Byte);
begin
  FAttributes := (FAttributes and $FF00) or Attr;
end;

function TEnhancedVideoCell.GetExtendedGraphemeCluster: UnicodeString;
begin
  if (FAttributes and $8000) = 0 then
    GetExtendedGraphemeCluster := EGC_SingleChar
  else
    GetExtendedGraphemeCluster := UnicodeString(EGC_WideStr);
end;

procedure TEnhancedVideoCell.SetExtendedGraphemeCluster(const AExtendedGraphemeCluster: UnicodeString);
begin
  if Length(AExtendedGraphemeCluster) = 1 then
  begin
    if (FAttributes and $8000) <> 0 then
    begin
      FAttributes := FAttributes and $7FFF;
      UnicodeString(EGC_WideStr) := '';
    end;
    EGC_SingleChar := AExtendedGraphemeCluster[1];
  end
  else
  begin
    if (FAttributes and $8000) = 0 then
    begin
      FAttributes := FAttributes or $8000;
      EGC_WideStr := nil;
    end;
    UnicodeString(EGC_WideStr) := AExtendedGraphemeCluster;
  end;
end;

class operator TEnhancedVideoCell.Initialize(var evc: TEnhancedVideoCell);
begin
  evc.FAttributes := 0;
end;

class operator TEnhancedVideoCell.Finalize(var evc: TEnhancedVideoCell);
begin
  if (evc.FAttributes and $8000) <> 0 then
    UnicodeString(evc.EGC_WideStr) := '';
end;

Procedure fpc_UnicodeStr_Incr_Ref(S : Pointer); external name 'FPC_UNICODESTR_INCR_REF';

class operator TEnhancedVideoCell.AddRef(var evc: TEnhancedVideoCell);
begin
  if (evc.FAttributes and $8000) <> 0 then
    fpc_UnicodeStr_Incr_Ref(evc.EGC_WideStr);
end;

class operator TEnhancedVideoCell.Copy(constref aSrc: TEnhancedVideoCell; var aDst: TEnhancedVideoCell);
begin
  aDst.ExtendedGraphemeCluster := aSrc.ExtendedGraphemeCluster;
  aDst.Attribute := aSrc.Attribute;
end;

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
  EnhancedVideoInitialized : Boolean = False;
  DriverInitialized : Boolean = False;
  NextVideoModeSet  : Boolean = False;

Function SetVideoDriver (Const Driver : TVideoDriver) : Boolean;
{ Sets the videodriver to be used }
begin
  if (not VideoInitialized) and (not EnhancedVideoInitialized) then
    begin
      CurrentVideoDriver:=Driver;
      DriverInitialized:=true;
      NextVideoModeSet:=false;
      SetVideoDriver:=true;
    end
  else
    SetVideoDriver:=false;
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

procedure FreeEnhancedVideoBuf;
begin
  SetLength(EnhancedVideoBuf,0);
  SetLength(OldEnhancedVideoBuf,0);
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
  if VideoInitialized or Assigned(CurrentVideoDriver.InitDriver) then
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
  if EnhancedVideoInitialized or Assigned(CurrentVideoDriver.InitEnhancedDriver) then
    begin
      NewVideoBufSize:=ScreenWidth*ScreenHeight;
      { todo: move contents of old to new buf, so the rows match, when the width changes }
      SetLength(EnhancedVideoBuf,NewVideoBufSize);
      SetLength(OldEnhancedVideoBuf,NewVideoBufSize);
    end;
end;

Procedure InitVideo;

begin
  if not VideoInitialized then
    begin
      if Assigned(CurrentVideoDriver.InitEnhancedDriver) then
        CurrentVideoDriver.InitEnhancedDriver
      else if Assigned(CurrentVideoDriver.InitDriver) then
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
    FreeEnhancedVideoBuf;
    VideoInitialized:=False;
    end;
end;

procedure InitEnhancedVideo;
begin
  if not EnhancedVideoInitialized then
    begin
      if Assigned(CurrentVideoDriver.InitEnhancedDriver) then
        CurrentVideoDriver.InitEnhancedDriver
      else if Assigned(CurrentVideoDriver.InitDriver) then
        CurrentVideoDriver.InitDriver;
      if errorcode=viook then
        begin
          EnhancedVideoInitialized:=true;
          if NextVideoModeSet then
            SetVideoMode(NextVideoMode)
          else
            AssignVideoBuf(0,0);
          ClearScreen;
        end;
    end;
end;

procedure DoneEnhancedVideo;
begin
  if EnhancedVideoInitialized then
    begin
      if Assigned(CurrentVideoDriver.DoneDriver) then
        CurrentVideoDriver.DoneDriver;
      FreeVideoBuf;
      FreeEnhancedVideoBuf;
      EnhancedVideoInitialized:=False;
    end;
end;

function ExtendedGraphemeCluster2LegacyChar(const EGC: UnicodeString): Char;
begin
  if (Length(EGC) = 1) then
    case Ord(EGC[1]) of
      0,32..126:
        Result:=Chr(Ord(EGC[1]));
      $263A:
        Result:=#1;
      $263B:
        Result:=#2;
      $2665:
        Result:=#3;
      $2666:
        Result:=#4;
      $2663:
        Result:=#5;
      $2660:
        Result:=#6;
      $2022:
        Result:=#7;
      $25D8:
        Result:=#8;
      $25CB:
        Result:=#9;
      $25D9:
        Result:=#10;
      $2642:
        Result:=#11;
      $2640:
        Result:=#12;
      $266A:
        Result:=#13;
      $266B:
        Result:=#14;
      $263C:
        Result:=#15;
      $25BA:
        Result:=#16;
      $25C4:
        Result:=#17;
      $2195:
        Result:=#18;
      $203C:
        Result:=#19;
      $00B6:
        Result:=#20;
      $00A7:
        Result:=#21;
      $25AC:
        Result:=#22;
      $21A8:
        Result:=#23;
      $2191:
        Result:=#24;
      $2193:
        Result:=#25;
      $2192:
        Result:=#26;
      $2190:
        Result:=#27;
      $221F:
        Result:=#28;
      $2194:
        Result:=#29;
      $25B2:
        Result:=#30;
      $25BC:
        Result:=#31;
      $2302:
        Result:=#127;
      else
        Result:='?';
    end
  else
    Result:='?';
end;

function LegacyChar2ExtendedGraphemeCluster(const Ch: Char): UnicodeString;
var
  tmpS: RawByteString;
begin
  SetLength(tmpS, 1);
  tmpS[1]:=Ch;
  System.SetCodePage(tmpS,CP_OEMCP,False);
  Result:=tmpS;
end;

procedure Enhanced2Legacy;
var
  I: Integer;
begin
  { todo: optimize this }
  for I := 0 to Length(EnhancedVideoBuf)-1 do
    begin
      with EnhancedVideoBuf[I] do
        VideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster));
      with OldEnhancedVideoBuf[I] do
        OldVideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster));
    end;
end;

procedure Legacy2Enhanced;
var
  I: Integer;
begin
  { todo: optimize this }
  for I := 0 to Length(EnhancedVideoBuf)-1 do
    begin
      with EnhancedVideoBuf[I] do
        begin
          Attribute:=Byte(VideoBuf^[I] shr 8);
          ExtendedGraphemeCluster:=LegacyChar2ExtendedGraphemeCluster(Chr(Byte(VideoBuf^[I])));
        end;
      with OldEnhancedVideoBuf[I] do
        begin
          Attribute:=Byte(OldVideoBuf^[I] shr 8);
          ExtendedGraphemeCluster:=LegacyChar2ExtendedGraphemeCluster(Chr(Byte(OldVideoBuf^[I])));
        end;
    end;
end;

Procedure UpdateScreen (Force : Boolean);

begin
  if (LockUpdateScreen<=0) and
     Assigned(CurrentVideoDriver.UpdateScreen) then
    begin
      if EnhancedVideoInitialized and Assigned(CurrentVideoDriver.InitDriver) then
        Enhanced2Legacy
      else if VideoInitialized and Assigned(CurrentVideoDriver.InitEnhancedDriver) then
        Legacy2Enhanced;
      CurrentVideoDriver.UpdateScreen(Force);
    end;
end;

procedure ClearScreen;
const
  DefaultChar=#32;
  // Should this not be the current color ?
  DefaultAttr=7;
var
  I: Integer;
begin
  if VideoInitialized then
    begin
      FillWord(VideoBuf^,VideoBufSize shr 1,(DefaultAttr shl 8) or Ord(DefaultChar));
      If Assigned(CurrentVideoDriver.ClearScreen) then
        begin
          if Assigned(CurrentVideoDriver.InitEnhancedDriver) then
            Legacy2Enhanced;
          CurrentVideoDriver.ClearScreen;
        end
      else
        UpdateScreen(True);
      FillWord(OldVideoBuf^,VideoBufSize shr 1,(DefaultAttr shl 8) or Ord(DefaultChar));
    end
  else if EnhancedVideoInitialized then
    begin
      { todo: optimize }
      for I:=0 to Length(EnhancedVideoBuf)-1 do
        with EnhancedVideoBuf[I] do
          begin
            Attribute:=DefaultAttr;
            ExtendedGraphemeCluster:=DefaultChar;
          end;
      If Assigned(CurrentVideoDriver.ClearScreen) then
        begin
          if Assigned(CurrentVideoDriver.InitDriver) then
            Enhanced2Legacy;
          CurrentVideoDriver.ClearScreen;
        end
      else
        UpdateScreen(True);
      { todo: optimize }
      for I:=0 to Length(EnhancedVideoBuf)-1 do
        with OldEnhancedVideoBuf[I] do
          begin
            Attribute:=DefaultAttr;
            ExtendedGraphemeCluster:=DefaultChar;
          end;
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

function ExtendedGraphemeClusterDisplayWidth(const EGC: UnicodeString): Integer;
var
  FirstCodePoint: UCS4Char;
begin
  if Length(EGC) > 0 then
    begin
      FirstCodePoint:=UCS4Char(EGC[1]);
      if (FirstCodePoint>=$D800) and (FirstCodePoint<=$DBFF) and (Length(EGC)>=2) and
         (Ord(EGC[2])>=$DC00) and (Ord(EGC[2])<=$DFFF) then
        begin
          FirstCodePoint := $10000+((FirstCodePoint-$D800) shl 10) or (Ord(EGC[2])-$DC00);
        end;
      { todo: handle emoji + modifiers }
      case GetEastAsianWidth(FirstCodePoint) of
        eawW, eawF:
          Result := 2;
        else
          Result := 1;
      end;
    end
  else
    Result := 0;
end;

function StringDisplayWidth(const S: UnicodeString): Integer;
var
  EGC: UnicodeString;
begin
  Result:=0;
  for EGC in TUnicodeStringExtendedGraphemeClustersEnumerator.Create(S) do
    Inc(Result, ExtendedGraphemeClusterDisplayWidth(EGC));
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
  If VideoInitialized or EnhancedVideoInitialized then
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


