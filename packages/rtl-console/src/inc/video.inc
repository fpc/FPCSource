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
  convert_lowascii_to_Unicode:array[#0..#31] of WideChar=(
    #8199,#9786,#9787,#9829,#9830,#9827,#9824,#8226,
    #9688,#9675,#9689,#9794,#9792,#9834,#9835,#9788,
    #9658,#9668,#8597,#8252,#0182,#0167,#9644,#8616,
    #8593,#8595,#8594,#8592,#8735,#8596,#9650,#9660
  );

{ TEnhancedVideoCell }

operator = (const a,b : TEnhancedVideoCell) res: Boolean;
begin
  res:=(a.FForegroundColor=b.FForegroundColor) and
       (a.FBackgroundColor=b.FBackgroundColor) and
       (a.EnhancedVideoAttributes=b.EnhancedVideoAttributes) and
       (a.ExtendedGraphemeCluster=b.ExtendedGraphemeCluster);
end;

function TEnhancedVideoCell.GetAttribute: Byte;
var
  EVA: TEnhancedVideoAttributes;
  Fg,Bg,Attr: Byte;
  tmpB: Byte;
begin
  EVA := EnhancedVideoAttributes;
  Fg := FForegroundColor and $0F;
  Bg := FBackgroundColor and $0F;
  if ScreenColor then
  begin
    { display underlined as cyan }
    if (EVA*[evaUnderlined,evaDoublyUnderlined])<>[] then
      Fg := 3;
    { display italicized as green }
    if evaItalicized in EVA then
      Fg := 2;
    if evaInverse in EVA then
    begin
      tmpB := Fg;
      Fg := Bg;
      Bg := tmpB;
    end;
    if evaFaint in EVA then
      Fg := 8;
    if evaBold in EVA then
      Fg := Fg or 8;
    if evaInvisible in EVA then
      Fg := Bg;
  end
  else
  begin
    if evaInverse in EVA then
    begin
      tmpB := Fg;
      Fg := Bg;
      Bg := tmpB;
    end;
    { foreground color 1 is underline on MDA }
    if (EVA*[evaUnderlined,evaDoublyUnderlined])<>[] then
      Fg := (Fg and 8) + 1;
    if evaBold in EVA then
      Fg := Fg or 8;
    if evaInvisible in EVA then
    begin
      { on MDA white on white background doesn't work as invisible, only black
        on black is possible }
      Fg := 0;
      Bg := 0;
    end;
  end;
  Attr := (Fg and $0F) or Byte((Bg and $07) shl 4);
  if (EVA*[evaBlinkSlow,evaBlinkFast])<>[] then
    Attr := Attr or $80;
  GetAttribute := Attr;
end;

procedure TEnhancedVideoCell.SetAttribute(Attr: Byte);
begin
  FForegroundColor := Attr and $0F;
  FBackgroundColor := (Attr shr 4) and $07;
  if (Attr and $80) <> 0 then
    EnhancedVideoAttributes := [evaBlinkSlow]
  else
    EnhancedVideoAttributes := [];
end;

function TEnhancedVideoCell.GetEnhancedVideoAttributes: TEnhancedVideoAttributes;
begin
  GetEnhancedVideoAttributes := TEnhancedVideoAttributes(Word(FInternalAttributes and $7FFF));
end;

procedure TEnhancedVideoCell.SetEnhancedVideoAttributes(AEnhancedVideoAttributes: TEnhancedVideoAttributes);
begin
  FInternalAttributes := (FInternalAttributes and $8000) or (Word(AEnhancedVideoAttributes) and $7FFF);
end;

function TEnhancedVideoCell.GetForegroundColor: Byte;
begin
  GetForegroundColor := FForegroundColor;
end;

procedure TEnhancedVideoCell.SetForegroundColor(AForegroundColor: Byte);
begin
  FForegroundColor := AForegroundColor;
end;

function TEnhancedVideoCell.GetBackgroundColor: Byte;
begin
  GetBackgroundColor := FBackgroundColor;
end;

procedure TEnhancedVideoCell.SetBackgroundColor(ABackgroundColor: Byte);
begin
  FBackgroundColor := ABackgroundColor;
end;

function TEnhancedVideoCell.GetExtendedGraphemeCluster: UnicodeString;
begin
  if (FInternalAttributes and $8000) = 0 then
    GetExtendedGraphemeCluster := EGC_SingleChar
  else
    GetExtendedGraphemeCluster := UnicodeString(EGC_WideStr);
end;

procedure TEnhancedVideoCell.SetExtendedGraphemeCluster(const AExtendedGraphemeCluster: UnicodeString);
begin
  if Length(AExtendedGraphemeCluster) = 1 then
  begin
    if (FInternalAttributes and $8000) <> 0 then
    begin
      FInternalAttributes := FInternalAttributes and $7FFF;
      UnicodeString(EGC_WideStr) := '';
    end;
    EGC_SingleChar := AExtendedGraphemeCluster[1];
  end
  else
  begin
    if (FInternalAttributes and $8000) = 0 then
    begin
      FInternalAttributes := FInternalAttributes or $8000;
      EGC_WideStr := nil;
    end;
    UnicodeString(EGC_WideStr) := AExtendedGraphemeCluster;
  end;
end;

class operator TEnhancedVideoCell.Initialize(var evc: TEnhancedVideoCell);
begin
  evc.FInternalAttributes := 0;
  evc.ForegroundColor := 0;
  evc.BackgroundColor := 0;
end;

class operator TEnhancedVideoCell.Finalize(var evc: TEnhancedVideoCell);
begin
  if (evc.FInternalAttributes and $8000) <> 0 then
    UnicodeString(evc.EGC_WideStr) := '';
end;

Procedure fpc_UnicodeStr_Incr_Ref(S : Pointer); external name 'FPC_UNICODESTR_INCR_REF';

class operator TEnhancedVideoCell.AddRef(var evc: TEnhancedVideoCell);
begin
  if (evc.FInternalAttributes and $8000) <> 0 then
    fpc_UnicodeStr_Incr_Ref(evc.EGC_WideStr);
end;

class operator TEnhancedVideoCell.Copy(constref aSrc: TEnhancedVideoCell; var aDst: TEnhancedVideoCell);
begin
  aDst.ExtendedGraphemeCluster := aSrc.ExtendedGraphemeCluster;
  aDst.EnhancedVideoAttributes := aSrc.EnhancedVideoAttributes;
  aDst.FForegroundColor := aSrc.FForegroundColor;
  aDst.FBackgroundColor := aSrc.FBackgroundColor;
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
  CurrentLegacy2EnhancedTranslationCodePage: TSystemCodePage;

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

procedure EnhancedVideoBufResize(var Buf: TEnhancedVideoBuf; OldCols, OldRows : Word);
var
  NewVideoBufSize : SizeUInt;
  NewBuf: TEnhancedVideoBuf;
  Y, X, YS, XS: Integer;
begin
  NewVideoBufSize:=ScreenWidth*ScreenHeight;
  if OldCols<>ScreenWidth then
    begin
      SetLength(NewBuf,NewVideoBufSize);
      if OldRows<ScreenHeight then
        YS := OldRows
      else
        YS := ScreenHeight;
      if OldCols<ScreenWidth then
        XS := OldCols
      else
        XS := ScreenWidth;
      for Y := 0 to YS-1 do
        for X := 0 to XS-1 do
          NewBuf[Y*ScreenWidth+X]:=Buf[Y*OldCols+X];
      Buf:=NewBuf;
    end
  else
    SetLength(Buf,NewVideoBufSize);
end;

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
      EnhancedVideoBufResize(EnhancedVideoBuf,OldCols,OldRows);
      EnhancedVideoBufResize(OldEnhancedVideoBuf,OldCols,OldRows);
    end;
end;

Procedure InitVideo;

begin
  if not VideoInitialized then
    begin
      if Assigned(CurrentVideoDriver.InitEnhancedDriver) then
        begin
          CurrentLegacy2EnhancedTranslationCodePage := 437;
          CurrentVideoDriver.InitEnhancedDriver;
        end
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

function ExtendedGraphemeCluster2LegacyChar(const EGC: UnicodeString; CodePage: TSystemCodePage): Char;

  function GenConvert: Char;
    var
      tmpS: RawByteString;
    begin
      tmpS:=System.UTF8Encode(EGC);
      System.SetCodePage(tmpS,CodePage,True);
      if Length(tmpS)=1 then
        Result:=tmpS[1]
      else
        Result:='?';
    end;

var
  Ch: Char;
begin
  if (Length(EGC) = 1) then
    begin
      for Ch:=Low(convert_lowascii_to_Unicode) to High(convert_lowascii_to_Unicode) do
        if convert_lowascii_to_Unicode[Ch]=EGC[1] then
          begin
            Result:=Ch;
            exit;
          end;
      case Ord(EGC[1]) of
        32..126:
          Result:=Chr(Ord(EGC[1]));
        $2302:
          Result:=#127;
        else
          Result:=GenConvert;
      end
    end
  else
    Result:=GenConvert;
end;

function LegacyChar2ExtendedGraphemeCluster(const Ch: Char): UnicodeString;
var
  tmpS: RawByteString;
begin
  if Ch<=#31 then
    Result:=convert_lowascii_to_Unicode[Ch]
  else if Ch=#127 then
    Result:=#$2302
  else
    begin
      SetLength(tmpS, 1);
      tmpS[1]:=Ch;
      System.SetCodePage(tmpS,CurrentLegacy2EnhancedTranslationCodePage,False);
      Result:=tmpS;
    end;
end;

procedure Enhanced2Legacy;
var
  I: Integer;
  CodePage: TSystemCodePage;
begin
  CodePage:=GetActiveCodePage();
  { todo: optimize this }
  for I := 0 to Length(EnhancedVideoBuf)-1 do
    begin
      with EnhancedVideoBuf[I] do
        VideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster,CodePage));
      with OldEnhancedVideoBuf[I] do
        OldVideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster,CodePage));
    end;
end;

procedure Enhanced2Legacy_Area(X1, Y1, X2, Y2: Word);
var
  I, IY, X, Y: Integer;
  CodePage: TSystemCodePage;
begin
  if (X1>=ScreenWidth) or (Y1>=ScreenHeight) then
    exit;
  if X2>=ScreenWidth then
    X2:=ScreenWidth-1;
  if Y2>=ScreenHeight then
    Y2:=ScreenHeight-1;
  if (X1>X2) or (Y1>Y2) then
    exit;
  CodePage:=GetActiveCodePage();
  { todo: optimize this }
  IY := Y1*ScreenWidth+X1;
  for Y := Y1 to Y2 do
    begin
      I := IY;
      for X := X1 to X2 do
        begin
          with EnhancedVideoBuf[I] do
            VideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster,CodePage));
          with OldEnhancedVideoBuf[I] do
            OldVideoBuf^[I]:=(Attribute shl 8) or Ord(ExtendedGraphemeCluster2LegacyChar(ExtendedGraphemeCluster,CodePage));
          Inc(I);
        end;
      Inc(IY, ScreenWidth);
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

procedure Legacy2Enhanced_Area(X1, Y1, X2, Y2: Word);
var
  I, IY, X, Y: Integer;
begin
  if (X1>=ScreenWidth) or (Y1>=ScreenHeight) then
    exit;
  if X2>=ScreenWidth then
    X2:=ScreenWidth-1;
  if Y2>=ScreenHeight then
    Y2:=ScreenHeight-1;
  if (X1>X2) or (Y1>Y2) then
    exit;
  { todo: optimize this }
  IY := Y1*ScreenWidth+X1;
  for Y := Y1 to Y2 do
    begin
      I := IY;
      for X := X1 to X2 do
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
          Inc(I);
        end;
      Inc(IY, ScreenWidth);
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

Procedure UpdateScreenArea (const X1, Y1, X2, Y2: Word; Force: Boolean);

begin
  if (LockUpdateScreen<=0) then
    if Assigned(CurrentVideoDriver.UpdateScreenArea) then
      begin
        if EnhancedVideoInitialized and Assigned(CurrentVideoDriver.InitDriver) then
          Enhanced2Legacy_Area(X1,Y1,X2,Y2)
        else if VideoInitialized and Assigned(CurrentVideoDriver.InitEnhancedDriver) then
          Legacy2Enhanced_Area(X1,Y1,X2,Y2);
        CurrentVideoDriver.UpdateScreenArea(X1,Y1,X2,Y2,Force);
      end
    else
      UpdateScreen(Force);
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

function GetActiveCodePage: TSystemCodePage;
begin
  if EnhancedVideoInitialized then
    Result := CurrentLegacy2EnhancedTranslationCodePage
  else if VideoInitialized and Assigned(CurrentVideoDriver.GetActiveCodePage) then
    Result := CurrentVideoDriver.GetActiveCodePage()
  else
    Result := DefaultSystemCodePage;
end;

{ disallowed codepages (variable length), code points larger than an 8-bit byte, etc. }
function IsDisallowedCodePage(CodePage: TSystemCodePage): Boolean;
const
  CP_UTF32LE=12000;
  CP_UTF32BE=12001;
begin
  Result:=(CodePage=CP_ACP) or (CodePage=CP_OEMCP) or (CodePage=CP_NONE) or
          (CodePage=CP_UTF8) or (CodePage=CP_UTF7) or
          (CodePage=CP_UTF16) or (CodePage=CP_UTF16BE) or
          (CodePage=CP_UTF32LE) or (CodePage=CP_UTF32BE);
end;

procedure ActivateCodePage(CodePage: TSystemCodePage);
begin
  if IsDisallowedCodePage(CodePage) then
    exit;
  if EnhancedVideoInitialized then
    CurrentLegacy2EnhancedTranslationCodePage := CodePage
  else if VideoInitialized and Assigned(CurrentVideoDriver.ActivateCodePage) then
    CurrentVideoDriver.ActivateCodePage(CodePage);
end;

var
  SupportedCodePagesCount: Integer = -1;
  SupportedCodePages: array of TSystemCodePage;

procedure InitSupportedCodePages;
var
  CP: TSystemCodePage;
begin
  SetLength(SupportedCodePages, 0);
  for CP:=Low(TSystemCodePage) to High(TSystemCodePage) do
    if (not IsDisallowedCodePage(CP)) and MappingAvailable(CP) then
      begin
        SetLength(SupportedCodePages,Length(SupportedCodePages)+1);
        SupportedCodePages[High(SupportedCodePages)]:=CP;
      end;
end;

function GetSupportedCodePageCount: Integer;
begin
  if EnhancedVideoInitialized then
    begin
      if SupportedCodePagesCount = -1 then
        InitSupportedCodePages;
      Result := SupportedCodePagesCount;
    end
  else if VideoInitialized and Assigned(CurrentVideoDriver.GetSupportedCodePageCount) then
    Result := CurrentVideoDriver.GetSupportedCodePageCount()
  else
    Result := 1;
end;

function GetSupportedCodePage(Index: Integer): TSystemCodePage;
begin
  if EnhancedVideoInitialized then
    begin
      if SupportedCodePagesCount = -1 then
        InitSupportedCodePages;
      Result := SupportedCodePages[Index];
    end
  else if VideoInitialized and Assigned(CurrentVideoDriver.GetSupportedCodePage) then
    Result := CurrentVideoDriver.GetSupportedCodePage(Index)
  else
    Result := DefaultSystemCodePage;
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


