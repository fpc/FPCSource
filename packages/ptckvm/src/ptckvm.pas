{
  An implementation of a console driver, integrated with the video, keyboard
  and mouse units, built on top of the PTCPas graphics library.

  Copyright (C) 2006, 2021, 2022, 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$IFNDEF FPC_DOTTEDUNITS}
unit ptckvm;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Console.Keyboard, System.Console.Video, System.Console.Mouse;
{$ELSE FPC_DOTTEDUNITS}
  Keyboard, Video, Mouse;
{$ENDIF FPC_DOTTEDUNITS}

procedure RegisterPtcKvmDrivers;
function CheckPendingResize(out NewMode: TVideoMode): Boolean;
procedure StartBlinkingCursor;
procedure StopBlinkingCursor;

var
  InitialWidth: Integer = 80;
  InitialHeight: Integer = 25;
  FontFileName: string = 'fptermfont.psfu';

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, ptc, PTC.EventQueue;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils, ptc, ptceventqueue;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSubsystem = (sKeyboard, sVideo, sMouse);
  TSubsystems = set of TSubsystem;

  { TBitmapFont }

  TBitmapFont = class
  strict private
    FGlyphCount: Integer;
    FWidth, FHeight: Integer;
    FData: array of Boolean;
    FCodePointToGlyph: array [UCS4Char] of Integer;

    procedure AddCodePointForGlyph(GlyphId: Integer; CodePoint: LongWord);
    procedure AddUTF8SequenceForGlyph(GlyphId: Integer; Seq: UTF8String);
    function GetPixel(GlyphId, Y, X: Integer): Boolean;
    procedure SetGlyphCount(AValue: Integer);
    procedure SetPixel(GlyphId, Y, X: Integer; AValue: Boolean);
    function DetectPSF1(var F: File): Boolean;
    function DetectPSF2(var F: File): Boolean;
    function DetectRAW(var F: File): Boolean;
    procedure CreateFromRAW(var F: File);
    procedure CreateFromPSF1(var F: File);
    procedure CreateFromPSF2(var F: File);
  public
    constructor Create(AWidth, AHeight, AGlyphCount: Integer);
    constructor CreateFromFile(const AFileName: string);

    function EGC2GlyphId(const EGC: UnicodeString): Integer;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount;
    property Pixel [GlyphId, Y, X: Integer]: Boolean read GetPixel write SetPixel;
  end;

var
  InitializedSubsystems: TSubsystems;

  CurrentFont: TBitmapFont;
  CurrentWidth, CurrentHeight: Integer; { characters }
  CurrentWidthPixels, CurrentHeightPixels: Integer; { pixels }

  CursorType: Word;
  CursorBlinkingEnabled: Boolean;
  CursorBlinkState: Boolean;  { true - visible; false - invisible }
  FastBlinkTextState: Boolean;  { true - visible; false - invisible }
  SlowBlinkTextState: Boolean;  { true - visible; false - invisible }
  MouseX, MouseY: Integer;
  MouseButtons: Integer;
  OldMouseX, OldMouseY: Integer;
  OldMouseButtons: Integer;
  ShiftState: Word;

  LastDrawnCursorType: Word;
  LastDrawnCursorBlinkState: Boolean;
  LastDrawnFastBlinkTextState: Boolean;
  LastDrawnSlowBlinkTextState: Boolean;
  LastDrawnCursorX: Word;
  LastDrawnCursorY: Word;

  LastFrameTime: Double;
  LastPolledEventBuffer: TEnhancedKeyEvent;
  PendingResize: IPTCResizeEvent;

  { ptc stuff }
  Format: IPTCFormat;
  Timer: IPTCTimer;
  CursorBlinkTimer: IPTCTimer;
  KeyPressEvents: TEventQueue;
  Palette: IPTCPalette;
  Surface: IPTCSurface;
  Console: IPTCConsole;

{ TBitmapFont }

procedure TBitmapFont.AddCodePointForGlyph(GlyphId: Integer; CodePoint: LongWord);
begin
  if CodePoint > High(UCS4Char) then
    raise EArgumentOutOfRangeException.Create('CodePoint not in valid range');
  if (CodePoint >= $D800) and (CodePoint <= $DFFF) then
    raise EArgumentOutOfRangeException.Create('CodePoint must not be in the reserved U+D800 to U+DFFF range');
  FCodePointToGlyph[CodePoint] := GlyphId;
end;

procedure TBitmapFont.AddUTF8SequenceForGlyph(GlyphId: Integer; Seq: UTF8String);
begin
  {todo...}
end;

function TBitmapFont.GetPixel(GlyphId, Y, X: Integer): Boolean;
begin
  Result := FData[(GlyphId * FHeight + Y) * FWidth + X];
end;

procedure TBitmapFont.SetGlyphCount(AValue: Integer);
begin
  if FGlyphCount = AValue then
    exit;
  FGlyphCount := AValue;
  SetLength(FData, FGlyphCount * FWidth * FHeight);
end;

procedure TBitmapFont.SetPixel(GlyphId, Y, X: Integer; AValue: Boolean);
begin
  FData[(GlyphId * FHeight + Y) * FWidth + X] := AValue;
end;

function TBitmapFont.DetectPSF1(var F: File): Boolean;
var
  HeaderMagic: array [0..1] of Byte;
begin
  Result := false;
  if FileSize(F) < 4 then
    exit;
  Seek(F, 0);
  BlockRead(F, HeaderMagic, SizeOf(HeaderMagic));
  Result := (HeaderMagic[0] = $36) and
            (HeaderMagic[1] = $04);
end;

function TBitmapFont.DetectPSF2(var F: File): Boolean;
var
  HeaderMagic: array [0..3] of Byte;
begin
  Result := false;
  if FileSize(F) < 32 then
    exit;
  Seek(F, 0);
  BlockRead(F, HeaderMagic, SizeOf(HeaderMagic));
  Result := (HeaderMagic[0] = $72) and
            (HeaderMagic[1] = $b5) and
            (HeaderMagic[2] = $4a) and
            (HeaderMagic[3] = $86);
end;

function TBitmapFont.DetectRAW(var F: File): Boolean;
begin
  case FileSize(F) of
    256*8,
    256*14,
    256*16:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TBitmapFont.CreateFromRAW(var F: File);
var
  I, Y, X: Integer;
  B: Byte;
begin
  case FileSize(F) of
    256*8:
      Create(8, 8, 256);
    256*14:
      Create(8, 14, 256);
    256*16:
      Create(8, 16, 256);
    else
      raise Exception.Create('Unknown font file format');
  end;
  for I := 0 to GlyphCount - 1 do
    for Y := 0 to Height - 1 do
    begin
      BlockRead(F, B, 1);
      for X := 0 to Width - 1 do
      begin
        Pixel[I, Y, X] := (B and $80) <> 0;
        B := B shl 1;
      end;
    end;
end;

procedure TBitmapFont.CreateFromPSF1(var F: File);
const
  PSF1_MODE512    = $01;
  PSF1_MODEHASTAB = $02;
  PSF1_MODEHASSEQ = $04;
  PSF1_MAXMODE    = $05;
type
  TPsf1Header = packed record
    magic:    array [0..1] of Byte;
    mode:     Byte;
    charsize: Byte;
  end;
var
  Header: TPsf1Header;
  I, Y, X: Integer;
  B: Byte;
  WC: WideChar;
  Seq: UnicodeString;
begin
  Seek(F, 0);
  BlockRead(F, Header, SizeOf(Header));
  if (Header.mode and PSF1_MODE512) <> 0 then
    Create(8, Header.charsize, 512)
  else
    Create(8, Header.charsize, 256);
  for I := 0 to GlyphCount - 1 do
  begin
    for Y := 0 to Height - 1 do
    begin
      BlockRead(F, B, 1);
      for X := 0 to Width - 1 do
      begin
        Pixel[I, Y, X] := (B and $80) <> 0;
        B := B shl 1;
      end;
    end;
  end;
  if (Header.mode and PSF1_MODEHASTAB) <> 0 then
  begin
    for I := 0 to GlyphCount - 1 do
    begin
      repeat
        BlockRead(F, WC, 2);
        if (WC <> WideChar($FFFF)) and (WC <> WideChar($FFFE)) then
          AddCodePointForGlyph(I, Ord(WC));
      until (WC = WideChar($FFFF)) or (WC = WideChar($FFFE));
      while WC = WideChar($FFFE) do
      begin
        Seq := '';
        repeat
          BlockRead(F, WC, 2);
          if (WC <> WideChar($FFFF)) and (WC <> WideChar($FFFE)) then
            Seq := Seq + WC;
        until (WC = WideChar($FFFF)) or (WC = WideChar($FFFE));
        AddUTF8SequenceForGlyph(I, UTF8Encode(Seq));
      end;
    end;
  end;
end;

procedure TBitmapFont.CreateFromPSF2(var F: File);
const
  PSF2_HAS_UNICODE_TABLE = 1;
type
  TPsf2Header = packed record
    magic:         array [0..3] of Byte;
    version:       UInt32;
    headersize:    UInt32;
    flags:         UInt32;
    length:        UInt32;
    charsize:      UInt32;
    height, width: UInt32;
  end;
var
  Header: TPsf2Header;
  I, BitsLeft, Y, X, J: Integer;
  B: Byte;
  CodePoint: UCS4Char;
  Seq: UTF8String;
begin
  Seek(F, 0);
  BlockRead(F, Header, SizeOf(Header));
  {$ifdef FPC_BIG_ENDIAN}
  with Header do
  begin
    version := SwapEndian(version);
    headersize := SwapEndian(headersize);
    flags := SwapEndian(flags);
    length := SwapEndian(length);
    charsize := SwapEndian(charsize);
    height := SwapEndian(height);
    width := SwapEndian(width);
  end;
  {$endif FPC_BIG_ENDIAN}
  Create(Header.width, Header.height, Header.length);
  for I := 0 to GlyphCount - 1 do
  begin
    BitsLeft := 0;
    Seek(F, Header.headersize + I * Header.charsize);
    for Y := 0 to Height - 1 do
    begin
      { each row starts on a byte boundary }
      if BitsLeft <> 0 then
      begin
        BitsLeft := 8;
        BlockRead(F, B, 1);
      end;
      for X := 0 to Width - 1 do
      begin
        if BitsLeft = 0 then
        begin
          BitsLeft := 8;
          BlockRead(F, B, 1);
        end;
        Pixel[I, Y, X] := (B and $80) <> 0;
        B := B shl 1;
        Dec(BitsLeft);
      end;
    end;
  end;
  if (Header.flags and PSF2_HAS_UNICODE_TABLE) <> 0 then
  begin
    Seek(F, Header.headersize + Header.length * Header.charsize);
    for I := 0 to GlyphCount - 1 do
    begin
      repeat
        BlockRead(F, B, 1);
        if (B <> $FF) and (B <> $FE) then
        begin
          case B of
            0..127:
              AddCodePointForGlyph(I, B);
            %11000000..%11011111:
              begin
                CodePoint := B and %11111;
                BlockRead(F, B, 1);
                if (B and %11000000) <> %10000000 then
                  raise Exception.Create('Error in file');
                CodePoint := (CodePoint shl 6) or (B and %111111);
                AddCodePointForGlyph(I, CodePoint);
              end;
            %11100000..%11101111:
              begin
                CodePoint := B and %1111;
                for J := 1 to 2 do
                begin
                  BlockRead(F, B, 1);
                  if (B and %11000000) <> %10000000 then
                    raise Exception.Create('Error in file');
                  CodePoint := (CodePoint shl 6) or (B and %111111);
                end;
                AddCodePointForGlyph(I, CodePoint);
              end;
            %11110000..%11110111:
              begin
                CodePoint := B and %111;
                for J := 1 to 3 do
                begin
                  BlockRead(F, B, 1);
                  if (B and %11000000) <> %10000000 then
                    raise Exception.Create('Error in file');
                  CodePoint := (CodePoint shl 6) or (B and %111111);
                end;
                AddCodePointForGlyph(I, CodePoint);
              end;
          end;
        end;
      until (B = $FF) or (B = $FE);
      while B = $FE do
      begin
        Seq := '';
        repeat
          BlockRead(F, B, 1);
          if (B <> $FF) and (B <> $FE) then
            Seq := Seq + Chr(B);
        until (B = $FF) or (B = $FE);
        AddUTF8SequenceForGlyph(I, Seq);
      end;
    end;
  end;
end;

constructor TBitmapFont.Create(AWidth, AHeight, AGlyphCount: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FGlyphCount := 0;
  GlyphCount := AGlyphCount;
end;

constructor TBitmapFont.CreateFromFile(const AFileName: string);
var
  F: File;
begin
  AssignFile(F, AFileName);
  Reset(F, 1);
  try
    if DetectPSF2(F) then
      CreateFromPSF2(F)
    else if DetectPSF1(F) then
      CreateFromPSF1(F)
    else if DetectRAW(F) then
      CreateFromRAW(F)
    else
      raise Exception.Create('Unknown font file format');
  finally
    CloseFile(F);
  end;
end;

const
  convert_lowascii_to_Unicode:array[#0..#31] of WideChar=(
    #8199,#9786,#9787,#9829,#9830,#9827,#9824,#8226,
    #9688,#9675,#9689,#9794,#9792,#9834,#9835,#9788,
    #9658,#9668,#8597,#8252,#0182,#0167,#9644,#8616,
    #8593,#8595,#8594,#8592,#8735,#8596,#9650,#9660
  );

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

function TBitmapFont.EGC2GlyphId(const EGC: UnicodeString): Integer;
begin
  Result := -1;
  if (Length(EGC) = 1) and ((Ord(EGC[1]) < $D800) or (Ord(EGC[1]) > $DFFF)) then
    Result := FCodePointToGlyph[Ord(EGC[1])]
  else if (Length(EGC) = 2) and ((Ord(EGC[1]) >= $D800) and (Ord(EGC[1]) <= $DBFF)) and
                                ((Ord(EGC[2]) >= $DC00) and (Ord(EGC[2]) <= $DFFF)) then
    Result := FCodePointToGlyph[$10000 + (((Ord(EGC[1]) - $D800) shl 10) or (Ord(EGC[2]) - $DC00))];
  if Result = -1 then
    Result := Ord(ExtendedGraphemeCluster2LegacyChar(EGC,437));
end;

procedure SetMode(Width, Height: Integer);
var
  IsResize: Boolean;
begin
  { resize or reopen? }
  IsResize := (PendingResize <> nil) and (Width = (PendingResize.Width div CurrentFont.Width)) and (Height = (PendingResize.Height div CurrentFont.Height));

  if not IsResize then
    Console.Close;
  Surface := nil;

  CurrentWidth := Width;
  CurrentHeight := Height;
  if IsResize then
  begin
    CurrentWidthPixels := PendingResize.Width;
    CurrentHeightPixels := PendingResize.Height;
  end
  else
  begin
    CurrentWidthPixels := CurrentWidth * CurrentFont.Width;
    CurrentHeightPixels := CurrentHeight * CurrentFont.Height;
  end;

  Surface := TPTCSurfaceFactory.CreateNew(CurrentWidthPixels, CurrentHeightPixels, Format);
  if IsResize then
  begin
    Console.InternalResize(CurrentWidthPixels, CurrentHeightPixels);
    PendingResize := nil;
  end
  else
    Console.Open('ptc kvm', CurrentWidthPixels, CurrentHeightPixels, Format);

  Surface.Palette(Palette);
  Console.Palette(Palette);

  CursorBlinkTimer.Stop;
  CursorBlinkTimer.SetTime(0);
  CursorBlinkTimer.Start;

  Timer.Stop;
  Timer.SetTime(0);
  LastFrameTime := Timer.Time;
  Timer.Start;
end;

procedure UpdateBlinkState;
const
  BlinkPeriod = 16/70;
begin
  if CursorBlinkTimer.Time >= BlinkPeriod then
  begin
    FastBlinkTextState := not FastBlinkTextState;
    if not FastBlinkTextState then
      SlowBlinkTextState := not SlowBlinkTextState;
    CursorBlinkTimer.SetTime(0);
  end;
  if CursorBlinkingEnabled then
    CursorBlinkState := FastBlinkTextState
  else
    CursorBlinkState := True;
end;

procedure KVMInit;
const
  PaletteData: array [0..255] of Uint32 =
  ($000000, $0000AA, $00AA00, $00AAAA, $AA0000, $AA00AA, $AA5500, $AAAAAA,
   $555555, $5555FF, $55FF55, $55FFFF, $FF5555, $FF55FF, $FFFF55, $FFFFFF,

   { 6x6x6 colour cube }
   $000000, $00005f, $000087, $0000af, $0000d7, $0000ff,
   $005f00, $005f5f, $005f87, $005faf, $005fd7, $005fff,
   $008700, $00875f, $008787, $0087af, $0087d7, $0087ff,
   $00af00, $00af5f, $00af87, $00afaf, $00afd7, $00afff,
   $00d700, $00d75f, $00d787, $00d7af, $00d7d7, $00d7ff,
   $00ff00, $00ff5f, $00ff87, $00ffaf, $00ffd7, $00ffff,

   $5f0000, $5f005f, $5f0087, $5f00af, $5f00d7, $5f00ff,
   $5f5f00, $5f5f5f, $5f5f87, $5f5faf, $5f5fd7, $5f5fff,
   $5f8700, $5f875f, $5f8787, $5f87af, $5f87d7, $5f87ff,
   $5faf00, $5faf5f, $5faf87, $5fafaf, $5fafd7, $5fafff,
   $5fd700, $5fd75f, $5fd787, $5fd7af, $5fd7d7, $5fd7ff,
   $5fff00, $5fff5f, $5fff87, $5fffaf, $5fffd7, $5fffff,

   $870000, $87005f, $870087, $8700af, $8700d7, $8700ff,
   $875f00, $875f5f, $875f87, $875faf, $875fd7, $875fff,
   $878700, $87875f, $878787, $8787af, $8787d7, $8787ff,
   $87af00, $87af5f, $87af87, $87afaf, $87afd7, $87afff,
   $87d700, $87d75f, $87d787, $87d7af, $87d7d7, $87d7ff,
   $87ff00, $87ff5f, $87ff87, $87ffaf, $87ffd7, $87ffff,

   $af0000, $af005f, $af0087, $af00af, $af00d7, $af00ff,
   $af5f00, $af5f5f, $af5f87, $af5faf, $af5fd7, $af5fff,
   $af8700, $af875f, $af8787, $af87af, $af87d7, $af87ff,
   $afaf00, $afaf5f, $afaf87, $afafaf, $afafd7, $afafff,
   $afd700, $afd75f, $afd787, $afd7af, $afd7d7, $afd7ff,
   $afff00, $afff5f, $afff87, $afffaf, $afffd7, $afffff,

   $d70000, $d7005f, $d70087, $d700af, $d700d7, $d700ff,
   $d75f00, $d75f5f, $d75f87, $d75faf, $d75fd7, $d75fff,
   $d78700, $d7875f, $d78787, $d787af, $d787d7, $d787ff,
   $d7af00, $d7af5f, $d7af87, $d7afaf, $d7afd7, $d7afff,
   $d7d700, $d7d75f, $d7d787, $d7d7af, $d7d7d7, $d7d7ff,
   $d7ff00, $d7ff5f, $d7ff87, $d7ffaf, $d7ffd7, $d7ffff,

   $ff0000, $ff005f, $ff0087, $ff00af, $ff00d7, $ff00ff,
   $ff5f00, $ff5f5f, $ff5f87, $ff5faf, $ff5fd7, $ff5fff,
   $ff8700, $ff875f, $ff8787, $ff87af, $ff87d7, $ff87ff,
   $ffaf00, $ffaf5f, $ffaf87, $ffafaf, $ffafd7, $ffafff,
   $ffd700, $ffd75f, $ffd787, $ffd7af, $ffd7d7, $ffd7ff,
   $ffff00, $ffff5f, $ffff87, $ffffaf, $ffffd7, $ffffff,

   { grayscale }
   $080808, $121212, $1c1c1c, $262626, $303030, $3a3a3a, $444444, $4e4e4e,
   $585858, $626262, $6c6c6c, $767676, $808080, $8a8a8a, $949494, $9e9e9e,
   $a8a8a8, $b2b2b2, $bcbcbc, $c6c6c6, $d0d0d0, $dadada, $e4e4e4, $eeeeee);
begin
  MouseX := 0;
  MouseY := 0;
  MouseButtons := 0;
  OldMouseX := 0;
  OldMouseY := 0;
  OldMouseButtons := 0;
  ShiftState := 0;
  try
    FreeAndNil(KeyPressEvents);
    FreeAndNil(CurrentFont);
    CurrentFont := TBitmapFont.CreateFromFile(FontFileName);

    Format := TPTCFormatFactory.CreateNew(8);
    Timer := TPTCTimerFactory.CreateNew;
    CursorBlinkTimer := TPTCTimerFactory.CreateNew;
    Palette := TPTCPaletteFactory.CreateNew;
    Console := TPTCConsoleFactory.CreateNew;
    Console.Option('windowed output');
    Console.Option('resizable window');
    //Console.Option('intercept window close');

    Palette.Load(PaletteData);

    CursorBlinkingEnabled := True;
    CursorBlinkState := True;
    FastBlinkTextState := True;
    SlowBlinkTextState := True;

    KeyPressEvents := TEventQueue.Create;

    SetMode(InitialWidth, InitialHeight);
  except
    on error : TPTCError do
      error.report;
  end;
end;

procedure KVMDone;
begin
  Surface := nil;
  Console := nil;
  Palette := nil;
  Timer := nil;
  CursorBlinkTimer := nil;
  Format := nil;
  PendingResize := nil;
  FreeAndNil(KeyPressEvents);
  CurrentWidth := 0;
  CurrentHeight := 0;
  CurrentWidthPixels := 0;
  CurrentHeightPixels := 0;
  FreeAndNil(CurrentFont);
end;

procedure KVMUpdateScreen;
var
  ptr, p2, p3: Puint8;
  X, Y, CX, CY: Integer;
  IsCursor: Boolean;
  CursorHeight: Integer;
  EGC: UnicodeString;
  EVA: TEnhancedVideoAttributes;
  GlyphId: Integer;
  Attr: Byte;
  foreground, background: Byte;
begin
  LastDrawnCursorType := CursorType;
  LastDrawnCursorX := CursorX;
  LastDrawnCursorY := CursorY;
  LastDrawnCursorBlinkState := CursorBlinkState;
  LastDrawnFastBlinkTextState := FastBlinkTextState;
  LastDrawnSlowBlinkTextState := SlowBlinkTextState;
  case CursorType of
    crUnderLine:
      CursorHeight := CurrentFont.Height div 4;
    crBlock:
      CursorHeight := CurrentFont.Height;
    crHalfBlock:
      CursorHeight := CurrentFont.Height div 2;
  end;
  try
    ptr := Surface.Lock;
    try
      for Y := 0 to CurrentHeight - 1 do
        for X := 0 to CurrentWidth - 1 do
        begin
          IsCursor := CursorBlinkState and (X = CursorX) and (Y = CursorY) and (CursorType <> 0);

          EGC := EnhancedVideoBuf[Y * ScreenWidth + X].ExtendedGraphemeCluster;
          EVA := EnhancedVideoBuf[Y * ScreenWidth + X].EnhancedVideoAttributes;
          if evaInverse in EVA then
          begin
            foreground := EnhancedVideoBuf[Y * ScreenWidth + X].BackgroundColor;
            background := EnhancedVideoBuf[Y * ScreenWidth + X].ForegroundColor;
          end
          else
          begin
            foreground := EnhancedVideoBuf[Y * ScreenWidth + X].ForegroundColor;
            background := EnhancedVideoBuf[Y * ScreenWidth + X].BackgroundColor;
          end;
          { todo: use a bold font? }
          if (evaBold in EVA) and (foreground <= 15) then
            foreground := foreground or 8;
          if ((evaBlinkSlow in EVA) and not SlowBlinkTextState) or
             ((evaBlinkFast in EVA) and not FastBlinkTextState) or
             (evaInvisible in EVA) then
            foreground := background;
          GlyphId := CurrentFont.EGC2GlyphId(EGC);

          p2 := ptr + X * CurrentFont.Width + Y * CurrentFont.Height * Surface.Pitch;
          for CY := 0 to CurrentFont.Height - 1 do
          begin
            p3 := p2;

            if ((not IsCursor) or (CY < (CurrentFont.Height - CursorHeight))) and
               ((not (evaUnderlined in EVA)) or (CY <> CurrentFont.Height - 1)) and
               ((not (evaDoublyUnderlined in EVA)) or ((CY <> CurrentFont.Height - 1) and (CY <> CurrentFont.Height - 3))) and
               ((not (evaCrossedOut in EVA)) or (CY <> (CurrentFont.Height div 2))) then
            begin
              for CX := 0 to CurrentFont.Width - 1 do
              begin
                if CurrentFont.Pixel[GlyphId, CY, CX] then
                  p3^ := foreground
                else
                  p3^ := background;
                Inc(p3);
              end;
            end
            else
            begin
              for CX := 0 to CurrentFont.Width - 1 do
              begin
                p3^ := foreground;
                Inc(p3);
              end;
            end;

            Inc(p2, Surface.Pitch);
          end;
        end;
    finally
      Surface.Unlock;
    end;
    Surface.Copy(Console);
    Console.Update;
  except
    on Error: TPTCError do
      Error.Report;
  end;
end;

procedure KVMHandleKeyEvent(KeyEvent: IPTCKeyEvent);
begin
  if KeyEvent.Press then
    KeyPressEvents.AddEvent(KeyEvent);
  { update shift state }
  case KeyEvent.Code of
    PTCKEY_SHIFT:
      if pmkRightKey in KeyEvent.ModifierKeys then
      begin
        if KeyEvent.Press then
          ShiftState := ShiftState or 1
        else
          ShiftState := ShiftState and not 1;
      end
      else
      begin
        if KeyEvent.Press then
          ShiftState := ShiftState or 2
        else
          ShiftState := ShiftState and not 2;
      end;
    PTCKEY_CONTROL:
      begin
        if pmkRightKey in KeyEvent.ModifierKeys then
        begin
          if KeyEvent.Press then
            ShiftState := ShiftState or $400
          else
            ShiftState := ShiftState and not $400;
        end
        else
        begin
          if KeyEvent.Press then
            ShiftState := ShiftState or $100
          else
            ShiftState := ShiftState and not $100;
        end;
        ShiftState := (ShiftState and not 4) or ((ShiftState and $400) shr 8) or ((ShiftState and $100) shr 6);
      end;
    PTCKEY_ALT:
      begin
        if pmkRightKey in KeyEvent.ModifierKeys then
        begin
          if KeyEvent.Press then
            ShiftState := ShiftState or $800
          else
            ShiftState := ShiftState and not $800;
        end
        else
        begin
          if KeyEvent.Press then
            ShiftState := ShiftState or $200
          else
            ShiftState := ShiftState and not $200;
        end;
        ShiftState := (ShiftState and not 8) or ((ShiftState and $800) shr 8) or ((ShiftState and $200) shr 6);
      end;
  end;
end;

procedure KVMHandleMouseEvent(MouseEvent: IPTCMouseEvent);
var
  B: TPTCMouseButton;
  ME: TMouseEvent;
begin
  if not (sMouse in InitializedSubsystems) then
    exit;

  MouseX := MouseEvent.X div CurrentFont.Width;
  MouseY := MouseEvent.Y div CurrentFont.Height;
  MouseButtons := 0;
  for B := PTCMouseButton1 to PTCMouseButton16 do
    if B in MouseEvent.ButtonState then
      MouseButtons := MouseButtons or (1 shl (Ord(B) - Ord(PTCMouseButton1)));

  if (MouseX <> OldMouseX) or (MouseY <> OldMouseY) or (MouseButtons <> OldMouseButtons) then
  begin
    FillChar(ME, SizeOf(ME), 0);
    ME.X := MouseX;
    ME.Y := MouseY;
    ME.Buttons := MouseButtons;

    if (MouseX <> OldMouseX) or (MouseY <> OldMouseY) then
      ME.Action := MouseActionMove;
    if MouseButtons <> OldMouseButtons then
      if (MouseButtons and OldMouseButtons) <> OldMouseButtons then
        ME.Action := MouseActionUp
      else
        ME.Action := MouseActionDown;

    PutMouseEvent(ME);
  end
end;

procedure KVMHandleEvents;
var
  Event: IPTCEvent;
begin
  while Console.NextEvent(Event, False, PTCAnyEvent) do
    if Event <> nil then
    begin
      if Event is IPTCResizeEvent then
        PendingResize := Event as IPTCResizeEvent
      else if Event is IPTCKeyEvent then
        KVMHandleKeyEvent(Event as IPTCKeyEvent)
      else if Event is IPTCMouseEvent then
        KVMHandleMouseEvent(Event as IPTCMouseEvent);
    end;
end;

function KVMGetKeyEvent: IPTCKeyEvent;
var
  Event: IPTCEvent;
begin
  repeat
    KVMHandleEvents;
    Event := KeyPressEvents.NextEvent([PTCKeyEvent]);
  until Event <> nil;
  Result := Event as IPTCKeyEvent;
end;

function KVMPollKeyEvent: IPTCKeyEvent;
var
  Event: IPTCEvent;
begin
  KVMHandleEvents;
  Event := KeyPressEvents.NextEvent([PTCKeyEvent]);

  if Event <> nil then
    Result := Event as IPTCKeyEvent
  else
    Result := nil;
end;

function ConvertPTCKeyEventToTEnhancedKeyEvent(ev: IPTCKeyEvent): TEnhancedKeyEvent;
var
  ShiftState: TEnhancedShiftState;
begin
  if (ev = nil) or (ev.Code = -1) then
  begin
    Result := NilEnhancedKeyEvent;
    exit;
  end;

  ShiftState := [];
  if ev.Shift then
    Include(ShiftState, essShift);
  if ev.Control then
    Include(ShiftState, essCtrl);
  if ev.Alt then
    Include(ShiftState, essAlt);

  Result.AsciiChar := #0;
  Result.UnicodeChar := WideChar(0);

  if ev.alt then
    case ev.Code of
      PTCKEY_ESCAPE:
        Result.VirtualScanCode := $0100;
      PTCKEY_F1:
        begin
          Result.VirtualScanCode := $6800;
          Result.VirtualKeyCode := kbdF1;
        end;
      PTCKEY_F2:
        begin
          Result.VirtualScanCode := $6900;
          Result.VirtualKeyCode := kbdF2;
        end;
      PTCKEY_F3:
        begin
          Result.VirtualScanCode := $6A00;
          Result.VirtualKeyCode := kbdF3;
        end;
      PTCKEY_F4:
        begin
          Result.VirtualScanCode := $6B00;
          Result.VirtualKeyCode := kbdF4;
        end;
      PTCKEY_F5:
        begin
          Result.VirtualScanCode := $6C00;
          Result.VirtualKeyCode := kbdF5;
        end;
      PTCKEY_F6:
        begin
          Result.VirtualScanCode := $6D00;
          Result.VirtualKeyCode := kbdF6;
        end;
      PTCKEY_F7:
        begin
          Result.VirtualScanCode := $6E00;
          Result.VirtualKeyCode := kbdF7;
        end;
      PTCKEY_F8:
        begin
          Result.VirtualScanCode := $6F00;
          Result.VirtualKeyCode := kbdF8;
        end;
      PTCKEY_F9:
        begin
          Result.VirtualScanCode := $7000;
          Result.VirtualKeyCode := kbdF9;
        end;
      PTCKEY_F10:
        begin
          Result.VirtualScanCode := $7100;
          Result.VirtualKeyCode := kbdF10;
        end;
      PTCKEY_F11:
        begin
          Result.VirtualScanCode := $8B00;
          Result.VirtualKeyCode := kbdF11;
        end;
      PTCKEY_F12:
        begin
          Result.VirtualScanCode := $8C00;
          Result.VirtualKeyCode := kbdF12;
        end;
      PTCKEY_ENTER:
        begin
          if pmkNumPadKey in ev.ModifierKeys then
            Result.VirtualScanCode := $A600
          else
            Result.VirtualScanCode := $1C00;
        end;
      PTCKEY_BACKSPACE:
        Result.VirtualScanCode := $0E00;
      PTCKEY_TAB:
        Result.VirtualScanCode := $A500;
      PTCKEY_UP:
        Result.VirtualScanCode := $9800;
      PTCKEY_LEFT:
        Result.VirtualScanCode := $9B00;
      PTCKEY_DOWN:
        Result.VirtualScanCode := $A000;
      PTCKEY_RIGHT:
        Result.VirtualScanCode := $9D00;
      PTCKEY_INSERT:
        Result.VirtualScanCode := $A200;
      PTCKEY_HOME:
        Result.VirtualScanCode := $9700;
      PTCKEY_PAGEUP:
        Result.VirtualScanCode := $9900;
      PTCKEY_DELETE:
        Result.VirtualScanCode := $A300;
      PTCKEY_END:
        Result.VirtualScanCode := $9F00;
      PTCKEY_PAGEDOWN:
        Result.VirtualScanCode := $A100;
      PTCKEY_BACKQUOTE:
        Result.VirtualScanCode := $2900;
      PTCKEY_ONE:
        Result.VirtualScanCode := $7800;
      PTCKEY_TWO:
        Result.VirtualScanCode := $7900;
      PTCKEY_THREE:
        Result.VirtualScanCode := $7A00;
      PTCKEY_FOUR:
        Result.VirtualScanCode := $7B00;
      PTCKEY_FIVE:
        Result.VirtualScanCode := $7C00;
      PTCKEY_SIX:
        Result.VirtualScanCode := $7D00;
      PTCKEY_SEVEN:
        Result.VirtualScanCode := $7E00;
      PTCKEY_EIGHT:
        Result.VirtualScanCode := $7F00;
      PTCKEY_NINE:
        Result.VirtualScanCode := $8000;
      PTCKEY_ZERO:
        Result.VirtualScanCode := $8100;
      PTCKEY_MINUS:
        Result.VirtualScanCode := $8200;
      PTCKEY_EQUALS:
        Result.VirtualScanCode := $8300;
      PTCKEY_Q:
        Result.VirtualScanCode := $1000;
      PTCKEY_W:
        Result.VirtualScanCode := $1100;
      PTCKEY_E:
        Result.VirtualScanCode := $1200;
      PTCKEY_R:
        Result.VirtualScanCode := $1300;
      PTCKEY_T:
        Result.VirtualScanCode := $1400;
      PTCKEY_Y:
        Result.VirtualScanCode := $1500;
      PTCKEY_U:
        Result.VirtualScanCode := $1600;
      PTCKEY_I:
        Result.VirtualScanCode := $1700;
      PTCKEY_O:
        Result.VirtualScanCode := $1800;
      PTCKEY_P:
        Result.VirtualScanCode := $1900;
      PTCKEY_OPENBRACKET:
        Result.VirtualScanCode := $1A00;
      PTCKEY_CLOSEBRACKET:
        Result.VirtualScanCode := $1B00;
      PTCKEY_BACKSLASH:
        Result.VirtualScanCode := $2B00;
      PTCKEY_A:
        Result.VirtualScanCode := $1E00;
      PTCKEY_S:
        Result.VirtualScanCode := $1F00;
      PTCKEY_D:
        Result.VirtualScanCode := $2000;
      PTCKEY_F:
        Result.VirtualScanCode := $2100;
      PTCKEY_G:
        Result.VirtualScanCode := $2200;
      PTCKEY_H:
        Result.VirtualScanCode := $2300;
      PTCKEY_J:
        Result.VirtualScanCode := $2400;
      PTCKEY_K:
        Result.VirtualScanCode := $2500;
      PTCKEY_L:
        Result.VirtualScanCode := $2600;
      PTCKEY_SEMICOLON:
        Result.VirtualScanCode := $2700;
      PTCKEY_QUOTE:
        Result.VirtualScanCode := $2800;
      PTCKEY_Z:
        Result.VirtualScanCode := $2C00;
      PTCKEY_X:
        Result.VirtualScanCode := $2D00;
      PTCKEY_C:
        Result.VirtualScanCode := $2E00;
      PTCKEY_V:
        Result.VirtualScanCode := $2F00;
      PTCKEY_B:
        Result.VirtualScanCode := $3000;
      PTCKEY_N:
        Result.VirtualScanCode := $3100;
      PTCKEY_M:
        Result.VirtualScanCode := $3200;
      PTCKEY_COMMA:
        Result.VirtualScanCode := $3300;
      PTCKEY_PERIOD:
        Result.VirtualScanCode := $3400;
      PTCKEY_SLASH:
        Result.VirtualScanCode := $3500;
      PTCKEY_SPACE:
        begin
          Result.AsciiChar := #32;
          Result.UnicodeChar := WideChar(32);
          Result.VirtualScanCode := $3920;
        end;
    end
  else if ev.Control then
    case ev.Code of
      PTCKEY_ESCAPE:
        begin
          Result.AsciiChar := #27;
          Result.UnicodeChar := WideChar(27);
          Result.VirtualScanCode := $011B;
        end;
      PTCKEY_F1:
        begin
          Result.VirtualScanCode := $5E00;
          Result.VirtualKeyCode := kbdF1;
        end;
      PTCKEY_F2:
        begin
          Result.VirtualScanCode := $5F00;
          Result.VirtualKeyCode := kbdF2;
        end;
      PTCKEY_F3:
        begin
          Result.VirtualScanCode := $6000;
          Result.VirtualKeyCode := kbdF3;
        end;
      PTCKEY_F4:
        begin
          Result.VirtualScanCode := $6100;
          Result.VirtualKeyCode := kbdF4;
        end;
      PTCKEY_F5:
        begin
          Result.VirtualScanCode := $6200;
          Result.VirtualKeyCode := kbdF5;
        end;
      PTCKEY_F6:
        begin
          Result.VirtualScanCode := $6300;
          Result.VirtualKeyCode := kbdF6;
        end;
      PTCKEY_F7:
        begin
          Result.VirtualScanCode := $6400;
          Result.VirtualKeyCode := kbdF7;
        end;
      PTCKEY_F8:
        begin
          Result.VirtualScanCode := $6500;
          Result.VirtualKeyCode := kbdF8;
        end;
      PTCKEY_F9:
        begin
          Result.VirtualScanCode := $6600;
          Result.VirtualKeyCode := kbdF9;
        end;
      PTCKEY_F10:
        begin
          Result.VirtualScanCode := $6700;
          Result.VirtualKeyCode := kbdF10;
        end;
      PTCKEY_F11:
        begin
          Result.VirtualScanCode := $8900;
          Result.VirtualKeyCode := kbdF11;
        end;
      PTCKEY_F12:
        begin
          Result.VirtualScanCode := $8A00;
          Result.VirtualKeyCode := kbdF12;
        end;
      PTCKEY_ENTER:
        begin
          Result.AsciiChar := #10;
          Result.UnicodeChar := WideChar(10);
          if pmkNumPadKey in ev.ModifierKeys then
            Result.VirtualScanCode := $E00A
          else
            Result.VirtualScanCode := $1C0A;
        end;
      PTCKEY_BACKSPACE:
        begin
          Result.AsciiChar := #127;
          Result.UnicodeChar := WideChar(127);
          Result.VirtualScanCode := $0E7F;
        end;
      PTCKEY_TAB:
        Result.VirtualScanCode := $9400;
      PTCKEY_UP:
        Result.VirtualScanCode := $8D00;
      PTCKEY_LEFT:
        Result.VirtualScanCode := $7300;
      PTCKEY_DOWN:
        Result.VirtualScanCode := $9100;
      PTCKEY_RIGHT:
        Result.VirtualScanCode := $7400;
      PTCKEY_INSERT:
        Result.VirtualScanCode := $9200;
      PTCKEY_HOME:
        Result.VirtualScanCode := $7700;
      PTCKEY_PAGEUP:
        Result.VirtualScanCode := $8400;
      PTCKEY_DELETE:
        Result.VirtualScanCode := $9300;
      PTCKEY_END:
        Result.VirtualScanCode := $7500;
      PTCKEY_PAGEDOWN:
        Result.VirtualScanCode := $7600;
      PTCKEY_TWO:
        Result.VirtualScanCode := $0300;
      PTCKEY_SIX:
        begin
          Result.AsciiChar := #30;
          Result.UnicodeChar := WideChar(30);
          Result.VirtualScanCode := $071E;
        end;
      PTCKEY_MINUS:
        begin
          Result.AsciiChar := #31;
          Result.UnicodeChar := WideChar(31);
          Result.VirtualScanCode := $0C1F;
        end;
      PTCKEY_Q:
        begin
          Result.AsciiChar := #17;
          Result.UnicodeChar := WideChar(17);
          Result.VirtualScanCode := $1011;
        end;
      PTCKEY_W:
        begin
          Result.AsciiChar := #23;
          Result.UnicodeChar := WideChar(23);
          Result.VirtualScanCode := $1117;
        end;
      PTCKEY_E:
        begin
          Result.AsciiChar := #5;
          Result.UnicodeChar := WideChar(5);
          Result.VirtualScanCode := $1205;
        end;
      PTCKEY_R:
        begin
          Result.AsciiChar := #18;
          Result.UnicodeChar := WideChar(18);
          Result.VirtualScanCode := $1312;
        end;
      PTCKEY_T:
        begin
          Result.AsciiChar := #20;
          Result.UnicodeChar := WideChar(20);
          Result.VirtualScanCode := $1414;
        end;
      PTCKEY_Y:
        begin
          Result.AsciiChar := #25;
          Result.UnicodeChar := WideChar(25);
          Result.VirtualScanCode := $1519;
        end;
      PTCKEY_U:
        begin
          Result.AsciiChar := #21;
          Result.UnicodeChar := WideChar(21);
          Result.VirtualScanCode := $1615;
        end;
      PTCKEY_I:
        begin
          Result.AsciiChar := #9;
          Result.UnicodeChar := WideChar(9);
          Result.VirtualScanCode := $1709;
        end;
      PTCKEY_O:
        begin
          Result.AsciiChar := #15;
          Result.UnicodeChar := WideChar(15);
          Result.VirtualScanCode := $180F;
        end;
      PTCKEY_P:
        begin
          Result.AsciiChar := #16;
          Result.UnicodeChar := WideChar(16);
          Result.VirtualScanCode := $1910;
        end;
      PTCKEY_OPENBRACKET:
        begin
          Result.AsciiChar := #27;
          Result.UnicodeChar := WideChar(27);
          Result.VirtualScanCode := $1A1B;
        end;
      PTCKEY_CLOSEBRACKET:
        begin
          Result.AsciiChar := #29;
          Result.UnicodeChar := WideChar(29);
          Result.VirtualScanCode := $1B1D;
        end;
      PTCKEY_BACKSLASH:
        begin
          Result.AsciiChar := #28;
          Result.UnicodeChar := WideChar(28);
          Result.VirtualScanCode := $2B1C;
        end;
      PTCKEY_A:
        begin
          Result.AsciiChar := #1;
          Result.UnicodeChar := WideChar(1);
          Result.VirtualScanCode := $1E01;
        end;
      PTCKEY_S:
        begin
          Result.AsciiChar := #19;
          Result.UnicodeChar := WideChar(19);
          Result.VirtualScanCode := $1F13;
        end;
      PTCKEY_D:
        begin
          Result.AsciiChar := #4;
          Result.UnicodeChar := WideChar(4);
          Result.VirtualScanCode := $2004;
        end;
      PTCKEY_F:
        begin
          Result.AsciiChar := #6;
          Result.UnicodeChar := WideChar(6);
          Result.VirtualScanCode := $2106;
        end;
      PTCKEY_G:
        begin
          Result.AsciiChar := #7;
          Result.UnicodeChar := WideChar(7);
          Result.VirtualScanCode := $2207;
        end;
      PTCKEY_H:
        begin
          Result.AsciiChar := #8;
          Result.UnicodeChar := WideChar(8);
          Result.VirtualScanCode := $2308;
        end;
      PTCKEY_J:
        begin
          Result.AsciiChar := #10;
          Result.UnicodeChar := WideChar(10);
          Result.VirtualScanCode := $240A;
        end;
      PTCKEY_K:
        begin
          Result.AsciiChar := #11;
          Result.UnicodeChar := WideChar(11);
          Result.VirtualScanCode := $250B;
        end;
      PTCKEY_L:
        begin
          Result.AsciiChar := #12;
          Result.UnicodeChar := WideChar(12);
          Result.VirtualScanCode := $260C;
        end;
      PTCKEY_Z:
        begin
          Result.AsciiChar := #26;
          Result.UnicodeChar := WideChar(26);
          Result.VirtualScanCode := $2C1A;
        end;
      PTCKEY_X:
        begin
          Result.AsciiChar := #24;
          Result.UnicodeChar := WideChar(24);
          Result.VirtualScanCode := $2D18;
        end;
      PTCKEY_C:
        begin
          Result.AsciiChar := #3;
          Result.UnicodeChar := WideChar(3);
          Result.VirtualScanCode := $2E03;
        end;
      PTCKEY_V:
        begin
          Result.AsciiChar := #22;
          Result.UnicodeChar := WideChar(22);
          Result.VirtualScanCode := $2F16;
        end;
      PTCKEY_B:
        begin
          Result.AsciiChar := #2;
          Result.UnicodeChar := WideChar(2);
          Result.VirtualScanCode := $3002;
        end;
      PTCKEY_N:
        begin
          Result.AsciiChar := #14;
          Result.UnicodeChar := WideChar(14);
          Result.VirtualScanCode := $310E;
        end;
      PTCKEY_M:
        begin
          Result.AsciiChar := #13;
          Result.UnicodeChar := WideChar(13);
          Result.VirtualScanCode := $320D;
        end;
    end
  else if ev.shift then
    case ev.Code of
      PTCKEY_ESCAPE:
        begin
          Result.AsciiChar := #27;
          Result.UnicodeChar := WideChar(27);
          Result.VirtualScanCode := $011B;
        end;
      PTCKEY_F1:
        begin
          Result.VirtualScanCode := $5400;
          Result.VirtualKeyCode := kbdF1;
        end;
      PTCKEY_F2:
        begin
          Result.VirtualScanCode := $5500;
          Result.VirtualKeyCode := kbdF2;
        end;
      PTCKEY_F3:
        begin
          Result.VirtualScanCode := $5600;
          Result.VirtualKeyCode := kbdF3;
        end;
      PTCKEY_F4:
        begin
          Result.VirtualScanCode := $5700;
          Result.VirtualKeyCode := kbdF4;
        end;
      PTCKEY_F5:
        begin
          Result.VirtualScanCode := $5800;
          Result.VirtualKeyCode := kbdF5;
        end;
      PTCKEY_F6:
        begin
          Result.VirtualScanCode := $5900;
          Result.VirtualKeyCode := kbdF6;
        end;
      PTCKEY_F7:
        begin
          Result.VirtualScanCode := $5A00;
          Result.VirtualKeyCode := kbdF7;
        end;
      PTCKEY_F8:
        begin
          Result.VirtualScanCode := $5B00;
          Result.VirtualKeyCode := kbdF8;
        end;
      PTCKEY_F9:
        begin
          Result.VirtualScanCode := $5C00;
          Result.VirtualKeyCode := kbdF9;
        end;
      PTCKEY_F10:
        begin
          Result.VirtualScanCode := $5D00;
          Result.VirtualKeyCode := kbdF10;
        end;
      PTCKEY_F11:
        begin
          Result.VirtualScanCode := $8700;
          Result.VirtualKeyCode := kbdF11;
        end;
      PTCKEY_F12:
        begin
          Result.VirtualScanCode := $8800;
          Result.VirtualKeyCode := kbdF12;
        end;
      PTCKEY_ENTER:
        begin
          Result.AsciiChar := #13;
          Result.UnicodeChar := WideChar(13);
          if pmkNumPadKey in ev.ModifierKeys then
            Result.VirtualScanCode := $E00D
          else
            Result.VirtualScanCode := $1C0D;
        end;
      PTCKEY_BACKSPACE:
        begin
          Result.AsciiChar := #8;
          Result.UnicodeChar := WideChar(8);
          Result.VirtualScanCode := $0E08;
        end;
      PTCKEY_TAB:
        Result.VirtualScanCode := $0F00;
      PTCKEY_UP:
        begin
          Result.VirtualScanCode := $4800;
          Result.VirtualKeyCode := kbdUp;
        end;
      PTCKEY_LEFT:
        begin
          Result.VirtualScanCode := $4B00;
          Result.VirtualKeyCode := kbdLeft;
        end;
      PTCKEY_DOWN:
        begin
          Result.VirtualScanCode := $5000;
          Result.VirtualKeyCode := kbdDown;
        end;
      PTCKEY_RIGHT:
        begin
          Result.VirtualScanCode := $4D00;
          Result.VirtualKeyCode := kbdRight;
        end;
      PTCKEY_INSERT:
        begin
          Result.VirtualScanCode := $5200;
          Result.VirtualKeyCode := kbdInsert;
        end;
      PTCKEY_HOME:
        begin
          Result.VirtualScanCode := $4700;
          Result.VirtualKeyCode := kbdHome;
        end;
      PTCKEY_PAGEUP:
        begin
          Result.VirtualScanCode := $4900;
          Result.VirtualKeyCode := kbdPgUp;
        end;
      PTCKEY_DELETE:
        begin
          Result.VirtualScanCode := $5300;
          Result.VirtualKeyCode := kbdDelete;
        end;
      PTCKEY_END:
        begin
          Result.VirtualScanCode := $4F00;
          Result.VirtualKeyCode := kbdEnd;
        end;
      PTCKEY_PAGEDOWN:
        begin
          Result.VirtualScanCode := $5100;
          Result.VirtualKeyCode := kbdPgDn;
        end;
      else
      begin
        if (ev.unicode >= 32) and (ev.unicode <= 126) then
        begin
          Result.UnicodeChar := WideChar(ev.unicode);
          Result.AsciiChar := Chr(ev.Unicode);
        end
        else if ((ev.unicode >= 128) and (ev.unicode <= $D7FF)) or
                ((ev.unicode >= $E000) and (ev.unicode <= $FFFF)) then
        begin
          Result.UnicodeChar := WideChar(ev.unicode);
          Result.AsciiChar := '?';
        end
        else
          Result := NilEnhancedKeyEvent;
      end;
    end
  else
    case ev.Code of
      PTCKEY_ESCAPE:
        begin
          Result.AsciiChar := #27;
          Result.UnicodeChar := WideChar(27);
          Result.VirtualScanCode := $011B;
        end;
      PTCKEY_F1:
        begin
          Result.VirtualScanCode := $3B00;
          Result.VirtualKeyCode := kbdF1;
        end;
      PTCKEY_F2:
        begin
          Result.VirtualScanCode := $3C00;
          Result.VirtualKeyCode := kbdF2;
        end;
      PTCKEY_F3:
        begin
          Result.VirtualScanCode := $3D00;
          Result.VirtualKeyCode := kbdF3;
        end;
      PTCKEY_F4:
        begin
          Result.VirtualScanCode := $3E00;
          Result.VirtualKeyCode := kbdF4;
        end;
      PTCKEY_F5:
        begin
          Result.VirtualScanCode := $3F00;
          Result.VirtualKeyCode := kbdF5;
        end;
      PTCKEY_F6:
        begin
          Result.VirtualScanCode := $4000;
          Result.VirtualKeyCode := kbdF6;
        end;
      PTCKEY_F7:
        begin
          Result.VirtualScanCode := $4100;
          Result.VirtualKeyCode := kbdF7;
        end;
      PTCKEY_F8:
        begin
          Result.VirtualScanCode := $4200;
          Result.VirtualKeyCode := kbdF8;
        end;
      PTCKEY_F9:
        begin
          Result.VirtualScanCode := $4300;
          Result.VirtualKeyCode := kbdF9;
        end;
      PTCKEY_F10:
        begin
          Result.VirtualScanCode := $4400;
          Result.VirtualKeyCode := kbdF10;
        end;
      PTCKEY_F11:
        begin
          Result.VirtualScanCode := $8500;
          Result.VirtualKeyCode := kbdF11;
        end;
      PTCKEY_F12:
        begin
          Result.VirtualScanCode := $8600;
          Result.VirtualKeyCode := kbdF12;
        end;
      PTCKEY_ENTER:
        begin
          Result.AsciiChar := #13;
          Result.UnicodeChar := WideChar(13);
          if pmkNumPadKey in ev.ModifierKeys then
            Result.VirtualScanCode := $E00D
          else
            Result.VirtualScanCode := $1C0D;
        end;
      PTCKEY_BACKSPACE:
        begin
          Result.AsciiChar := #8;
          Result.UnicodeChar := WideChar(8);
          Result.VirtualScanCode := $0E08;
        end;
      PTCKEY_TAB:
        begin
          Result.AsciiChar := #9;
          Result.UnicodeChar := WideChar(9);
          Result.VirtualScanCode := $0F09;
        end;
      PTCKEY_UP:
        begin
          Result.VirtualScanCode := $4800;
          Result.VirtualKeyCode := kbdUp;
        end;
      PTCKEY_LEFT:
        begin
          Result.VirtualScanCode := $4B00;
          Result.VirtualKeyCode := kbdLeft;
        end;
      PTCKEY_DOWN:
        begin
          Result.VirtualScanCode := $5000;
          Result.VirtualKeyCode := kbdDown;
        end;
      PTCKEY_RIGHT:
        begin
          Result.VirtualScanCode := $4D00;
          Result.VirtualKeyCode := kbdRight;
        end;
      PTCKEY_INSERT:
        begin
          Result.VirtualScanCode := $5200;
          Result.VirtualKeyCode := kbdInsert;
        end;
      PTCKEY_HOME:
        begin
          Result.VirtualScanCode := $4700;
          Result.VirtualKeyCode := kbdHome;
        end;
      PTCKEY_PAGEUP:
        begin
          Result.VirtualScanCode := $4900;
          Result.VirtualKeyCode := kbdPgUp;
        end;
      PTCKEY_DELETE:
        begin
          Result.VirtualScanCode := $5300;
          Result.VirtualKeyCode := kbdDelete;
        end;
      PTCKEY_END:
        begin
          Result.VirtualScanCode := $4F00;
          Result.VirtualKeyCode := kbdEnd;
        end;
      PTCKEY_PAGEDOWN:
        begin
          Result.VirtualScanCode := $5100;
          Result.VirtualKeyCode := kbdPgDn;
        end;
      else
      begin
        if (ev.unicode >= 32) and (ev.unicode <= 126) then
        begin
          Result.UnicodeChar := WideChar(ev.unicode);
          Result.AsciiChar := Chr(ev.Unicode);
        end
        else if ((ev.unicode >= 128) and (ev.unicode <= $D7FF)) or
                ((ev.unicode >= $E000) and (ev.unicode <= $FFFF)) then
        begin
          Result.UnicodeChar := WideChar(ev.unicode);
          Result.AsciiChar := '?';
        end
        else
          Result := NilEnhancedKeyEvent;
      end;
    end;

  (*if ScanCode <> kbNoKey then
  begin
    Result.VirtualScanCode := ScanCode shl 8;
    Result.AsciiChar := #0;
    Result.UnicodeChar := WideChar(0);
  end
  else
  begin
    case ev.code of
      PTCKEY_ENTER:
        begin
          Result.VirtualScanCode := $1C0D;
          Result.AsciiChar := #13;
          Result.UnicodeChar := WideChar(13);
        end;
      PTCKEY_ESCAPE:
        begin
          Result.VirtualScanCode := $011B;
          Result.AsciiChar := #27;
          Result.UnicodeChar := WideChar(27);
        end;
      PTCKEY_TAB:
        begin
          Result.VirtualScanCode := $0F09;
          Result.AsciiChar := #9;
          Result.UnicodeChar := WideChar(9);
        end;
      PTCKEY_BACKSPACE:
        begin
          if ev.alt Then
            Result.VirtualScanCode := $0E00
          else if ev.control Then
            Result.VirtualScanCode := $0E7F
          else
          begin
            Result.VirtualScanCode := $0E08;
            Result.AsciiChar := #8;
            Result.UnicodeChar := WideChar(8);
          end;
        end;
      else
      begin
        if (ev.unicode >= 32) and (ev.unicode <= 126) then
        begin
          Result.UnicodeChar := WideChar(ev.unicode);
          Result.AsciiChar := Chr(ev.Unicode);
        end
        else
          Result := NilEnhancedKeyEvent;
      end;
    end;
  end;

  if ev.Control and (Result.AsciiChar >= '@') and (UpCase(Result.AsciiChar) <= ']') then
  begin
    Result.AsciiChar := Chr(Ord(UpCase(Result.AsciiChar)) - Ord('@'));
    Result.UnicodeChar := WideChar(Result.AsciiChar);
  end;*)

  if Result <> NilEnhancedKeyEvent then
    Result.ShiftState := ShiftState;
end;

procedure SysInitKeyboard;
begin
  if InitializedSubsystems = [] then
    KVMInit;
  Include(InitializedSubsystems, sKeyboard);
  LastPolledEventBuffer := NilEnhancedKeyEvent;
end;

procedure SysDoneKeyboard;
begin
  if sKeyboard in InitializedSubsystems then
  begin
    Exclude(InitializedSubsystems, sKeyboard);
    if InitializedSubsystems = [] then
      KVMDone;
  end;
end;

function SysGetShiftState: Byte;
begin
  KVMHandleEvents;
  Result := ShiftState and $f;
end;

function SysGetEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  if LastPolledEventBuffer <> NilEnhancedKeyEvent then
  begin
    Result := LastPolledEventBuffer;
    LastPolledEventBuffer := NilEnhancedKeyEvent;
    exit;
  end;

  repeat
    Result := ConvertPTCKeyEventToTEnhancedKeyEvent(KVMGetKeyEvent);
  until Result <> NilEnhancedKeyEvent;
end;

function SysPollEnhancedKeyEvent: TEnhancedKeyEvent;
begin
  if LastPolledEventBuffer <> NilEnhancedKeyEvent then
  begin
    Result := LastPolledEventBuffer;
    exit;
  end;

  LastPolledEventBuffer := ConvertPTCKeyEventToTEnhancedKeyEvent(KVMPollKeyEvent);
  Result := LastPolledEventBuffer;
end;

procedure SysInitVideo;
begin
  if InitializedSubsystems = [] then
    KVMInit;
  Include(InitializedSubsystems, sVideo);
  ScreenWidth := CurrentWidth;
  ScreenHeight := CurrentHeight;
  ScreenColor := True;
  CursorX := 0;
  CursorY := 0;
  CursorType := crUnderLine;
end;

procedure SysDoneVideo;
begin
  if sVideo in InitializedSubsystems then
  begin
    Exclude(InitializedSubsystems, sVideo);
    if InitializedSubsystems = [] then
      KVMDone;
  end;
end;

procedure SysUpdateScreen(Force: Boolean);
var
  DoUpdate: Boolean;
  I: SizeInt;
begin
  UpdateBlinkState;
  if Force then
    DoUpdate := True
  else
  begin
    DoUpdate := (CursorType <> LastDrawnCursorType) or
                (CursorBlinkState <> LastDrawnCursorBlinkState) or
                (FastBlinkTextState <> LastDrawnFastBlinkTextState) or
                (SlowBlinkTextState <> LastDrawnSlowBlinkTextState) or
                (CursorX <> LastDrawnCursorX) or
                (CursorY <> LastDrawnCursorY);
    if not DoUpdate then
      for I := Low(EnhancedVideoBuf) to High(EnhancedVideoBuf) do
        if OldEnhancedVideoBuf[I] <> EnhancedVideoBuf[I] then
        begin
          DoUpdate := True;
          break;
        end;
  end;

  if DoUpdate then
  begin
    if (Timer.Time - LastFrameTime) >= (1/70) then
    begin
      KVMUpdateScreen;
      LastFrameTime := Timer.Time;
      for I := Low(EnhancedVideoBuf) to High(EnhancedVideoBuf) do
        OldEnhancedVideoBuf[I] := EnhancedVideoBuf[I];
    end;
  end;
end;

function SysSetVideoMode(const mode:TVideoMode): Boolean;
begin
  SetMode(mode.Col, mode.Row);
  screenwidth:=mode.col;
  screenheight:=mode.row;
  screencolor:=true;
  Result := True;
end;

function SysGetVideoModeCount: Word;
begin
  Result := 1;
end;

function SysGetVideoModeData(Index: Word; var Data: TVideoMode): Boolean;
begin
  if Index < 1 then
  begin
    Data.Col := InitialWidth;
    Data.Row := InitialHeight;
    Data.Color := True;
    Result := True;
  end
  else
    Result := False;
end;

procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  if (NewCursorX <> CursorX) or (NewCursorY <> CursorY) then
  begin
    CursorX := NewCursorX;
    CursorY := NewCursorY;
    SysUpdateScreen(True);
  end;
end;

function SysGetCursorType: Word;
begin
  Result := CursorType;
end;

procedure SysSetCursorType(NewType: Word);
begin
  if CursorType = NewType then
    exit;
  CursorType := NewType;
  SysUpdateScreen(True);
end;

function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpUnderLine + cpBlink + cpColor;
end;

procedure SysInitMouse;
begin
  if InitializedSubsystems = [] then
    KVMInit;
  Include(InitializedSubsystems, sMouse);
end;

procedure SysDoneMouse;
begin
  if sMouse in InitializedSubsystems then
  begin
    Exclude(InitializedSubsystems, sMouse);
    if InitializedSubsystems = [] then
      KVMDone;
  end;
end;

function SysDetectMouse: byte;
begin
  Result := 3;
end;

procedure SysShowMouse;
begin
  {todo...}
end;

procedure SysHideMouse;
begin
  {todo...}
end;

function SysGetMouseX: word;
begin
  KVMHandleEvents;
  Result := MouseX;
end;

function SysGetMouseY: word;
begin
  KVMHandleEvents;
  Result := MouseY;
end;

function SysGetMouseButtons: word;
begin
  KVMHandleEvents;
  Result := MouseButtons;
end;

procedure SysGetMouseEvent(var MouseEvent: TMouseEvent);
var
  NewMouseX, NewMouseY, NewMouseButtons: Integer;
begin
  FillChar(MouseEvent, SizeOf(MouseEvent), 0);
end;

function SysPollMouseEvent(var MouseEvent: TMouseEvent): Boolean;
begin
  KVMHandleEvents;
  Result := False;
  FillChar(MouseEvent, SizeOf(MouseEvent), 0);
end;

function CheckPendingResize(out NewMode: TVideoMode): Boolean;
begin
  if PendingResize <> nil then
  begin
    GetVideoMode(NewMode);
    NewMode.Col := PendingResize.Width div CurrentFont.Width;
    NewMode.Row := PendingResize.Height div CurrentFont.Height;
    Result := True;
  end
  else
  begin
    FillChar(NewMode, SizeOf(NewMode), 0);
    Result := False;
  end;
end;

procedure StartBlinkingCursor;
begin
  CursorBlinkingEnabled := True;
end;

procedure StopBlinkingCursor;
begin
  CursorBlinkingEnabled := False;
  CursorBlinkState := True;
  UpdateScreen(False);
end;

const
  SysKeyboardDriver : TKeyboardDriver = (
    InitDriver : @SysInitKeyBoard;
    DoneDriver : @SysDoneKeyBoard;
    GetKeyevent : Nil;
    PollKeyEvent : Nil;
    GetShiftState : @SysGetShiftState;
    TranslateKeyEvent : Nil;
    TranslateKeyEventUnicode : Nil;
    GetEnhancedKeyEvent : @SysGetEnhancedKeyEvent;
    PollEnhancedKeyEvent : @SysPollEnhancedKeyEvent;
  );
  SysVideoDriver : TVideoDriver = (
    InitDriver : nil;
    InitEnhancedDriver: @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    UpdateScreenArea : Nil;
    ClearScreen : Nil;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : @SysGetVideoModeCount;
    GetVideoModeData : @SysGetVideoModeData;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities;
    GetActiveCodePage : Nil;
    ActivateCodePage : Nil;
    GetSupportedCodePageCount : Nil;
    GetSupportedCodePage : Nil;
  );
  SysMouseDriver : TMouseDriver = (
    UseDefaultQueue : true;
    InitDriver      : @SysInitMouse;
    DoneDriver      : @SysDoneMouse;
    DetectMouse     : @SysDetectMouse;
    ShowMouse       : @SysShowMouse;
    HideMouse       : @SysHideMouse;
    GetMouseX       : @SysGetMouseX;
    GetMouseY       : @SysGetMouseY;
    GetMouseButtons : @SysGetMouseButtons;
    SetMouseXY      : Nil;
    GetMouseEvent   : @SysGetMouseEvent;
    PollMouseEvent  : @SysPollMouseEvent;
    PutMouseEvent   : Nil;
  );

procedure RegisterPtcKvmDrivers;
begin
  SetKeyboardDriver(SysKeyboardDriver);
  SetVideoDriver(SysVideoDriver);
  SetMouseDriver(SysMouseDriver);
end;

begin
  InitializedSubsystems := [];
end.

