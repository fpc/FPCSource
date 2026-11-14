{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Sixel graphics: terminal detection and bitmap output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPSixel;
{$ENDIF}

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

const
  SixelDefaultCellWidth = 10;
  SixelDefaultCellHeight = 20;
  SixelMinColors = 2;
  SixelMaxColors = 1024;

type
  ESixel = class(Exception);

  TSixelColor = packed record
    R : Byte;
    G : Byte;
    B : Byte;
  end;

  TSixelDetectMethod = (sdmNone, sdmDeviceAttributes, sdmEnvironment);

  // Terminal capabilities found by DetectSixel; numeric fields are 0 when unknown.
  TSixelTerminalInfo = record
    // Both standard input and standard output are a terminal.
    IsTerminal : Boolean;
    Supported : Boolean;
    Method : TSixelDetectMethod;
    // Parameters of the primary device attributes reply, e.g. '63;1;2;4;6;9'.
    DeviceAttributes : string;
    ColorRegisters : Integer;
    MaxColorRegisters : Integer;
    // Largest sixel image the terminal accepts, in pixels.
    MaxWidth : Integer;
    MaxHeight : Integer;
    Columns : Integer;
    Rows : Integer;
    // Size of the text area in pixels.
    PixelWidth : Integer;
    PixelHeight : Integer;
    // Size of a character cell in pixels; defaults when CellSizeKnown is False.
    CellWidth : Integer;
    CellHeight : Integer;
    CellSizeKnown : Boolean;
  end;

  { TSixelTerminal }

  TSixelTerminal = class
  public
    // True if standard input and standard output are both a terminal.
    class function IsTerminal : Boolean;
    // Switch terminal input to unbuffered, non-echoing mode; calls nest.
    class function EnterRawMode : Boolean;
    // Undo one EnterRawMode call; the outermost call restores the saved mode.
    class procedure LeaveRawMode;
    // Wait at most aTimeoutMS milliseconds for input; True if a character is available.
    class function InputAvailable(aTimeoutMS : Integer) : Boolean;
    // Read one input character, waiting at most aTimeoutMS milliseconds.
    class function ReadChar(out aChar : AnsiChar; aTimeoutMS : Integer) : Boolean;
    // Discard all input that is already available.
    class procedure FlushInput;
    // Write aData to standard output and flush it.
    class procedure Write(const aData : string);
    // Send aRequest and read the reply until aTerminator arrives or aTimeoutMS elapses.
    class function Query(const aRequest : string; aTerminator : AnsiChar; aTimeoutMS : Integer) : string;
    // Get the terminal size in cells and, when the system reports it, in pixels.
    class function GetSize(out aColumns, aRows, aPixelWidth, aPixelHeight : Integer) : Boolean;
    // Ask the terminal to use aCount sixel color registers.
    class procedure SetColorRegisters(aCount : Integer);
    // Show or hide the text cursor.
    class procedure ShowCursor(aVisible : Boolean);
    // Save the cursor position.
    class procedure SaveCursor;
    // Restore the cursor position saved with SaveCursor.
    class procedure RestoreCursor;
    // Move the cursor to 1-based column aColumn and row aRow.
    class procedure MoveTo(aColumn, aRow : Integer);
  end;

  TSixelPaletteMode = (spmAdaptive, spmFixed);

  { TSixelBitmap }

  TSixelBitmap = class
  private
    FWidth : Integer;
    FHeight : Integer;
    FPixels : array of TSixelColor;
    FMaxColors : Integer;
    FPaletteMode : TSixelPaletteMode;
    FDither : Boolean;
    FTransparent : Boolean;
    FTransparentColor : TSixelColor;
    function GetPixel(aX, aY : Integer) : TSixelColor;
    procedure SetPixel(aX, aY : Integer; const aColor : TSixelColor);
    procedure SetMaxColors(aValue : Integer);
    procedure BlendPixel(aX, aY : Integer; const aColor : TSixelColor; aAlpha : Double);
    procedure HLine(aX1, aX2, aY : Integer; const aColor : TSixelColor);
  public
    // Create a bitmap of aWidth x aHeight black pixels.
    constructor Create(aWidth, aHeight : Integer);
    // Resize the bitmap; all pixels become black.
    procedure SetSize(aWidth, aHeight : Integer);
    // Set all pixels to aColor.
    procedure Clear(const aColor : TSixelColor);
    // Fill the rectangle with corners (aX1,aY1) and (aX2,aY2), both inclusive.
    procedure FillRect(aX1, aY1, aX2, aY2 : Integer; const aColor : TSixelColor);
    // Draw a one pixel wide line.
    procedure Line(aX1, aY1, aX2, aY2 : Integer; const aColor : TSixelColor);
    // Draw a one pixel wide circle outline.
    procedure Circle(aCX, aCY, aRadius : Integer; const aColor : TSixelColor);
    // Fill a circle with anti-aliased edges; the center may lie between pixels.
    procedure FillCircle(aCX, aCY, aRadius : Double; const aColor : TSixelColor);
    // Fill the triangle with the given corners.
    procedure FillTriangle(aX1, aY1, aX2, aY2, aX3, aY3 : Integer; const aColor : TSixelColor);
    // Encode the bitmap as a complete sixel sequence.
    function ToSixel : string;
    // Write the bitmap at the cursor position.
    procedure Draw;
    // Write the bitmap with its top left corner at 1-based cell (aColumn,aRow); the cursor does not move.
    procedure DrawAt(aColumn, aRow : Integer);
    // Width in pixels.
    property Width : Integer read FWidth;
    // Height in pixels.
    property Height : Integer read FHeight;
    // Pixel colors; reads outside the bitmap give black, writes outside are ignored.
    property Pixels[aX, aY : Integer] : TSixelColor read GetPixel write SetPixel; default;
    // Maximum number of palette entries used when encoding.
    property MaxColors : Integer read FMaxColors write SetMaxColors;
    // Palette built from the image colors, or a fixed color cube with grays.
    property PaletteMode : TSixelPaletteMode read FPaletteMode write FPaletteMode;
    // Apply Floyd-Steinberg error diffusion when mapping pixels to the palette.
    property Dither : Boolean read FDither write FDither;
    // Leave pixels of TransparentColor undrawn.
    property Transparent : Boolean read FTransparent write FTransparent;
    // Color treated as transparent when Transparent is True.
    property TransparentColor : TSixelColor read FTransparentColor write FTransparentColor;
  end;

// Make a TSixelColor.
function SixelColor(aR, aG, aB : Byte) : TSixelColor;
// Mix aColor1 and aColor2; aFactor 0 gives aColor1, 1 gives aColor2.
function SixelBlend(const aColor1, aColor2 : TSixelColor; aFactor : Double) : TSixelColor;
// Probe the terminal for sixel support and geometry; True if sixel is supported.
function DetectSixel(out aInfo : TSixelTerminalInfo; aTimeoutMS : Integer = 1000) : Boolean;
// Raise the number of color registers to aCount if the terminal allows; returns the number of colors to use.
function SixelRequestColors(var aInfo : TSixelTerminalInfo; aCount : Integer = 256) : Integer;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
{$IFDEF UNIX}
  UnixApi.Base, UnixApi.TermIO,
{$ENDIF}
{$IFDEF WINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.Math;
{$ELSE}
{$IFDEF UNIX}
  BaseUnix, TermIO,
{$ENDIF}
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Math;
{$ENDIF}

const
  ESC = #27;
  CSI = ESC + '[';
  HistBits = 5;
  HistSize = 1 shl (3 * HistBits);
  CacheSize = 1 shl 18;
  TransparentIndex = $FFFF;

type
  TSixelPalette = array of TSixelColor;
  TIndexArray = array of Word;

  TCSIReply = record
    Prefix : AnsiChar;
    Params : array of Integer;
    FinalChar : AnsiChar;
  end;
  TCSIReplyArray = array of TCSIReply;

  { TSixelBuffer }

  TSixelBuffer = record
    Data : AnsiString;
    Len : SizeInt;
    procedure Init(aCapacity : SizeInt);
    procedure AddChar(aChar : AnsiChar);
    procedure Add(const aText : AnsiString);
    procedure AddInt(aValue : Integer);
    function Text : AnsiString;
  end;

  TColorBox = record
    Lo : array[0..2] of Integer;
    Hi : array[0..2] of Integer;
    Count : QWord;
  end;

var
  GRawDepth : Integer = 0;
  GHasPending : Boolean = False;
  GPending : AnsiChar;
{$IFDEF UNIX}
  GInputFD : cint = -1;
  GOwnInputFD : Boolean = False;
  GSavedTermios : Termios;
{$ENDIF}
{$IFDEF WINDOWS}
  GVTOutputDone : Boolean = False;
  GSavedInputMode : DWORD;
{$ENDIF}

{ TSixelBuffer }

procedure TSixelBuffer.Init(aCapacity : SizeInt);

begin
  SetLength(Data, Max(aCapacity, 64));
  Len := 0;
end;


procedure TSixelBuffer.AddChar(aChar : AnsiChar);

begin
  if Len >= Length(Data) then
    SetLength(Data, Length(Data) * 2);
  Inc(Len);
  Data[Len] := aChar;
end;


procedure TSixelBuffer.Add(const aText : AnsiString);

var
  lCount : SizeInt;

begin
  lCount := Length(aText);
  if lCount = 0 then
    Exit;
  while Len + lCount > Length(Data) do
    SetLength(Data, Length(Data) * 2);
  Move(aText[1], Data[Len + 1], lCount);
  Inc(Len, lCount);
end;


procedure TSixelBuffer.AddInt(aValue : Integer);

begin
  Add(AnsiString(IntToStr(aValue)));
end;


function TSixelBuffer.Text : AnsiString;

begin
  Result := Copy(Data, 1, Len);
end;


function SixelColor(aR, aG, aB : Byte) : TSixelColor;

begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
end;


function SixelBlend(const aColor1, aColor2 : TSixelColor; aFactor : Double) : TSixelColor;

begin
  if aFactor <= 0 then
    Exit(aColor1);
  if aFactor >= 1 then
    Exit(aColor2);
  Result.R := Round(aColor1.R + (aColor2.R - aColor1.R) * aFactor);
  Result.G := Round(aColor1.G + (aColor2.G - aColor1.G) * aFactor);
  Result.B := Round(aColor1.B + (aColor2.B - aColor1.B) * aFactor);
end;


// Value of environment variable aName, empty if it is not set.
function EnvVar(const aName : string) : string;

begin
{$IFDEF FPC_DOTTEDUNITS}
  Result := System.SysUtils.GetEnvironmentVariable(aName);
{$ELSE}
  Result := SysUtils.GetEnvironmentVariable(aName);
{$ENDIF}
end;


// Exchange the values of aA and aB.
procedure SwapInt(var aA, aB : Integer); inline;

var
  lTemp : Integer;

begin
  lTemp := aA;
  aA := aB;
  aB := lTemp;
end;


// True if both colors have the same components.
function SameColor(const aColor1, aColor2 : TSixelColor) : Boolean; inline;

begin
  Result := (aColor1.R = aColor2.R) and (aColor1.G = aColor2.G) and (aColor1.B = aColor2.B);
end;


// Milliseconds left until aDeadline, never negative.
function Remaining(aDeadline : QWord) : Integer;

var
  lNow : QWord;

begin
  lNow := GetTickCount64;
  if lNow >= aDeadline then
    Result := 0
  else
    Result := aDeadline - lNow;
end;


// Split aText into the CSI sequences it contains; other bytes are skipped.
function ParseCSIReplies(const aText : string) : TCSIReplyArray;

var
  I, lLen, lCount, lNum : Integer;
  lReply : TCSIReply;

  // Append the number being parsed to the parameters of lReply.
  procedure AddParam;

  begin
    SetLength(lReply.Params, Length(lReply.Params) + 1);
    lReply.Params[High(lReply.Params)] := Max(lNum, 0);
    lNum := -1;
  end;

begin
  Result := nil;
  lCount := 0;
  lLen := Length(aText);
  I := 1;
  while I < lLen do
    begin
    if (aText[I] <> ESC) or (aText[I + 1] <> '[') then
      begin
      Inc(I);
      Continue;
      end;
    Inc(I, 2);
    lReply := Default(TCSIReply);
    if (I <= lLen) and (aText[I] in ['<'..'?']) then
      begin
      lReply.Prefix := aText[I];
      Inc(I);
      end;
    lNum := -1;
    while (I <= lLen) and (aText[I] in ['0'..'9', ';']) do
      begin
      if aText[I] = ';' then
        AddParam
      else if lNum < 100000000 then
        lNum := Max(lNum, 0) * 10 + Ord(aText[I]) - Ord('0');
      Inc(I);
      end;
    if (lNum >= 0) or (Length(lReply.Params) > 0) then
      AddParam;
    if I <= lLen then
      begin
      lReply.FinalChar := aText[I];
      Inc(I);
      SetLength(Result, lCount + 1);
      Result[lCount] := lReply;
      Inc(lCount);
      end;
    end;
end;


// Fill the sixel fields of aInfo from the device attributes reply in aReplies.
procedure ApplyDeviceAttributes(const aReplies : TCSIReplyArray; var aInfo : TSixelTerminalInfo);

var
  lReply : TCSIReply;
  I : Integer;

begin
  for lReply in aReplies do
    if (lReply.FinalChar = 'c') and (lReply.Prefix = '?') then
      begin
      aInfo.Method := sdmDeviceAttributes;
      aInfo.DeviceAttributes := '';
      for I := 0 to High(lReply.Params) do
        begin
        if I > 0 then
          aInfo.DeviceAttributes := aInfo.DeviceAttributes + ';';
        aInfo.DeviceAttributes := aInfo.DeviceAttributes + IntToStr(lReply.Params[I]);
        if (I > 0) and (lReply.Params[I] = 4) then
          aInfo.Supported := True;
        end;
      end;
end;


// Fill the graphics and geometry fields of aInfo from the replies in aReplies.
procedure ApplyGeometry(const aReplies : TCSIReplyArray; var aInfo : TSixelTerminalInfo);

var
  lReply : TCSIReply;
  lColorReplies : Integer;

begin
  lColorReplies := 0;
  for lReply in aReplies do
    if (lReply.FinalChar = 'S') and (lReply.Prefix = '?') and (Length(lReply.Params) >= 3) then
      begin
      if lReply.Params[0] = 1 then
        begin
        if lReply.Params[1] = 0 then
          if lColorReplies = 0 then
            aInfo.ColorRegisters := lReply.Params[2]
          else
            aInfo.MaxColorRegisters := lReply.Params[2];
        Inc(lColorReplies);
        end
      else if (lReply.Params[0] = 2) and (lReply.Params[1] = 0) and (Length(lReply.Params) >= 4) then
        begin
        aInfo.MaxWidth := lReply.Params[2];
        aInfo.MaxHeight := lReply.Params[3];
        end;
      end
    else if (lReply.FinalChar = 't') and (lReply.Prefix = #0) and (Length(lReply.Params) >= 3) then
      case lReply.Params[0] of
        6 :
          if (lReply.Params[1] > 0) and (lReply.Params[2] > 0) then
            begin
            aInfo.CellHeight := lReply.Params[1];
            aInfo.CellWidth := lReply.Params[2];
            aInfo.CellSizeKnown := True;
            end;
        4 :
          if (lReply.Params[1] > 0) and (lReply.Params[2] > 0) then
            begin
            aInfo.PixelHeight := lReply.Params[1];
            aInfo.PixelWidth := lReply.Params[2];
            end;
      end;
end;


// True if TERM or TERM_PROGRAM names a terminal known to support sixel.
function EnvironmentHasSixel : Boolean;

const
  TermPrefixes : array[0..4] of string = ('mlterm', 'yaft', 'foot', 'contour', 'mintty');
  Programs : array[0..4] of string = ('wezterm', 'iterm.app', 'mintty', 'mlterm', 'contour');

var
  lTerm, lProgram, S : string;

begin
  Result := False;
  lTerm := LowerCase(EnvVar('TERM'));
  lProgram := LowerCase(EnvVar('TERM_PROGRAM'));
  if Pos('sixel', lTerm) > 0 then
    Exit(True);
  for S in TermPrefixes do
    if Pos(S, lTerm) = 1 then
      Exit(True);
  for S in Programs do
    if lProgram = S then
      Exit(True);
end;


function DetectSixel(out aInfo : TSixelTerminalInfo; aTimeoutMS : Integer) : Boolean;

var
  lReplies : TCSIReplyArray;

begin
  aInfo := Default(TSixelTerminalInfo);
  aInfo.CellWidth := SixelDefaultCellWidth;
  aInfo.CellHeight := SixelDefaultCellHeight;
  aInfo.IsTerminal := TSixelTerminal.IsTerminal;
  TSixelTerminal.GetSize(aInfo.Columns, aInfo.Rows, aInfo.PixelWidth, aInfo.PixelHeight);
  if aInfo.IsTerminal and TSixelTerminal.EnterRawMode then
    try
      lReplies := ParseCSIReplies(TSixelTerminal.Query(CSI + 'c', 'c', aTimeoutMS));
      ApplyDeviceAttributes(lReplies, aInfo);
      if aInfo.Supported then
        begin
        lReplies := ParseCSIReplies(TSixelTerminal.Query(
                      CSI + '?1;1;0S' + CSI + '?1;4;0S' + CSI + '?2;1;0S' +
                      CSI + '16t' + CSI + '14t' + CSI + 'c', 'c', aTimeoutMS));
        ApplyGeometry(lReplies, aInfo);
        end;
    finally
      TSixelTerminal.LeaveRawMode;
    end;
  if not aInfo.Supported and EnvironmentHasSixel then
    begin
    aInfo.Supported := True;
    aInfo.Method := sdmEnvironment;
    end;
  if not aInfo.CellSizeKnown and (aInfo.PixelWidth > 0) and (aInfo.PixelHeight > 0)
     and (aInfo.Columns > 0) and (aInfo.Rows > 0) then
    begin
    aInfo.CellWidth := aInfo.PixelWidth div aInfo.Columns;
    aInfo.CellHeight := aInfo.PixelHeight div aInfo.Rows;
    aInfo.CellSizeKnown := (aInfo.CellWidth > 0) and (aInfo.CellHeight > 0);
    if not aInfo.CellSizeKnown then
      begin
      aInfo.CellWidth := SixelDefaultCellWidth;
      aInfo.CellHeight := SixelDefaultCellHeight;
      end;
    end;
  if (aInfo.PixelWidth = 0) and (aInfo.Columns > 0) then
    begin
    aInfo.PixelWidth := aInfo.Columns * aInfo.CellWidth;
    aInfo.PixelHeight := aInfo.Rows * aInfo.CellHeight;
    end;
  Result := aInfo.Supported;
end;


function SixelRequestColors(var aInfo : TSixelTerminalInfo; aCount : Integer) : Integer;

begin
  aCount := EnsureRange(aCount, SixelMinColors, SixelMaxColors);
  if (aInfo.ColorRegisters > 0) and (aInfo.ColorRegisters < aCount)
     and (aInfo.MaxColorRegisters > aInfo.ColorRegisters) then
    begin
    aInfo.ColorRegisters := Min(aCount, aInfo.MaxColorRegisters);
    TSixelTerminal.SetColorRegisters(aInfo.ColorRegisters);
    end;
  if aInfo.ColorRegisters > 0 then
    Result := Min(aCount, aInfo.ColorRegisters)
  else
    Result := aCount;
end;


{ TSixelTerminal }

{$IFDEF UNIX}

class function TSixelTerminal.IsTerminal : Boolean;

begin
  Result := (IsATTY(0) = 1) and (IsATTY(1) = 1);
end;


// Descriptor used to read terminal replies, opened on first use.
function InputFD : cint;

begin
  if GInputFD < 0 then
    if IsATTY(0) = 1 then
      GInputFD := 0
    else
      begin
      GInputFD := fpOpen('/dev/tty', O_RDWR);
      GOwnInputFD := GInputFD >= 0;
      end;
  Result := GInputFD;
end;


class function TSixelTerminal.EnterRawMode : Boolean;

var
  lTermios : Termios;

begin
  if GRawDepth = 0 then
    begin
    if (InputFD < 0) or (TCGetAttr(InputFD, GSavedTermios) <> 0) then
      Exit(False);
    lTermios := GSavedTermios;
    lTermios.c_lflag := lTermios.c_lflag and not (ICANON or ECHO);
    lTermios.c_cc[VMIN] := 1;
    lTermios.c_cc[VTIME] := 0;
    if TCSetAttr(InputFD, TCSANOW, lTermios) <> 0 then
      Exit(False);
    end;
  Inc(GRawDepth);
  Result := True;
end;


class procedure TSixelTerminal.LeaveRawMode;

begin
  if GRawDepth <= 0 then
    Exit;
  Dec(GRawDepth);
  if GRawDepth = 0 then
    TCSetAttr(InputFD, TCSANOW, GSavedTermios);
end;


class function TSixelTerminal.ReadChar(out aChar : AnsiChar; aTimeoutMS : Integer) : Boolean;

var
  lSet : TFDSet;

begin
  aChar := #0;
  if GHasPending then
    begin
    aChar := GPending;
    GHasPending := False;
    Exit(True);
    end;
  Result := False;
  if InputFD < 0 then
    begin
    Sleep(Max(aTimeoutMS, 0));
    Exit;
    end;
  fpFD_ZERO(lSet);
  fpFD_SET(InputFD, lSet);
  if fpSelect(InputFD + 1, @lSet, nil, nil, Max(aTimeoutMS, 0)) > 0 then
    Result := fpRead(InputFD, aChar, 1) = 1;
end;


class function TSixelTerminal.GetSize(out aColumns, aRows, aPixelWidth, aPixelHeight : Integer) : Boolean;

{$IF DECLARED(TIOCGWINSZ) and DECLARED(TWinSize)}
var
  lSize : TWinSize;
{$ENDIF}

begin
  aColumns := 0;
  aRows := 0;
  aPixelWidth := 0;
  aPixelHeight := 0;
  Result := False;
{$IF DECLARED(TIOCGWINSZ) and DECLARED(TWinSize)}
  lSize := Default(TWinSize);
  if fpIOCtl(1, TIOCGWINSZ, @lSize) = 0 then
    begin
    aColumns := lSize.ws_col;
    aRows := lSize.ws_row;
    aPixelWidth := lSize.ws_xpixel;
    aPixelHeight := lSize.ws_ypixel;
    Result := (aColumns > 0) and (aRows > 0);
    end;
{$ENDIF}
  if not Result then
    begin
    aColumns := StrToIntDef(EnvVar('COLUMNS'), 0);
    aRows := StrToIntDef(EnvVar('LINES'), 0);
    Result := (aColumns > 0) and (aRows > 0);
    end;
end;

{$ELSE}
{$IFDEF WINDOWS}

class function TSixelTerminal.IsTerminal : Boolean;

var
  lMode : DWORD;

begin
  Result := GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), @lMode)
            and GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), @lMode);
end;


class function TSixelTerminal.EnterRawMode : Boolean;

var
  lHandle : THandle;
  lMode : DWORD;

begin
  if GRawDepth = 0 then
    begin
    lHandle := GetStdHandle(STD_INPUT_HANDLE);
    if not GetConsoleMode(lHandle, @GSavedInputMode) then
      Exit(False);
    lMode := GSavedInputMode and not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT);
    if not SetConsoleMode(lHandle, lMode or ENABLE_VIRTUAL_TERMINAL_INPUT) then
      if not SetConsoleMode(lHandle, lMode) then
        Exit(False);
    end;
  Inc(GRawDepth);
  Result := True;
end;


class procedure TSixelTerminal.LeaveRawMode;

begin
  if GRawDepth <= 0 then
    Exit;
  Dec(GRawDepth);
  if GRawDepth = 0 then
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), GSavedInputMode);
end;


class function TSixelTerminal.ReadChar(out aChar : AnsiChar; aTimeoutMS : Integer) : Boolean;

var
  lHandle : THandle;
  lRecord : TInputRecord;
  lRead : DWORD;
  lDeadline : QWord;
  lChar : WideChar;

begin
  aChar := #0;
  if GHasPending then
    begin
    aChar := GPending;
    GHasPending := False;
    Exit(True);
    end;
  Result := False;
  lHandle := GetStdHandle(STD_INPUT_HANDLE);
  lDeadline := GetTickCount64 + QWord(Max(aTimeoutMS, 0));
  repeat
    if WaitForSingleObject(lHandle, Remaining(lDeadline)) <> WAIT_OBJECT_0 then
      Exit;
    lRead := 0;
    if not ReadConsoleInputW(lHandle, @lRecord, 1, @lRead) or (lRead = 0) then
      Exit;
    if (lRecord.EventType = KEY_EVENT) and lRecord.Event.KeyEvent.bKeyDown then
      begin
      lChar := lRecord.Event.KeyEvent.UnicodeChar;
      if (lChar <> #0) and (Ord(lChar) < 256) then
        begin
        aChar := AnsiChar(Ord(lChar));
        Exit(True);
        end;
      end;
  until Remaining(lDeadline) = 0;
end;


class function TSixelTerminal.GetSize(out aColumns, aRows, aPixelWidth, aPixelHeight : Integer) : Boolean;

var
  lInfo : TConsoleScreenBufferInfo;

begin
  aColumns := 0;
  aRows := 0;
  aPixelWidth := 0;
  aPixelHeight := 0;
  Result := GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), @lInfo);
  if Result then
    begin
    aColumns := lInfo.srWindow.Right - lInfo.srWindow.Left + 1;
    aRows := lInfo.srWindow.Bottom - lInfo.srWindow.Top + 1;
    end;
end;

{$ELSE}

class function TSixelTerminal.IsTerminal : Boolean;

begin
  Result := False;
end;


class function TSixelTerminal.EnterRawMode : Boolean;

begin
  Result := False;
end;


class procedure TSixelTerminal.LeaveRawMode;

begin
end;


class function TSixelTerminal.ReadChar(out aChar : AnsiChar; aTimeoutMS : Integer) : Boolean;

begin
  aChar := #0;
  if GHasPending then
    begin
    aChar := GPending;
    GHasPending := False;
    Exit(True);
    end;
  Sleep(Max(aTimeoutMS, 0));
  Result := False;
end;


class function TSixelTerminal.GetSize(out aColumns, aRows, aPixelWidth, aPixelHeight : Integer) : Boolean;

begin
  aColumns := StrToIntDef(EnvVar('COLUMNS'), 0);
  aRows := StrToIntDef(EnvVar('LINES'), 0);
  aPixelWidth := 0;
  aPixelHeight := 0;
  Result := (aColumns > 0) and (aRows > 0);
end;

{$ENDIF}
{$ENDIF}

class function TSixelTerminal.InputAvailable(aTimeoutMS : Integer) : Boolean;

var
  lChar : AnsiChar;

begin
  if GHasPending then
    Exit(True);
  Result := ReadChar(lChar, aTimeoutMS);
  if Result then
    begin
    GPending := lChar;
    GHasPending := True;
    end;
end;


class procedure TSixelTerminal.FlushInput;

var
  lChar : AnsiChar;

begin
  GHasPending := False;
  while ReadChar(lChar, 0) do
    ;
end;


class procedure TSixelTerminal.Write(const aData : string);

{$IFDEF WINDOWS}
var
  lHandle : THandle;
  lMode : DWORD;
{$ENDIF}

begin
{$IFDEF WINDOWS}
  if not GVTOutputDone then
    begin
    GVTOutputDone := True;
    lHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    if GetConsoleMode(lHandle, @lMode) then
      SetConsoleMode(lHandle, lMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    end;
{$ENDIF}
  System.Write(Output, aData);
  Flush(Output);
end;


class function TSixelTerminal.Query(const aRequest : string; aTerminator : AnsiChar; aTimeoutMS : Integer) : string;

var
  lDeadline : QWord;
  lChar : AnsiChar;

begin
  Result := '';
  Write(aRequest);
  lDeadline := GetTickCount64 + QWord(Max(aTimeoutMS, 0));
  while ReadChar(lChar, Remaining(lDeadline)) do
    begin
    Result := Result + Char(lChar);
    if lChar = aTerminator then
      Break;
    end;
end;


class procedure TSixelTerminal.SetColorRegisters(aCount : Integer);

begin
  Write(CSI + '?1;3;' + IntToStr(aCount) + 'S');
end;


class procedure TSixelTerminal.ShowCursor(aVisible : Boolean);

begin
  if aVisible then
    Write(CSI + '?25h')
  else
    Write(CSI + '?25l');
end;


class procedure TSixelTerminal.SaveCursor;

begin
  Write(ESC + '7');
end;


class procedure TSixelTerminal.RestoreCursor;

begin
  Write(ESC + '8');
end;


class procedure TSixelTerminal.MoveTo(aColumn, aRow : Integer);

begin
  Write(CSI + IntToStr(Max(aRow, 1)) + ';' + IntToStr(Max(aColumn, 1)) + 'H');
end;


// Fill aPalette with an evenly spaced color cube, the remaining entries are grays.
procedure BuildFixedPalette(aMaxColors : Integer; out aPalette : TSixelPalette);

var
  lLevels, lCount, lGrays, R, G, B, I : Integer;

begin
  SetLength(aPalette, aMaxColors);
  lCount := 0;
  if aMaxColors >= 8 then
    begin
    lLevels := 2;
    while (lLevels + 1) * (lLevels + 1) * (lLevels + 1) <= aMaxColors do
      Inc(lLevels);
    for R := 0 to lLevels - 1 do
      for G := 0 to lLevels - 1 do
        for B := 0 to lLevels - 1 do
          begin
          aPalette[lCount] := SixelColor(R * 255 div (lLevels - 1), G * 255 div (lLevels - 1), B * 255 div (lLevels - 1));
          Inc(lCount);
          end;
    lGrays := aMaxColors - lCount;
    for I := 1 to lGrays do
      begin
      R := I * 255 div (lGrays + 1);
      aPalette[lCount] := SixelColor(R, R, R);
      Inc(lCount);
      end;
    end
  else
    for I := 0 to aMaxColors - 1 do
      begin
      R := I * 255 div (aMaxColors - 1);
      aPalette[I] := SixelColor(R, R, R);
      end;
end;


// Histogram cell of a color, 5 bits per channel.
function HistIndex(aR, aG, aB : Integer) : Integer; inline;

begin
  Result := (aR shl (2 * HistBits)) or (aG shl HistBits) or aB;
end;


// Reduce aBox to the smallest box around its non-empty cells and recount it.
procedure ShrinkBox(const aHist : array of Cardinal; var aBox : TColorBox);

var
  R, G, B : Integer;
  lCount : Cardinal;
  lLo, lHi : array[0..2] of Integer;

begin
  for R := 0 to 2 do
    begin
    lLo[R] := 1 shl HistBits;
    lHi[R] := -1;
    end;
  aBox.Count := 0;
  for R := aBox.Lo[0] to aBox.Hi[0] do
    for G := aBox.Lo[1] to aBox.Hi[1] do
      for B := aBox.Lo[2] to aBox.Hi[2] do
        begin
        lCount := aHist[HistIndex(R, G, B)];
        if lCount = 0 then
          Continue;
        Inc(aBox.Count, lCount);
        lLo[0] := Min(lLo[0], R);
        lHi[0] := Max(lHi[0], R);
        lLo[1] := Min(lLo[1], G);
        lHi[1] := Max(lHi[1], G);
        lLo[2] := Min(lLo[2], B);
        lHi[2] := Max(lHi[2], B);
        end;
  if aBox.Count > 0 then
    for R := 0 to 2 do
      begin
      aBox.Lo[R] := lLo[R];
      aBox.Hi[R] := lHi[R];
      end;
end;


// Longest side of aBox, as an axis number 0..2.
function LongestAxis(const aBox : TColorBox) : Integer;

var
  I : Integer;

begin
  Result := 0;
  for I := 1 to 2 do
    if aBox.Hi[I] - aBox.Lo[I] > aBox.Hi[Result] - aBox.Lo[Result] then
      Result := I;
end;


// Split aBox at the median of its longest side; the upper part goes to aNew.
procedure SplitBox(const aHist : array of Cardinal; var aBox : TColorBox; out aNew : TColorBox);

var
  lAxis, lCut, R, G, B : Integer;
  lSlices : array[0..(1 shl HistBits) - 1] of QWord;
  lSum : QWord;
  lPos : array[0..2] of Integer;

begin
  lAxis := LongestAxis(aBox);
  FillChar(lSlices, SizeOf(lSlices), 0);
  for R := aBox.Lo[0] to aBox.Hi[0] do
    for G := aBox.Lo[1] to aBox.Hi[1] do
      for B := aBox.Lo[2] to aBox.Hi[2] do
        begin
        lPos[0] := R;
        lPos[1] := G;
        lPos[2] := B;
        Inc(lSlices[lPos[lAxis]], aHist[HistIndex(R, G, B)]);
        end;
  lSum := 0;
  lCut := aBox.Lo[lAxis];
  while lCut < aBox.Hi[lAxis] - 1 do
    begin
    Inc(lSum, lSlices[lCut]);
    if lSum * 2 >= aBox.Count then
      Break;
    Inc(lCut);
    end;
  aNew := aBox;
  aBox.Hi[lAxis] := lCut;
  aNew.Lo[lAxis] := lCut + 1;
  ShrinkBox(aHist, aBox);
  ShrinkBox(aHist, aNew);
end;


{ TSixelBitmap }

constructor TSixelBitmap.Create(aWidth, aHeight : Integer);

begin
  inherited Create;
  FMaxColors := 256;
  FPaletteMode := spmAdaptive;
  SetSize(aWidth, aHeight);
end;


procedure TSixelBitmap.SetSize(aWidth, aHeight : Integer);

begin
  if (aWidth < 0) or (aHeight < 0) then
    raise ESixel.CreateFmt('Invalid bitmap size: %dx%d', [aWidth, aHeight]);
  FWidth := aWidth;
  FHeight := aHeight;
  FPixels := nil;
  SetLength(FPixels, FWidth * FHeight);
end;


function TSixelBitmap.GetPixel(aX, aY : Integer) : TSixelColor;

begin
  if (aX < 0) or (aY < 0) or (aX >= FWidth) or (aY >= FHeight) then
    Result := SixelColor(0, 0, 0)
  else
    Result := FPixels[aY * FWidth + aX];
end;


procedure TSixelBitmap.SetPixel(aX, aY : Integer; const aColor : TSixelColor);

begin
  if (aX >= 0) and (aY >= 0) and (aX < FWidth) and (aY < FHeight) then
    FPixels[aY * FWidth + aX] := aColor;
end;


procedure TSixelBitmap.SetMaxColors(aValue : Integer);

begin
  FMaxColors := EnsureRange(aValue, SixelMinColors, SixelMaxColors);
end;


procedure TSixelBitmap.BlendPixel(aX, aY : Integer; const aColor : TSixelColor; aAlpha : Double);

begin
  if (aX >= 0) and (aY >= 0) and (aX < FWidth) and (aY < FHeight) then
    FPixels[aY * FWidth + aX] := SixelBlend(FPixels[aY * FWidth + aX], aColor, aAlpha);
end;


procedure TSixelBitmap.HLine(aX1, aX2, aY : Integer; const aColor : TSixelColor);

var
  X : Integer;

begin
  if (aY < 0) or (aY >= FHeight) then
    Exit;
  aX1 := Max(aX1, 0);
  aX2 := Min(aX2, FWidth - 1);
  for X := aX1 to aX2 do
    FPixels[aY * FWidth + X] := aColor;
end;


procedure TSixelBitmap.Clear(const aColor : TSixelColor);

var
  I : Integer;

begin
  for I := 0 to High(FPixels) do
    FPixels[I] := aColor;
end;


procedure TSixelBitmap.FillRect(aX1, aY1, aX2, aY2 : Integer; const aColor : TSixelColor);

var
  Y : Integer;

begin
  if aX1 > aX2 then
    SwapInt(aX1, aX2);
  if aY1 > aY2 then
    SwapInt(aY1, aY2);
  for Y := Max(aY1, 0) to Min(aY2, FHeight - 1) do
    HLine(aX1, aX2, Y, aColor);
end;


procedure TSixelBitmap.Line(aX1, aY1, aX2, aY2 : Integer; const aColor : TSixelColor);

var
  lDX, lDY, lSX, lSY, lErr, lErr2 : Integer;

begin
  lDX := Abs(aX2 - aX1);
  lDY := -Abs(aY2 - aY1);
  lSX := Sign(aX2 - aX1);
  lSY := Sign(aY2 - aY1);
  lErr := lDX + lDY;
  repeat
    SetPixel(aX1, aY1, aColor);
    if (aX1 = aX2) and (aY1 = aY2) then
      Break;
    lErr2 := 2 * lErr;
    if lErr2 >= lDY then
      begin
      Inc(lErr, lDY);
      Inc(aX1, lSX);
      end;
    if lErr2 <= lDX then
      begin
      Inc(lErr, lDX);
      Inc(aY1, lSY);
      end;
  until False;
end;


procedure TSixelBitmap.Circle(aCX, aCY, aRadius : Integer; const aColor : TSixelColor);

var
  X, Y, lErr : Integer;

begin
  X := aRadius;
  Y := 0;
  lErr := 1 - aRadius;
  while X >= Y do
    begin
    SetPixel(aCX + X, aCY + Y, aColor);
    SetPixel(aCX + Y, aCY + X, aColor);
    SetPixel(aCX - Y, aCY + X, aColor);
    SetPixel(aCX - X, aCY + Y, aColor);
    SetPixel(aCX - X, aCY - Y, aColor);
    SetPixel(aCX - Y, aCY - X, aColor);
    SetPixel(aCX + Y, aCY - X, aColor);
    SetPixel(aCX + X, aCY - Y, aColor);
    Inc(Y);
    if lErr < 0 then
      Inc(lErr, 2 * Y + 1)
    else
      begin
      Dec(X);
      Inc(lErr, 2 * (Y - X) + 1);
      end;
    end;
end;


procedure TSixelBitmap.FillCircle(aCX, aCY, aRadius : Double; const aColor : TSixelColor);

var
  X, Y : Integer;
  lDist, lCover : Double;

begin
  if aRadius <= 0 then
    Exit;
  for Y := Floor(aCY - aRadius - 1) to Ceil(aCY + aRadius + 1) do
    for X := Floor(aCX - aRadius - 1) to Ceil(aCX + aRadius + 1) do
      begin
      lDist := Sqrt(Sqr(X + 0.5 - aCX) + Sqr(Y + 0.5 - aCY));
      lCover := aRadius - lDist + 0.5;
      if lCover >= 1 then
        SetPixel(X, Y, aColor)
      else if lCover > 0 then
        BlendPixel(X, Y, aColor, lCover);
      end;
end;


procedure TSixelBitmap.FillTriangle(aX1, aY1, aX2, aY2, aX3, aY3 : Integer; const aColor : TSixelColor);

var
  X, Y, lArea, lMinX, lMaxX : Integer;
  lE1, lE2, lE3 : Int64;

begin
  lArea := (aX2 - aX1) * (aY3 - aY1) - (aY2 - aY1) * (aX3 - aX1);
  if lArea = 0 then
    begin
    Line(aX1, aY1, aX2, aY2, aColor);
    Line(aX2, aY2, aX3, aY3, aColor);
    Exit;
    end;
  if lArea < 0 then
    begin
    SwapInt(aX2, aX3);
    SwapInt(aY2, aY3);
    end;
  lMinX := Max(MinIntValue([aX1, aX2, aX3]), 0);
  lMaxX := Min(MaxIntValue([aX1, aX2, aX3]), FWidth - 1);
  for Y := Max(MinIntValue([aY1, aY2, aY3]), 0) to Min(MaxIntValue([aY1, aY2, aY3]), FHeight - 1) do
    for X := lMinX to lMaxX do
      begin
      lE1 := Int64(aX2 - aX1) * (Y - aY1) - Int64(aY2 - aY1) * (X - aX1);
      lE2 := Int64(aX3 - aX2) * (Y - aY2) - Int64(aY3 - aY2) * (X - aX2);
      lE3 := Int64(aX1 - aX3) * (Y - aY3) - Int64(aY1 - aY3) * (X - aX3);
      if (lE1 >= 0) and (lE2 >= 0) and (lE3 >= 0) then
        FPixels[Y * FWidth + X] := aColor;
      end;
end;


// Build at most aMaxColors palette entries by median cut over the visible pixels of aBitmap.
procedure BuildAdaptivePalette(aBitmap : TSixelBitmap; aMaxColors : Integer; out aPalette : TSixelPalette);

var
  lHist : array of Cardinal;
  lSumR, lSumG, lSumB : array of QWord;
  lBoxes : array of TColorBox;
  lBoxCount, I, lBest, R, G, B, lIndex : Integer;
  lScore, lBestScore : QWord;
  lColor : TSixelColor;
  lTotal, lTR, lTG, lTB : QWord;

begin
  SetLength(lHist, HistSize);
  SetLength(lSumR, HistSize);
  SetLength(lSumG, HistSize);
  SetLength(lSumB, HistSize);
  for I := 0 to aBitmap.FWidth * aBitmap.FHeight - 1 do
    begin
    lColor := aBitmap.FPixels[I];
    if aBitmap.FTransparent and SameColor(lColor, aBitmap.FTransparentColor) then
      Continue;
    lIndex := HistIndex(lColor.R shr (8 - HistBits), lColor.G shr (8 - HistBits), lColor.B shr (8 - HistBits));
    Inc(lHist[lIndex]);
    Inc(lSumR[lIndex], lColor.R);
    Inc(lSumG[lIndex], lColor.G);
    Inc(lSumB[lIndex], lColor.B);
    end;
  SetLength(lBoxes, aMaxColors);
  for I := 0 to 2 do
    begin
    lBoxes[0].Lo[I] := 0;
    lBoxes[0].Hi[I] := (1 shl HistBits) - 1;
    end;
  ShrinkBox(lHist, lBoxes[0]);
  if lBoxes[0].Count = 0 then
    begin
    SetLength(aPalette, 1);
    aPalette[0] := SixelColor(0, 0, 0);
    Exit;
    end;
  lBoxCount := 1;
  while lBoxCount < aMaxColors do
    begin
    lBest := -1;
    lBestScore := 0;
    for I := 0 to lBoxCount - 1 do
      begin
      lScore := lBoxes[I].Count * QWord(lBoxes[I].Hi[LongestAxis(lBoxes[I])] - lBoxes[I].Lo[LongestAxis(lBoxes[I])]);
      if lScore > lBestScore then
        begin
        lBestScore := lScore;
        lBest := I;
        end;
      end;
    if lBest < 0 then
      Break;
    SplitBox(lHist, lBoxes[lBest], lBoxes[lBoxCount]);
    Inc(lBoxCount);
    end;
  SetLength(aPalette, lBoxCount);
  for I := 0 to lBoxCount - 1 do
    begin
    lTotal := 0;
    lTR := 0;
    lTG := 0;
    lTB := 0;
    for R := lBoxes[I].Lo[0] to lBoxes[I].Hi[0] do
      for G := lBoxes[I].Lo[1] to lBoxes[I].Hi[1] do
        for B := lBoxes[I].Lo[2] to lBoxes[I].Hi[2] do
          begin
          lIndex := HistIndex(R, G, B);
          Inc(lTotal, lHist[lIndex]);
          Inc(lTR, lSumR[lIndex]);
          Inc(lTG, lSumG[lIndex]);
          Inc(lTB, lSumB[lIndex]);
          end;
    if lTotal > 0 then
      aPalette[I] := SixelColor(lTR div lTotal, lTG div lTotal, lTB div lTotal)
    else
      aPalette[I] := SixelColor(0, 0, 0);
    end;
end;


// Index of the palette entry closest to the given color.
function NearestColor(const aPalette : TSixelPalette; aR, aG, aB : Integer) : Integer;

var
  I : Integer;
  lDist, lBest : Integer;

begin
  Result := 0;
  lBest := MaxInt;
  for I := 0 to High(aPalette) do
    begin
    lDist := 3 * Sqr(aR - aPalette[I].R) + 4 * Sqr(aG - aPalette[I].G) + 2 * Sqr(aB - aPalette[I].B);
    if lDist < lBest then
      begin
      lBest := lDist;
      Result := I;
      if lDist = 0 then
        Break;
      end;
    end;
end;


// Map every pixel of aBitmap to a palette index; transparent pixels get TransparentIndex.
procedure MapPixels(aBitmap : TSixelBitmap; const aPalette : TSixelPalette; out aIndexes : TIndexArray);

var
  lCache : array of SmallInt;
  lErrCur, lErrNext, lErrSwap : array of Integer;
  X, Y, lPos, lKey, lIdx, R, G, B, lW, C : Integer;
  lColor : TSixelColor;
  lErr : array[0..2] of Integer;

begin
  lW := aBitmap.FWidth;
  SetLength(aIndexes, lW * aBitmap.FHeight);
  SetLength(lCache, CacheSize);
  FillChar(lCache[0], CacheSize * SizeOf(SmallInt), $FF);
  SetLength(lErrCur, (lW + 2) * 3);
  SetLength(lErrNext, (lW + 2) * 3);
  for Y := 0 to aBitmap.FHeight - 1 do
    begin
    lErrSwap := lErrCur;
    lErrCur := lErrNext;
    lErrNext := lErrSwap;
    FillChar(lErrNext[0], Length(lErrNext) * SizeOf(Integer), 0);
    for X := 0 to lW - 1 do
      begin
      lPos := Y * lW + X;
      lColor := aBitmap.FPixels[lPos];
      if aBitmap.FTransparent and SameColor(lColor, aBitmap.FTransparentColor) then
        begin
        aIndexes[lPos] := TransparentIndex;
        Continue;
        end;
      R := lColor.R;
      G := lColor.G;
      B := lColor.B;
      if aBitmap.FDither then
        begin
        R := EnsureRange(R + lErrCur[(X + 1) * 3] div 16, 0, 255);
        G := EnsureRange(G + lErrCur[(X + 1) * 3 + 1] div 16, 0, 255);
        B := EnsureRange(B + lErrCur[(X + 1) * 3 + 2] div 16, 0, 255);
        end;
      lKey := ((R shr 2) shl 12) or ((G shr 2) shl 6) or (B shr 2);
      lIdx := lCache[lKey];
      if lIdx < 0 then
        begin
        lIdx := NearestColor(aPalette, R, G, B);
        lCache[lKey] := lIdx;
        end;
      aIndexes[lPos] := lIdx;
      if aBitmap.FDither then
        begin
        lErr[0] := R - aPalette[lIdx].R;
        lErr[1] := G - aPalette[lIdx].G;
        lErr[2] := B - aPalette[lIdx].B;
        for C := 0 to 2 do
          begin
          Inc(lErrCur[(X + 2) * 3 + C], lErr[C] * 7);
          Inc(lErrNext[X * 3 + C], lErr[C] * 3);
          Inc(lErrNext[(X + 1) * 3 + C], lErr[C] * 5);
          Inc(lErrNext[(X + 2) * 3 + C], lErr[C]);
          end;
        end;
      end;
    end;
end;


// Append a run of aCount identical sixel characters to aBuffer.
procedure AddRun(var aBuffer : TSixelBuffer; aChar : AnsiChar; aCount : Integer);

begin
  if aCount > 3 then
    begin
    aBuffer.AddChar('!');
    aBuffer.AddInt(aCount);
    aBuffer.AddChar(aChar);
    end
  else
    while aCount > 0 do
      begin
      aBuffer.AddChar(aChar);
      Dec(aCount);
      end;
end;


// Percentage 0..100 of an 8-bit color component.
function Percent(aValue : Byte) : Integer; inline;

begin
  Result := (aValue * 100 + 127) div 255;
end;


function TSixelBitmap.ToSixel : string;

var
  lPalette : TSixelPalette;
  lIndexes : TIndexArray;
  lBuffer : TSixelBuffer;
  lBits : array of Byte;
  lSlotOf : array of Integer;
  lSlotColor, lSlotLast : array of Integer;
  lSlotCount, lBand, lBands, X, Y, lIdx, lSlot, lRow, lRun, lBase : Integer;
  lChar, lPrev : AnsiChar;

begin
  if (FWidth = 0) or (FHeight = 0) then
    Exit('');
  if FPaletteMode = spmFixed then
    BuildFixedPalette(FMaxColors, lPalette)
  else
    BuildAdaptivePalette(Self, FMaxColors, lPalette);
  MapPixels(Self, lPalette, lIndexes);
  lBuffer.Init(FWidth * FHeight div 2);
  lBuffer.Add(ESC + 'P0;1;0q"1;1;');
  lBuffer.AddInt(FWidth);
  lBuffer.AddChar(';');
  lBuffer.AddInt(FHeight);
  for lIdx := 0 to High(lPalette) do
    begin
    lBuffer.AddChar('#');
    lBuffer.AddInt(lIdx);
    lBuffer.Add(';2;');
    lBuffer.AddInt(Percent(lPalette[lIdx].R));
    lBuffer.AddChar(';');
    lBuffer.AddInt(Percent(lPalette[lIdx].G));
    lBuffer.AddChar(';');
    lBuffer.AddInt(Percent(lPalette[lIdx].B));
    end;
  SetLength(lBits, Length(lPalette) * FWidth);
  SetLength(lSlotOf, Length(lPalette));
  SetLength(lSlotColor, Length(lPalette));
  SetLength(lSlotLast, Length(lPalette));
  for lIdx := 0 to High(lSlotOf) do
    lSlotOf[lIdx] := -1;
  lBands := (FHeight + 5) div 6;
  for lBand := 0 to lBands - 1 do
    begin
    lSlotCount := 0;
    for lRow := 0 to 5 do
      begin
      Y := lBand * 6 + lRow;
      if Y >= FHeight then
        Break;
      for X := 0 to FWidth - 1 do
        begin
        lIdx := lIndexes[Y * FWidth + X];
        if lIdx = TransparentIndex then
          Continue;
        lSlot := lSlotOf[lIdx];
        if lSlot < 0 then
          begin
          lSlot := lSlotCount;
          Inc(lSlotCount);
          lSlotOf[lIdx] := lSlot;
          lSlotColor[lSlot] := lIdx;
          lSlotLast[lSlot] := X;
          FillChar(lBits[lSlot * FWidth], FWidth, 0);
          end;
        lBits[lSlot * FWidth + X] := lBits[lSlot * FWidth + X] or (1 shl lRow);
        if X > lSlotLast[lSlot] then
          lSlotLast[lSlot] := X;
        end;
      end;
    for lSlot := 0 to lSlotCount - 1 do
      begin
      if lSlot > 0 then
        lBuffer.AddChar('$');
      lBuffer.AddChar('#');
      lBuffer.AddInt(lSlotColor[lSlot]);
      lBase := lSlot * FWidth;
      lPrev := AnsiChar(63 + lBits[lBase]);
      lRun := 1;
      for X := 1 to lSlotLast[lSlot] do
        begin
        lChar := AnsiChar(63 + lBits[lBase + X]);
        if lChar = lPrev then
          Inc(lRun)
        else
          begin
          AddRun(lBuffer, lPrev, lRun);
          lPrev := lChar;
          lRun := 1;
          end;
        end;
      AddRun(lBuffer, lPrev, lRun);
      lSlotOf[lSlotColor[lSlot]] := -1;
      end;
    if lBand < lBands - 1 then
      lBuffer.AddChar('-');
    end;
  lBuffer.Add(ESC + '\');
  Result := string(lBuffer.Text);
end;


procedure TSixelBitmap.Draw;

begin
  TSixelTerminal.Write(ToSixel);
end;


procedure TSixelBitmap.DrawAt(aColumn, aRow : Integer);

begin
  TSixelTerminal.Write(ESC + '7' + CSI + IntToStr(Max(aRow, 1)) + ';' + IntToStr(Max(aColumn, 1)) + 'H'
                       + ToSixel + ESC + '8');
end;


finalization
  if GRawDepth > 0 then
    begin
    GRawDepth := 1;
    TSixelTerminal.LeaveRawMode;
    end;
{$IFDEF UNIX}
  if GOwnInputFD then
    fpClose(GInputFD);
{$ENDIF}
end.
