unit WinCrt;

interface

Uses Windows;

type
  WinReadKeyRecord = record
    KeyStatus: byte;
    AsciiChar: char;
    KeyCode: word;
  end;

const

// Foreground color constants

fBlack        = 0;
fBlue         = FOREGROUND_BLUE;
fGreen        = FOREGROUND_GREEN;
fCyan         = FOREGROUND_BLUE OR FOREGROUND_GREEN;
fRed          = FOREGROUND_RED;
fMagenta      = FOREGROUND_BLUE OR FOREGROUND_RED;
fBrown        = FOREGROUND_GREEN OR FOREGROUND_RED;
fLightGray    = FOREGROUND_BLUE OR FOREGROUND_GREEN OR FOREGROUND_RED;
fDarkGray     = fBlack OR FOREGROUND_INTENSITY;
fLightBlue    = fBlue OR FOREGROUND_INTENSITY;
fLightGreen   = fGreen OR FOREGROUND_INTENSITY;
fLightCyan    = fCyan OR FOREGROUND_INTENSITY;
fLightRed     = fRed OR FOREGROUND_INTENSITY;
fLightMagenta = fMagenta OR FOREGROUND_INTENSITY;
fYellow       = fBrown OR FOREGROUND_INTENSITY;
fWhite        = fLightGray OR FOREGROUND_INTENSITY;

// Background color constants

bBlack        = 0;
bBlue         = BACKGROUND_BLUE;
bGreen        = BACKGROUND_GREEN;
bCyan         = BACKGROUND_BLUE OR BACKGROUND_GREEN;
bRed          = BACKGROUND_RED;
bMagenta      = BACKGROUND_BLUE OR BACKGROUND_RED;
bBrown        = BACKGROUND_GREEN OR BACKGROUND_RED;
bLightGray    = BACKGROUND_BLUE OR BACKGROUND_GREEN OR BACKGROUND_RED;
bDarkGray     = bBlack OR BACKGROUND_INTENSITY;
bLightBlue    = bBlue OR BACKGROUND_INTENSITY;
bLightGreen   = bGreen OR BACKGROUND_INTENSITY;
bLightCyan    = bCyan OR BACKGROUND_INTENSITY;
bLightRed     = bRed OR BACKGROUND_INTENSITY;
bLightMagenta = bMagenta OR BACKGROUND_INTENSITY;
bYellow       = bBrown OR BACKGROUND_INTENSITY;
bWhite        = bLightGray OR BACKGROUND_INTENSITY;

// Constants designating input events

NO_EVENT = 0;
KEY_EVENT_IN_PROGRESS = $100;
_MOUSE_EVENT_IN_PROGRESS = $200;

procedure ClrEol;
{ Clears all characters from cursor position to end of line without
  moving the cursor  by filling character cells with blanks
  and attribute cells with the current screen buffer attribute.
}
procedure ClrScr;
{ Clears screen buffer by filling character cells with blanks
  and attribute cells with the current screen buffer attribute.
  The cursor is positioned in the top left corner of the screen
  buffer
}
procedure FlushInputBuffer;
function GetTextBackground: byte;
function GetTextColor: byte;
Procedure GotoXY(X, Y: integer);
Procedure HighVideo;
Procedure HighVideoBackground;
Function InputEvent: word;
{ Returns
  NO_EVENT if input buffer is empty ;
  KEY_EVENT if there is a pending key event with
    key released again,
    and key is not one of the control keys;
  KEY_EVENT_IN_PROGRESS if there is another pending key event;
  _MOUSE_EVENT if there is a pending mouse event
    without moving the mouse;
  _MOUSE_EVENT_IN_PROGRESS if there is another pending mouse event;
  WINDOW_BUFFER_SIZE_EVENT is the user resized the screen buffer
    and window input is enabled (default mode disabled).
}
function KeyPressed: boolean;
{ Returns
  TRUE if there is a pending key event with
    key released again,
    and key is not one of the control keys;
  FALSE otherwise.
}
Procedure LowVideo;
Procedure LowVideoBackground;
Procedure NormVideo;
Procedure NormVideoBackground;
Function ReadKey: char;
Procedure TextBackground (Color: Byte);
Procedure TextColor (Color: Byte);
Function WhereX: integer;
Function WhereY: integer;
Function WinReadKey: WinReadKeyRecord;
{ Return value in KeyStatus element:
  - bit  0: shift key pressed
  - bit  1: ctrl key pressed
  - bit  2: alt key pressed
  The KeyCode element has the virtual key code.

  N.B. nog regelen: extended ASCII via Alt-keypad toetsen.
}

implementation

type
  PInputBuffer = ^TInputBuffer;
  TInputBuffer = array[word] of TInputRecord;

var
  StartTextIntensity, StartBackgroundIntensity: byte;
  pCsbi: PConsoleScreenBufferInfo;

function GetScreenInfo: TConsoleScreenBufferInfo; forward;
Function RemapScanCode (ScanCode: byte; CtrlKeyState: byte): byte; forward;

procedure ClrEol;
var
  hConsoleOutput: THandle;
  cCharacter: Char;
  wAttribute: word;
  nLength: dword;
  dwWriteCoord: TCoord;
  lpWritten: dword;
begin
  hConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  cCharacter := ' ';
  New(pCsbi);
  GetConsoleScreenBufferInfo(hConsoleOutput, pCsbi^);
  wAttribute := pCsbi^.wAttributes;
  nLength := pCsbi^.dwSize.X - pCsbi^.dwCursorPosition.X + 1;
  dwWriteCoord.X := pCsbi^.dwCursorPosition.X;
  dwWriteCoord.Y := pCsbi^.dwCursorPosition.Y;
  Dispose(pCsbi);
  FillConsoleOutputCharacter(hConsoleOutput, cCharacter, nLength,
    dwWriteCoord, lpWritten);
  FillConsoleOutputAttribute(hConsoleOutput, wAttribute, nLength,
    dwWriteCoord, lpWritten);
end;

procedure ClrScr;
var
  hConsoleOutput: THandle;
  cCharacter: Char;
  wAttribute: word;
  nLength: dword;
  dwWriteCoord: TCoord;
  lpWritten: dword;
begin
  hConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  cCharacter := ' ';
  New(pCsbi);
  GetConsoleScreenBufferInfo(hConsoleOutput, pCsbi^);
  wAttribute := pCsbi^.wAttributes;
  nLength := pCsbi^.dwSize.X * pCsbi^.dwSize.Y;
  Dispose(pCsbi);
  dwWriteCoord.X := 0;
  dwWriteCoord.Y := 0;
  FillConsoleOutputCharacter(hConsoleOutput, cCharacter, nLength,
    dwWriteCoord, lpWritten);
  FillConsoleOutputAttribute(hConsoleOutput, wAttribute, nLength,
    dwWriteCoord, lpWritten);
  SetConsoleCursorPosition(hConsoleOutput, dwWriteCoord);
end;

procedure FlushInputBuffer;
begin
  FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
end;

function GetTextBackground: byte;
begin
  Result := GetScreenInfo.wAttributes AND bWhite;
end;

function GetTextColor: byte;
begin
  Result := GetScreenInfo.wAttributes AND fWhite;
end;

function GetScreenInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), Result);
end;

Procedure GotoXY(X, Y: integer);
var
  CoordCursor: TCoord;
begin
  CoordCursor.X := X - 1;
  CoordCursor.Y := Y - 1;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), CoordCursor);
end;

Procedure HighVideo;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute OR FOREGROUND_INTENSITY);
end;

Procedure HighVideoBackground;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute OR BACKGROUND_INTENSITY);
end;

Function InputEvent: word;
var
  hConsoleInput: THandle;
  pInput: pInputBuffer;
  lpNumberOfEvents: dword;
  lpNumberRead: integer;
  i: word;

  const
  KeysToSkip: set of byte =
    [VK_SHIFT, VK_CONTROL, VK_MENU, VK_CAPITAL, VK_NUMLOCK, VK_SCROLL];

begin
  hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
  GetNumberOfConsoleInputEvents(hConsoleInput, lpNumberOfEvents);
  Result := NO_EVENT;
  if lpNumberOfEvents > 0 then
  try
    GetMem(pInput, lpNumberOfEvents * SizeOf(TInputRecord));
    PeekConsoleInput(hConsoleInput, pInput^[0], lpNumberOfEvents, lpNumberRead);
    i := 0;
    repeat
      with pInput^[i] do begin
        case EventType of
          KEY_EVENT:
            if (KeyEvent.bKeyDown = false) and
               not (KeyEvent.wVirtualKeyCode in KeysToSkip) then
               Result := EventType
             else
               Result := KEY_EVENT_IN_PROGRESS;
          _MOUSE_EVENT:
            if (MouseEvent.dwEventFlags <> MOUSE_MOVED) then
               Result := EventType
             else
               Result := _MOUSE_EVENT_IN_PROGRESS;
          else
            Result := EventType;
        end;
      end;
      inc(i);
    until (Result <> NO_EVENT) or (i >= lpNumberOfEvents);
  finally
    FreeMem(pInput);
  end;
end;

Function KeyPressed: boolean;
var
  hConsoleInput: THandle;
  pInput: pInputBuffer;
  lpNumberOfEvents: dword;
  lpNumberRead: integer;
  i: word;

  const
  KeysToSkip: set of byte =
    [VK_SHIFT, VK_CONTROL, VK_MENU, VK_CAPITAL, VK_NUMLOCK, VK_SCROLL];

begin
  hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
  GetNumberOfConsoleInputEvents(hConsoleInput, lpNumberOfEvents);
  Result := FALSE;
  if lpNumberOfEvents > 0 then
  try
    GetMem(pInput, lpNumberOfEvents * SizeOf(TInputRecord));
    PeekConsoleInput(hConsoleInput, pInput^[0], lpNumberOfEvents, lpNumberRead);
    i := 0;
    repeat
      with pInput^[i] do begin
        if EventType = KEY_EVENT then
          Result := (KeyEvent.bKeyDown = false) and
                    not (KeyEvent.wVirtualKeyCode in KeysToSkip);
      end;
      inc(i);
    until (Result = TRUE) or (i >= lpNumberOfEvents);
  finally
    FreeMem(pInput);
  end;
end;

Procedure LowVideo;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute AND NOT FOREGROUND_INTENSITY);
end;

Procedure LowVideoBackground;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute AND NOT BACKGROUND_INTENSITY);
end;
Procedure NormVideo;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute AND (fLightGray OR bWhite) OR StartTextIntensity);
end;

Procedure NormVideoBackground;
var
  Attribute: word;
begin
  Attribute := GetScreenInfo.wAttributes;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Attribute AND (fWhite OR bLightGray) OR StartBackgroundIntensity);
end;

Function ReadKey: char;
var
  hConsoleInput: THandle;
  pInput: pInputRecord;
  lpcRead: integer;
  AltKey, CtrlKey, ShiftKey: boolean;

const
  ExtendedChar: boolean = false;
  Scancode: byte = 0;
  {
    Scancodes to skip:

      $1D - Ctrl keys
      $2A - left Shift key
      $36 - right Shift key
      $38 - Alt keys
      $3A - Caps lock key
      $45 - Num lock key
      $46 - Scroll lock key
  }
  ScanCodesToSkip: set of 0..255 =
    [$1D, $2A, $36, $38, $3A, $45, $46];

begin
  if not ExtendedChar then begin
    hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
    try
      New(pInput);
      with pInput^.KeyEvent do begin
        Repeat
          ReadConsoleInput(hConsoleInput, pInput^, 1, lpcRead);
        until (pInput^.EventType = KEY_EVENT)
          and (bKeyDown = false)
          and not (wVirtualScanCode in ScanCodesToSkip);

        { Get state of control keys }

        AltKey := ((dwControlKeyState AND
                  (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
        CtrlKey := ((dwControlKeyState AND
                  (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
        ShiftKey := ((dwControlKeyState AND SHIFT_PRESSED) > 0);

        { Get key value, making some corrections to comply with MSDOS}

        if AltKey then
          Result := #0
        else begin
          Result := AsciiChar;
          if CtrlKey then
            case wVirtualScanCode of
              $07: Result := #$1E;    // ^_6  (Win32 gives ASCII = 0)
              $0C: Result := #$1F;    // ^_-  (Win32 gives ASCII = 0)
            end
          else if ShiftKey then
            case wVirtualScanCode of
              $01: Result := #$1B;    // Shift Esc (Win32 gives ASCII = 0)
              $0F: Result := #0;      // Shift Tab (Win32 gives ASCII = 9)
            end;
        end;

        {Save scancode of non-ASCII keys for second call}

        if (Result = #0) then begin
          ExtendedChar := true;
          ScanCode := RemapScanCode(wVirtualScanCode, dwControlKeyState);
        end;
      end;
    finally
      Dispose(pInput);
    end;
  end
  else begin
    Result := char(ScanCode);
    ExtendedChar := false;
  end;
end;

Function RemapScanCode (ScanCode: byte; CtrlKeyState: byte): byte;

  { Several remappings of scancodes are necessary to comply with what
    we get with MSDOS. Special Windows keys, as Alt-Tab, Ctrl-Esc etc.
    are excluded }

var
  AltKey, CtrlKey, ShiftKey: boolean;
const
  {
    Keypad key scancodes:

      Ctrl Norm

      $77  $47 - Home
      $8D  $48 - Up arrow
      $84  $49 - PgUp
      $8E  $4A - -
      $73  $4B - Left Arrow
      $8F  $4C - 5
      $74  $4D - Right arrow
      $4E  $4E - +
      $75  $4F - End
      $91  $50 - Down arrow
      $76  $51 - PgDn
      $92  $52 - Ins
      $93  $53 - Del
  }
  CtrlKeypadKeys: array[$47..$53] of byte =
    ($77, $8D, $84, $8E, $73, $8F, $74, $4E, $75, $91, $76, $92, $93);

begin
  AltKey := ((CtrlKeyState AND
            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
  CtrlKey := ((CtrlKeyState AND
            (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
  ShiftKey := ((CtrlKeyState AND SHIFT_PRESSED) > 0);
  if AltKey then
    case ScanCode of
    // Digits, -, =
    $02..$0D: inc(ScanCode, $76);
    // Function keys
    $3B..$44: inc(Scancode, $2D);
    $57..$58: inc(Scancode, $34);
    // Extended cursor block keys
    $47..$49, $4B, $4D, $4F..$53:
              inc(Scancode, $50);
    // Other keys
    $1C:      Scancode := $A6;   // Enter
    $35:      Scancode := $A4;   // / (keypad and normal!)
    end
  else if CtrlKey then
    case Scancode of
    // Tab key
    $0F:      Scancode := $94;
    // Function keys
    $3B..$44: inc(Scancode, $23);
    $57..$58: inc(Scancode, $32);
    // Keypad keys
    $35:      Scancode := $95;   // \
    $37:      Scancode := $96;   // *
    $47..$53: Scancode := CtrlKeypadKeys[Scancode];
    end
  else if ShiftKey then
    case Scancode of
    // Function keys
    $3B..$44: inc(Scancode, $19);
    $57..$58: inc(Scancode, $30);
    end
  else
    case Scancode of
    // Function keys
    $57..$58: inc(Scancode, $2E); // F11 and F12
  end;
  Result := ScanCode;
end;


Procedure TextBackground (Color: Byte);
var
  Background, Foreground: byte;
begin
  Background := Color AND bWhite;
  Foreground := GetScreenInfo.wAttributes AND fWhite;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Background OR Foreground);
end;

Procedure TextColor (Color: Byte);
var
  Background, Foreground: byte;
begin
  Background := GetScreenInfo.wAttributes AND bWhite;
  Foreground := Color AND fWhite;
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    Background OR Foreground);
end;

Function WhereX: integer;
begin
  Result := GetScreenInfo.dwCursorPosition.X + 1;
end;

Function WhereY: integer;
begin
  Result := GetScreenInfo.dwCursorPosition.Y + 1;
end;

Function WinReadKey: WinReadKeyRecord;
var
  hConsoleInput: THandle;
  pInput: pInputRecord;
  lpcRead: integer;

  const
  KeysToSkip: set of byte =
    [VK_SHIFT, VK_CONTROL, VK_MENU, VK_CAPITAL, VK_NUMLOCK, VK_SCROLL];

begin
  hConsoleInput := GetStdHandle(STD_INPUT_HANDLE);
  try
    New(pInput);
    with pInput^.KeyEvent do begin
      Repeat
        ReadConsoleInput(hConsoleInput, pInput^, 1, lpcRead);
      until (pInput^.EventType = KEY_EVENT)
        and (bKeyDown = TRUE)
        and not (wVirtualKeyCode in KeysToSkip);

      { Get key value }

      with Result do begin

        KeyStatus := 0;
        AsciiChar := pInput^.KeyEvent.AsciiChar;
        KeyCode := wVirtualKeyCode;

      { Set bits 0..2 of KeyStatus to indicate control key state}

      if ((dwControlKeyState AND SHIFT_PRESSED) > 0) then
        KeyStatus := (KeyStatus OR $01);
      if ((dwControlKeyState AND
          (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0) then
        KeyStatus := (KeyStatus OR $02);
      if ((dwControlKeyState AND
          (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0) then
        KeyStatus := (KeyStatus OR $04);

      end;
    end;
  finally
    Dispose(pInput);
  end;
end;

begin
  New(pCsbi);
  pCsbi^ := GetScreenInfo;
  StartTextIntensity := pCsbi^.wAttributes AND FOREGROUND_INTENSITY;
  StartBackgroundIntensity := pCsbi^.wAttributes AND BACKGROUND_INTENSITY;
  Dispose(pCsbi);
end.
