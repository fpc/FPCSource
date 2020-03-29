{
 Keyboard example for the PTCPas library
 This source code is in the public domain
}

program KeyboardExample3;

{$MODE objfpc}

uses
  ptc;

function KeyCode2String(ACode: Integer): string;
begin
  case ACode of
    PTCKEY_UNDEFINED    : Result := 'PTCKEY_UNDEFINED';
    PTCKEY_CANCEL       : Result := 'PTCKEY_CANCEL';
    PTCKEY_BACKSPACE    : Result := 'PTCKEY_BACKSPACE';
    PTCKEY_TAB          : Result := 'PTCKEY_TAB';
    PTCKEY_ENTER        : Result := 'PTCKEY_ENTER';
    PTCKEY_CLEAR        : Result := 'PTCKEY_CLEAR';
    PTCKEY_SHIFT        : Result := 'PTCKEY_SHIFT';
    PTCKEY_CONTROL      : Result := 'PTCKEY_CONTROL';
    PTCKEY_ALT          : Result := 'PTCKEY_ALT';
    PTCKEY_PAUSE        : Result := 'PTCKEY_PAUSE';
    PTCKEY_CAPSLOCK     : Result := 'PTCKEY_CAPSLOCK';
    PTCKEY_KANA         : Result := 'PTCKEY_KANA';
    PTCKEY_FINAL        : Result := 'PTCKEY_FINAL';
    PTCKEY_KANJI        : Result := 'PTCKEY_KANJI';
    PTCKEY_ESCAPE       : Result := 'PTCKEY_ESCAPE';
    PTCKEY_CONVERT      : Result := 'PTCKEY_CONVERT';
    PTCKEY_NONCONVERT   : Result := 'PTCKEY_NONCONVERT';
    PTCKEY_ACCEPT       : Result := 'PTCKEY_ACCEPT';
    PTCKEY_MODECHANGE   : Result := 'PTCKEY_MODECHANGE';
    PTCKEY_SPACE        : Result := 'PTCKEY_SPACE';
    PTCKEY_PAGEUP       : Result := 'PTCKEY_PAGEUP';
    PTCKEY_PAGEDOWN     : Result := 'PTCKEY_PAGEDOWN';
    PTCKEY_END          : Result := 'PTCKEY_END';
    PTCKEY_HOME         : Result := 'PTCKEY_HOME';
    PTCKEY_LEFT         : Result := 'PTCKEY_LEFT';
    PTCKEY_UP           : Result := 'PTCKEY_UP';
    PTCKEY_RIGHT        : Result := 'PTCKEY_RIGHT';
    PTCKEY_DOWN         : Result := 'PTCKEY_DOWN';
    PTCKEY_COMMA        : Result := 'PTCKEY_COMMA';
    PTCKEY_PERIOD       : Result := 'PTCKEY_PERIOD';
    PTCKEY_SLASH        : Result := 'PTCKEY_SLASH';
    PTCKEY_ZERO         : Result := 'PTCKEY_ZERO';
    PTCKEY_ONE          : Result := 'PTCKEY_ONE';
    PTCKEY_TWO          : Result := 'PTCKEY_TWO';
    PTCKEY_THREE        : Result := 'PTCKEY_THREE';
    PTCKEY_FOUR         : Result := 'PTCKEY_FOUR';
    PTCKEY_FIVE         : Result := 'PTCKEY_FIVE';
    PTCKEY_SIX          : Result := 'PTCKEY_SIX';
    PTCKEY_SEVEN        : Result := 'PTCKEY_SEVEN';
    PTCKEY_EIGHT        : Result := 'PTCKEY_EIGHT';
    PTCKEY_NINE         : Result := 'PTCKEY_NINE';
    PTCKEY_SEMICOLON    : Result := 'PTCKEY_SEMICOLON';
    PTCKEY_EQUALS       : Result := 'PTCKEY_EQUALS';
    PTCKEY_A            : Result := 'PTCKEY_A';
    PTCKEY_B            : Result := 'PTCKEY_B';
    PTCKEY_C            : Result := 'PTCKEY_C';
    PTCKEY_D            : Result := 'PTCKEY_D';
    PTCKEY_E            : Result := 'PTCKEY_E';
    PTCKEY_F            : Result := 'PTCKEY_F';
    PTCKEY_G            : Result := 'PTCKEY_G';
    PTCKEY_H            : Result := 'PTCKEY_H';
    PTCKEY_I            : Result := 'PTCKEY_I';
    PTCKEY_J            : Result := 'PTCKEY_J';
    PTCKEY_K            : Result := 'PTCKEY_K';
    PTCKEY_L            : Result := 'PTCKEY_L';
    PTCKEY_M            : Result := 'PTCKEY_M';
    PTCKEY_N            : Result := 'PTCKEY_N';
    PTCKEY_O            : Result := 'PTCKEY_O';
    PTCKEY_P            : Result := 'PTCKEY_P';
    PTCKEY_Q            : Result := 'PTCKEY_Q';
    PTCKEY_R            : Result := 'PTCKEY_R';
    PTCKEY_S            : Result := 'PTCKEY_S';
    PTCKEY_T            : Result := 'PTCKEY_T';
    PTCKEY_U            : Result := 'PTCKEY_U';
    PTCKEY_V            : Result := 'PTCKEY_V';
    PTCKEY_W            : Result := 'PTCKEY_W';
    PTCKEY_X            : Result := 'PTCKEY_X';
    PTCKEY_Y            : Result := 'PTCKEY_Y';
    PTCKEY_Z            : Result := 'PTCKEY_Z';
    PTCKEY_OPENBRACKET  : Result := 'PTCKEY_OPENBRACKET';
    PTCKEY_BACKSLASH    : Result := 'PTCKEY_BACKSLASH';
    PTCKEY_CLOSEBRACKET : Result := 'PTCKEY_CLOSEBRACKET';
    PTCKEY_NUMPAD0      : Result := 'PTCKEY_NUMPAD0';
    PTCKEY_NUMPAD1      : Result := 'PTCKEY_NUMPAD1';
    PTCKEY_NUMPAD2      : Result := 'PTCKEY_NUMPAD2';
    PTCKEY_NUMPAD3      : Result := 'PTCKEY_NUMPAD3';
    PTCKEY_NUMPAD4      : Result := 'PTCKEY_NUMPAD4';
    PTCKEY_NUMPAD5      : Result := 'PTCKEY_NUMPAD5';
    PTCKEY_NUMPAD6      : Result := 'PTCKEY_NUMPAD6';
    PTCKEY_NUMPAD7      : Result := 'PTCKEY_NUMPAD7';
    PTCKEY_NUMPAD8      : Result := 'PTCKEY_NUMPAD8';
    PTCKEY_NUMPAD9      : Result := 'PTCKEY_NUMPAD9';
    PTCKEY_MULTIPLY     : Result := 'PTCKEY_MULTIPLY';
    PTCKEY_ADD          : Result := 'PTCKEY_ADD';
    PTCKEY_SEPARATOR    : Result := 'PTCKEY_SEPARATOR';
    PTCKEY_SUBTRACT     : Result := 'PTCKEY_SUBTRACT';
    PTCKEY_DECIMAL      : Result := 'PTCKEY_DECIMAL';
    PTCKEY_DIVIDE       : Result := 'PTCKEY_DIVIDE';
    PTCKEY_F1           : Result := 'PTCKEY_F1';
    PTCKEY_F2           : Result := 'PTCKEY_F2';
    PTCKEY_F3           : Result := 'PTCKEY_F3';
    PTCKEY_F4           : Result := 'PTCKEY_F4';
    PTCKEY_F5           : Result := 'PTCKEY_F5';
    PTCKEY_F6           : Result := 'PTCKEY_F6';
    PTCKEY_F7           : Result := 'PTCKEY_F7';
    PTCKEY_F8           : Result := 'PTCKEY_F8';
    PTCKEY_F9           : Result := 'PTCKEY_F9';
    PTCKEY_F10          : Result := 'PTCKEY_F10';
    PTCKEY_F11          : Result := 'PTCKEY_F11';
    PTCKEY_F12          : Result := 'PTCKEY_F12';
    PTCKEY_DELETE       : Result := 'PTCKEY_DELETE';
    PTCKEY_NUMLOCK      : Result := 'PTCKEY_NUMLOCK';
    PTCKEY_SCROLLLOCK   : Result := 'PTCKEY_SCROLLLOCK';
    PTCKEY_PRINTSCREEN  : Result := 'PTCKEY_PRINTSCREEN';
    PTCKEY_INSERT       : Result := 'PTCKEY_INSERT';
    PTCKEY_HELP         : Result := 'PTCKEY_HELP';
    PTCKEY_META         : Result := 'PTCKEY_META';
    PTCKEY_MINUS        : Result := 'PTCKEY_MINUS';
    PTCKEY_BACKQUOTE    : Result := 'PTCKEY_BACKQUOTE';
    PTCKEY_QUOTE        : Result := 'PTCKEY_QUOTE';
    else
      Result := '';
  end;
end;

procedure DumpKey(AKey: IPTCKeyEvent);
var
  mk: TPTCModifierKey;
  first: Boolean;
begin
  Write('Code=', AKey.Code:3, ' (', KeyCode2String(AKey.Code):19,
    '), Unicode=$', HexStr(AKey.Unicode, 4), ', Press=', AKey.Press:5,
    ', Shift=', AKey.Shift:5, ', Alt=', AKey.Alt:5, ', Control=',
    AKey.Control:5, ', ModifierKeys=[');
  first := True;
  for mk in TPTCModifierKey do
    if mk in AKey.ModifierKeys then
    begin
      if not first then
        Write(',');
      first := False;
      Write(mk);
    end;
  Writeln(']');
end;

var
  console: IPTCConsole;
  format: IPTCFormat;
  key: IPTCKeyEvent;
  Done: Boolean;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { enable key release events }
      console.KeyReleaseEnabled := True;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Keyboard example 3', format);

      { main loop }
      Done := False;
      repeat
        { check for key press/release }
        while console.KeyPressed do
        begin
          console.ReadKey(key);
          case key.code of
            PTCKEY_ESCAPE:
              begin
                Done := True;
                Break;
              end;
            else
              DumpKey(key);
          end;
        end;

        { update console }
        console.update;
      until Done;
    finally
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
