{ Old file: tbs0329.pp }
{  }

{$packrecords c}

type
     SHORT=smallint;
     WINBOOL = longbool;
     WCHAR=word;
     UINT=cardinal;

     COORD = record
          X : SHORT;
          Y : SHORT;
       end;

     KEY_EVENT_RECORD = packed record
          bKeyDown : WINBOOL;
          wRepeatCount : WORD;
          wVirtualKeyCode : WORD;
          wVirtualScanCode : WORD;
          case longint of
             0 : ( UnicodeChar : WCHAR;
                   dwControlKeyState : DWORD; );
             1 : ( AsciiChar : CHAR );
       end;

     MOUSE_EVENT_RECORD = record
          dwMousePosition : COORD;
          dwButtonState : DWORD;
          dwControlKeyState : DWORD;
          dwEventFlags : DWORD;
       end;

     WINDOW_BUFFER_SIZE_RECORD = record
          dwSize : COORD;
       end;

     MENU_EVENT_RECORD = record
          dwCommandId : UINT;
       end;

     FOCUS_EVENT_RECORD = record
          bSetFocus : WINBOOL;
       end;

     INPUT_RECORD = record
          EventType : WORD;
              case longint of
                 0 : ( KeyEvent : KEY_EVENT_RECORD );
                 1 : ( MouseEvent : MOUSE_EVENT_RECORD );
                 2 : ( WindowBufferSizeEvent : WINDOW_BUFFER_SIZE_RECORD );
                 3 : ( MenuEvent : MENU_EVENT_RECORD );
                 4 : ( FocusEvent : FOCUS_EVENT_RECORD );
       end;

const
{$ifdef cpu68k}
  { GNU C only aligns at word boundaries
    for m68k cpu PM }
  correct_size = 18;
{$else }
  correct_size = 20;
{$endif }
begin
  if sizeof(INPUT_RECORD)<>correct_size then
   begin
     writeln('Wrong packing for Packrecords C and union ',sizeof(INPUT_RECORD),' instead of ',correct_size);
     halt(1);
   end;
end.
