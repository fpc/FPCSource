program video1;

uses
  video, keyboard;

var
  k: TKeyEvent;
  X, Y: Integer;
begin
  InitKeyboard;
  InitVideo;
  repeat
    for X := 0 to ScreenWidth - 1 do
      for Y := 0 to ScreenHeight - 1 do
        VideoBuf^[Y * ScreenWidth + X] := ((X + Y) mod 256) or $0700;
    UpdateScreen(False);

    k := GetKeyEvent;
    k := TranslateKeyEvent(k);
  until GetKeyEventChar(k) = 'q';
  DoneVideo;
  DoneKeyboard;
end.

