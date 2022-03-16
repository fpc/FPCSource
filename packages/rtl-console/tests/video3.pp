{ test for the 256-color support }
program video3;

uses
  video, keyboard;

var
  k: TKeyEvent;
  X, Y: Integer;
begin
  InitKeyboard;
  InitEnhancedVideo;
  repeat
    for X := 0 to ScreenWidth - 1 do
      for Y := 0 to ScreenHeight - 1 do
        with EnhancedVideoBuf[Y * ScreenWidth + X] do
        begin
          ForegroundColor := Byte(X + Y);
          BackgroundColor := ForegroundColor xor 255;
          ExtendedGraphemeCluster := 'A';
        end;
    UpdateScreen(False);

    k := GetKeyEvent;
    k := TranslateKeyEvent(k);
  until GetKeyEventChar(k) = 'q';
  DoneEnhancedVideo;
  DoneKeyboard;
end.

