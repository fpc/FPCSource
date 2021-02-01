program video2;

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
          Attribute := $07;
          ExtendedGraphemeCluster := WideChar(X + Y);
        end;
    UpdateScreen(False);

    k := GetKeyEvent;
    k := TranslateKeyEvent(k);
  until GetKeyEventChar(k) = 'q';
  DoneEnhancedVideo;
  DoneKeyboard;
end.

