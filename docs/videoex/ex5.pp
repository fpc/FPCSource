Program Example5;

{ Program to demonstrate the GetCursorType function. }

Uses video,keyboard,vidutil;

Const
  Cursortypes : Array[crHidden..crHalfBlock] of string =
    ('Hidden','UnderLine','Block','HalfBlock');

begin
  InitVideo;
  InitKeyboard;
  TextOut(1,1,'Cursor type: '+CursorTypes[GetCursorType]);
  TextOut(1,2,'Press any key to exit.');
  UpdateScreen(False);
  GetKeyEvent;
  DoneKeyboard;
  DoneVideo;
end.
