program testvideo;

uses video,keyboard,vidutil;

Var
  i : longint;
  k : TkeyEvent;

begin
  InitVideo;
  InitKeyboard;
  For I:=1 to 10 do
    TextOut(i,i, 'Press any key to end');
  UpdateScreen(False);
  K:=GetKeyEvent;
  DoneKeyBoard;
  DoneVideo;
end.  