Program Example6;

{ Program to demonstrate the GetLockScreenCount function. }

Uses video,keyboard,vidutil;

Var
  I : Longint;
  S : String;

begin
  InitVideo;
  InitKeyboard;
  TextOut(1,1,'Press key till new text appears.');
  UpdateScreen(False);
  Randomize;
  For I:=0 to Random(10)+1 do
    LockScreenUpdate;
  I:=0;
  While GetLockScreenCount<>0 do
    begin
    Inc(I);
    Str(I,S);
    UnlockScreenUpdate;
    GetKeyEvent;
    TextOut(1,1,'UnLockScreenUpdate had to be called '+S+' times');
    UpdateScreen(False);
    end;
  TextOut(1,2,'Press any key to end.');
  UpdateScreen(False);
  GetKeyEvent;
  DoneKeyboard;
  DoneVideo;
end.
