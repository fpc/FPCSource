Program Example7;

{ Program to demonstrate the GetVideoMode function. }

Uses video,keyboard,vidutil;

Var
  M : TVideoMode;
  S : String;

begin
  InitVideo;
  InitKeyboard;
  GetVideoMode(M);
  if M.Color then
    TextOut(1,1,'Current mode has color')
  else
    TextOut(1,1,'Current mode does not have color');
  Str(M.Row,S);
  TextOut(1,2,'Number of rows    : '+S);
  Str(M.Col,S);
  TextOut(1,3,'Number of columns : '+S);
  Textout(1,4,'Press any key to exit.');
  UpdateScreen(False);
  GetKeyEvent;
  DoneKeyboard;
  DoneVideo;
end.
