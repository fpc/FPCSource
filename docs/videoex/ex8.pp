Program Example8;

{ Program to demonstrate the GetVideoModeCount function. }

Uses video,keyboard,vidutil;

Procedure DumpMode (M : TVideoMode; Index : Integer);

Var
 S : String;

begin
  Str(Index:2,S);
  inc(Index);
  TextOut(1,Index,'Data for mode '+S+': ');
  if M.Color then
    TextOut(19,Index,'   color,')
  else
    TextOut(19,Index,'No color,');
  Str(M.Row:3,S);
  TextOut(28,Index,S+' rows');
  Str(M.Col:3,S);
  TextOut(36,index,S+' columns');
end;

Var
  i,Count : Integer;
  m : TVideoMode;

begin
  InitVideo;
  InitKeyboard;
  Count:=GetVideoModeCount;
  For I:=1 to Count do
    begin
    GetVideoModeData(I-1,M);
    DumpMode(M,I-1);
    end;
  TextOut(1,Count+1,'Press any key to exit');
  UpdateScreen(False);
  GetKeyEvent;
  DoneKeyboard;
  DoneVideo;
end.
