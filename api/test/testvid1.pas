uses
  Video, Keyboard;

procedure FillScreen(W: Word);
var
  I: Integer;
  P: PVideoCell;
  Mode: TVideoMode;
begin
  GetVideoMode(Mode);
  P := PVideoCell(VideoBuf);
  for I := 0 to Mode.Row * Mode.Col do begin
    P^ := W;
    Inc(P);
  end;
  UpdateScreen(True);
end;

var
  Mode: TVideoMode;

begin
  { Video automatically determines the dimensions, so you may want to
    add "magic" numbers here to identify a given video mode }
  {$IFDEF FPC}
    RegisterVideoMode($FF, $FF, True, @DefaultVideoModeSelector, $01094F02);
  {$ELSE}
    RegisterVideoMode($FF, $FF, True, DefaultVideoModeSelector, $01094F02);
  {$ENDIF}
  InitVideo;
  FillScreen($1FB0);
  readln;

  Mode.Col := $FF; Mode.Row := $FF; Mode.Color := True;
  SetVideoMode(Mode);
  FillScreen($1FB0);
  WriteLn('ScreenWidth = ', ScreenWidth);
  WriteLn('ScreenHeight = ', ScreenHeight);
  readln;
  Mode.Row := 25;
  SetVideoMode(Mode);
  DoneVideo;
end.
