uses
  Common, Video;

var
  I, J: CPUInt;
  Direction: CPUWord;

begin
  Randomize;
  InitVideo;
  I := 1; J := 1;
  Direction := Random(8);
  repeat
    VideoBuf^[I+J*ScreenWidth] := $0720;
    case Direction of
      0: Dec(J);
      1: Inc(I);
      2: Inc(J);
      3: Dec(I);
      4:
        begin
          Inc(I);
          Dec(J);
        end;
      5:
        begin
          Inc(I);
          Inc(J);
        end;
      6:
        begin
          Dec(I);
          Inc(J);
        end;
      7:
        begin
          Dec(I);
          Dec(J);
        end;
    end;
    if (I < 0) then I := 0;
    if (J < 0) then J := 0;
    if (I >= ScreenWidth) then I := ScreenWidth-1;
    if (J >= ScreenHeight) then J := ScreenHeight-1;
    VideoBuf^[I+J*ScreenWidth] := $1F2A;
    if Random(100) < 30 then Direction := Random(8);
    UpdateScreen(False);
  until False; {KeyPressed;}
  DoneVideo;
end.
