{ %NORUN }

program tw37286;

{$mode objfpc}

var preShiftWorldDx: LongInt;
  WorldDx: LongInt;
  playWidth: LongInt;

procedure ShiftWorld(Dir: LongInt); inline;
begin
    preShiftWorldDx:= WorldDx;
    WorldDx:= WorldDx + LongInt(Dir * LongInt(playWidth));

end;

begin
  ShiftWorld(-1);
end.
