unit cycle2_changeb_crc_bird;

{$mode objfpc}

interface

generic function Fly<T>(w : T): T;

procedure Flap;

implementation

uses cycle2_changeb_crc_ant;

generic function Fly<T>(w : T): T;
begin
  Result:=sizeof(w) * 5;
end;

procedure Flap;
begin
  writeln('Flap');
end;

end.
