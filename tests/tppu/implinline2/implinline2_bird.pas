unit implinline2_bird;

{$mode objfpc}

interface

procedure Walk;

implementation

uses implinline2_ant;

procedure Walk; inline;
begin
  writeln(Times123(2));
end;

end.
