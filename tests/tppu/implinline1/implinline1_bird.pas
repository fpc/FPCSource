unit implinline1_bird;

{$mode objfpc}

interface

procedure Walk;

implementation

uses implinline1_ant;

procedure Walk; inline;
begin
  writeln(Times123(2));
end;

end.
