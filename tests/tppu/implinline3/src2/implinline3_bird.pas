unit implinline3_bird;

{$mode objfpc}

interface

procedure Walk;

implementation

uses implinline3_ant;

procedure Walk; inline;
begin
  writeln(Times123(2));
end;

end.
