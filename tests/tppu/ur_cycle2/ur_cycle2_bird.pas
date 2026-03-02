unit ur_cycle2_bird;

{$mode objfpc}

interface

function Fly(w: word): word;

implementation

uses ur_cycle2_ant;

function Fly(w: word): word;
begin
  Result:=3*Walk(w);
end;

end.
