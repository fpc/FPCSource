unit ur_cycle2_ant;

{$mode objfpc}

interface

uses ur_cycle2_bird;

function Walk(w: word): word;

implementation

function Walk(w: word): word;
begin
  Result:=2*w;
end;

end.
