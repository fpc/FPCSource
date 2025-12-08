unit testcib_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

function Fly(w : word): word; inline;
begin
  Result:= 13 * w;
end;

end.
