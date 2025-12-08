unit testcib_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

function Fly(w : word): word; inline;
begin
  Result:= 3 * w;
end;

end.
