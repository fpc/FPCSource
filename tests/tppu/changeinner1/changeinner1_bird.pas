unit changeinner1_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

function Fly(w : word): word;
begin
  Result := 5*w; // changed
end;

end.
