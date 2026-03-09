unit ur_newmainsrc1_bird;

{$mode objfpc}

interface

function Fly(w: word): word;

implementation

function Fly(w: word): word;
begin
  Result:=17 * w; // changed
end;

end.
