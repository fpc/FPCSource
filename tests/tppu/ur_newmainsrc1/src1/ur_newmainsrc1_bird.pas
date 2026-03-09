unit ur_newmainsrc1_bird;

{$mode objfpc}

interface

function Fly(w: word): word;

implementation

function Fly(w: word): word;
begin
  Result:=w*11;
end;

end.
