unit inline1_cat;

{$mode objfpc}

interface

function Jump(w: word): word; inline;

implementation

function Jump(w : word): word;
begin
  Result:=13*w;
end;

end.
