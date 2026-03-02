unit inline1_cat;

{$mode objfpc}

interface

function Jump(w: word): word; inline;

implementation

function Jump(w : word): word;
begin
  Result:=w * 7; // changed inline implementation
end;

end.
