unit inline1_bird;

{$mode objfpc}

interface

uses inline1_cat;

function Fly(w: word): word;

implementation

function Fly(w : word): word;
begin
  Result:=2*Jump(w);
end;

end.
