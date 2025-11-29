unit changeleaf1_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

function Fly(w : word): word;
begin
  Result:=w*7;
end;

end.
