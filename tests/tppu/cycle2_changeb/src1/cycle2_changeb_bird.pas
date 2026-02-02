unit cycle2_changeb_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

uses cycle2_changeb_ant;

function Fly(w : word): word;
begin
  Result:=w*7*Factor;
end;

end.
