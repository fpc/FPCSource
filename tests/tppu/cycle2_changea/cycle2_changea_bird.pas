unit cycle2_changea_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

uses cycle2_changea_ant;

function Fly(w : word): word;
begin
  Result:=w*7*Factor;
end;

end.
