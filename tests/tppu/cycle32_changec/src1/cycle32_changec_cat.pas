unit cycle32_changec_cat;

{$mode objfpc}

interface

function Jump(w : word): word;

implementation

uses cycle32_changec_bird, cycle32_changec_ant;

function Jump(w : word): word;
begin
  Result:=w*7*Factor;
end;

end.
