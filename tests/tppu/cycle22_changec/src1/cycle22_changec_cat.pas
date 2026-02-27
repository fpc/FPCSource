unit cycle22_changec_cat;

{$mode objfpc}

interface

function Jump(w : word): word;

implementation

uses cycle22_changec_bird;

function Jump(w : word): word;
begin
  Result:=w*7*Factor;
end;

end.
