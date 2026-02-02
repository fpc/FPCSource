unit cycleimpl3_changec_bird;

{$mode objfpc}

interface

function Fly(w : word): word;

implementation

uses cycleimpl3_changec_cat;

function Fly(w : word): word;
begin
  Result:=Jump(w)*5;
end;

end.
