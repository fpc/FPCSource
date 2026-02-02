unit cycle3_changec_bird;

{$mode objfpc}

interface

uses cycle3_changec_cat;

function Fly(w : word): word;

implementation

function Fly(w : word): word;
begin
  Result:=Jump(w)*5;
end;

end.
