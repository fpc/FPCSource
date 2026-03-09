unit cycle22_changec_bird;

{$mode objfpc}

interface

uses cycle22_changec_cat;

const Factor = 7;

function Fly(w : word): word;

implementation

uses cycle22_changec_ant;

function Fly(w : word): word;
begin
  Result:=Jump(w)*5;
end;

end.
