unit generic_changec_ant;

{$mode objfpc}

interface

uses generic_changec_bird;

function run: word;

implementation

function run: word;
var
  bc: TBirdCat;
begin
  bc:=TBirdCat.Create;
  Result:=bc.Jump(3);
end;

end.
