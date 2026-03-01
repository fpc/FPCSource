unit inline1_ant;

{$mode objfpc}

interface

uses inline1_bird;

function run: word;

implementation

function run: word;
begin
  Result:=Fly(3);
end;

end.
