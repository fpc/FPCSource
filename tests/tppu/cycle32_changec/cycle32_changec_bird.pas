unit cycle32_changec_bird;

{$mode objfpc}

interface

uses cycle32_changec_cat;

type
  TWing	= longint;

const Factor = 7;

function Fly(w: TWing): TWing;

implementation

uses cycle32_changec_ant;

function Fly(w: TWing): TWing;
begin
  Result:=Jump(w)*5;
end;

end.
