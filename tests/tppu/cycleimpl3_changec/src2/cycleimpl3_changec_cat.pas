unit cycleimpl3_changec_cat;

{$mode objfpc}

interface

function Jump(w : word): word;

implementation

uses cycleimpl3_changec_ant;

function Jump(w : word): word;
begin
  Result:=w*11*Factor; // changed
end;

end.
