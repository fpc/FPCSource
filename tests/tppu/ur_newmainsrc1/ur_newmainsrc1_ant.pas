unit ur_newmainsrc1_ant;

{$mode objfpc}

interface

uses ur_newmainsrc1_bird;

function Walk(w: word): word;

implementation

function Walk(w: word): word;
begin
  Result:=2*w;
end;

end.
