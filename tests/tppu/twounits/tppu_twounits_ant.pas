unit tppu_twounits_ant;

{$mode objfpc}

interface

uses tppu_twounits_bird;

function Times123(w : word): word;

implementation

function Times123(w : word): word;
begin
  Result := w*123;
end;

end.
