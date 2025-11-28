unit implinline2_ant;

{$mode objfpc}

interface

uses implinline2_bird;

function Times123(w : word): word;

implementation

function Times123(w : word): word; inline;
begin
  Result := w*123;
end;

end.
