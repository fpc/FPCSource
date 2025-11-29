unit implinline3_ant;

{$mode objfpc}

interface

uses implinline3_bird;

function Times123(w : word): word;

implementation

function Times123(w : word): word; inline;
begin
  Result := 123 * w; // changed
end;

end.
