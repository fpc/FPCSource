unit implinline1_ant;

{$mode objfpc}

interface

uses implinline1_bird;

function Times123(w : word): word;

implementation

function Times123(w : word): word; inline;
begin
   Result := w*123;
end;

end.
