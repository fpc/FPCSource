unit testcib_elk;

{$mode objfpc}
{$inline on}

interface

uses testcib_bird;

function Run(w : word): word;

implementation

function Run(w : word): word;
begin
  Result:= 10 * Fly(w);
end;

end.
