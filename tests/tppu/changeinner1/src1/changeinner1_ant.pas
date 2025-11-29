unit changeinner1_ant;

{$mode objfpc}

interface

uses changeinner1_bird;

function Crawl(w : word): word;

implementation

function Crawl(w : word): word;
begin
  Result := Fly(w*2);
end;

end.
