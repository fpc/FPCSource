unit cycle2_changeb_ant;

{$mode objfpc}

interface

uses cycle2_changeb_bird;

const Factor = 3;

function Crawl(w : word): word;

implementation

function Crawl(w : word): word;
begin
  Result := Fly(w*2);
end;

end.
