unit cycle3_changec_ant;

{$mode objfpc}

interface

uses cycle3_changec_bird;

const Factor = 3;

function Crawl(w : word): word;

implementation

function Crawl(w : word): word;
begin
  Result := Fly(w*2);
end;

end.
