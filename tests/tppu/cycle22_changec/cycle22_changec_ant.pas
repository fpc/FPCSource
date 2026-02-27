unit cycle22_changec_ant;

{$mode objfpc}

interface

uses cycle22_changec_bird;

function Crawl(w : word): word;

implementation

function Crawl(w : word): word;
begin
  Result := Fly(w*2);
end;

end.
