unit cycleimpl3_changec_ant;

{$mode objfpc}

interface

const Factor = 3;

function Crawl(w : word): word;

implementation

uses cycleimpl3_changec_bird;

function Crawl(w : word): word;
begin
  Result := Fly(w*2);
end;

end.
