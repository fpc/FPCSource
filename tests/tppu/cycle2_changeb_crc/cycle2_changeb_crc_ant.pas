unit cycle2_changeb_crc_ant;

{$mode objfpc}

interface

uses cycle2_changeb_crc_bird;

function Crawl(w : word): word;

implementation

function Crawl(w : word): word;
begin
  Result := specialize Fly<word>(w*2);
end;

end.
