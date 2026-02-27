unit cycle32_changec_ant;

{$mode objfpc}

interface

uses cycle32_changec_bird;

function Crawl(w: TWing): TWing; // using symbol TWing from bird

implementation

function Crawl(w: TWing): TWing;
begin
  Result := Fly(w*2);
end;

end.
