unit generic_cycle2_ant;

{$mode objfpc}

interface

const
  Factor = 7;

function Crawl(w : word): word;

implementation

uses generic_cycle2_bird;

type
  TAnt = specialize TBird<word>;

function Crawl(w : word): word;
var
  Ant: TAnt;
begin
  Ant:=TAnt.Create;
  Result:=Ant.Fly(w);
  Ant.Free;
end;

end.
