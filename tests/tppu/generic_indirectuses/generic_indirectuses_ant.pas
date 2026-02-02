unit generic_indirectuses_ant;

{$mode objfpc}

interface

uses generic_indirectuses_bird;

type

  { TAntBirdCat }

  TAntBirdCat = class(TBirdCat)
    function Crawl(w: word): word;
  end;

implementation

{ TAntBirdCat }

function TAntBirdCat.Crawl(w: word): word;
begin
  Result:=specialize Jump<dword>(w);
end;

end.
