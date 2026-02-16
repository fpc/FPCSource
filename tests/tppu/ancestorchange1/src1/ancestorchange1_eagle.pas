unit ancestorchange1_eagle;

{$mode objfpc}

interface

type

  { TEagle }

  TEagle	=  class
  public
    function Swoop(w: word): word;
  end;		

implementation

{ TEagle }

function TEagle.Swoop(w: word): word;
begin
  Result:=2*w;
end;

end.
