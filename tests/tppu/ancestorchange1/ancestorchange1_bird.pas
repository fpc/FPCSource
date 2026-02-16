unit ancestorchange1_bird;

{$mode objfpc}

interface

uses ancestorchange1_cat;

type

  { TBird }

  TBird	=  class(TCat)
  public
    function Fly(w: word): integer;
  end;		

implementation

function TBird.Fly(w: word): integer;
begin
   Result:=Swoop(w);
end;

end.
