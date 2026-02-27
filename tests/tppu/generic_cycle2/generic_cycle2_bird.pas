unit generic_cycle2_bird;

{$mode objfpc}

interface

uses generic_cycle2_ant;

type

  { TBird }

  generic TBird<T> = class
  public
    function Fly(p: T): word;
  end;

implementation

{ TBird }

function TBird.Fly(p: T): word;
begin
  Result:=sizeof(T)*p*Factor;
end;

end.
