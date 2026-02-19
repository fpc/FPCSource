unit generic_cycle1_bird;

{$mode objfpc}

interface

type

  { TBird }

  generic TBird<T> = class
  public
    function Fly(p: T): word;
  end;

implementation

uses generic_cycle1_ant;

{ TBird }

function TBird.Fly(p: T): word;
var
  R: specialize TRunner<T>;
begin
  R.Value:=p;
  Result:=sizeof(T)*R.Value*Factor;
end;

end.
