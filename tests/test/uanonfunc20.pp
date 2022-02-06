unit uanonfunc20;

{$mode objfpc}

interface

type
  tbase = class
  protected
    function x: longint;
  end;

implementation

function tbase.x: longint;
begin
  result := 123;
end;

end.

