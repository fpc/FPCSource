unit ancestorchange1_eagle;

{$mode objfpc}

interface

type

  { TEagle }

  TEagle = class
  public
    // 16th Feb 2026 fpc's indirect_crc does not always change when the parameter type changes,
    // as tobjectdef only stores a ppu index.
    // For this test change the modifier:
    function Swoop(const w: word): word;
  end;		

implementation

{ TEagle }

function TEagle.Swoop(const w: word): word;
begin
  Result:=2*w;
end;

end.
