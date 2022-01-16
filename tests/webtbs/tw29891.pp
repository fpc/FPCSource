{ %NORUN }

program tinterr;

{$mode objfpc}{$H+}

type
  TALVector3f = array[0..2] of Single;

function alGetSource3f: TALVector3f;
begin

end;

function VectorToNiceStr(const v: array of Single): string;
begin

end;

  function SampleSourceState:string;
  begin
   result := 'POSITION : '+ VectorToNiceStr(alGetSource3f) + LineEnding;
  end;

begin
end.
