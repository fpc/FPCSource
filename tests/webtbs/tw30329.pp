program TestOutbox2d;

{$mode objfpc}{$H+}

type
  TGF2DPoint = array[0..1] of Single;

  TGF2DOutbox = packed record
    FMin, FMax: TGF2DPoint;
  end;

function Some2DOutbox : TGF2DOutbox;
begin
  Result.FMin[0]:=1.0;
  Result.FMin[1]:=2.0;
  Result.FMax[0]:=3.0;
  Result.FMax[1]:=4.0;
end;


var
  Outbox : TGF2DOutbox;
begin
  Outbox:=Some2DOutbox;
  with outbox do
    if (fmin[0]<>1.0) or
       (fmin[1]<>2.0) or
       (fmax[0]<>3.0) or
       (fmax[1]<>4.0) then
      halt(1);
end.

