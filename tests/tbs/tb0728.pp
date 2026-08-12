program tb0728;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  { with the Spacer field we make sure that TSmallPoint is passed as reference
    for const parameters also on non-Windows }
  TSmallPoint =
  packed record
      X,
      Y : SmallInt;
      Spacer: array[0..7] of SmallInt;
      end;

  TPoint  =
  record
       X : Longint; Y : Longint;
     public
       class operator := (const aspt : TSmallPoint) : TPoint;
     end;

  class operator TPoint.:= (const aspt : TSmallPoint): TPoint;
  begin
    result.x:=aspt.x;
    result.y:=aspt.y;
  end;

var
  s: TSmallPoint;
  p: TPoint;
begin
  s.x:=123;
  s.y:=-2;
  p:=s;
  if p.x <> 123 then
    Halt(1);
  if p.y <> -2 then
    Halt(2);
  Writeln('ok');
end.
