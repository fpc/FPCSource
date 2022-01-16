{$IFDEF FPC}
{$MODE TP}
{$ENDIF}
program a;

type
  rectyp = record
    { it doesn't crash if this is 0..5 }
    f: array[0..6] of byte;
  end;
  prectyp = ^rectyp;

  arrrec = array[0..1] of rectyp;
  parrrectyp = ^arrrec;

var
  arr: parrrectyp;

procedure xx(x: integer);
begin
  { crash here }
  arr^[x]:=arr^[x+1];
end;

begin
end.
