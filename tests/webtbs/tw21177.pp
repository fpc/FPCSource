{$modeswitch ADVANCEDRECORDS}
{$OPTIMIZATION REGVAR}
program record_bug;

type
TColor = object
  R : Byte;
  function toDWord : DWord;
end;

function TColor.toDWord : DWord;
begin
  r:=4;
  toDWord:=5;
end;

procedure Fill(Color: TColor);
begin
  Color.toDWord;
  if color.r<>4 then
    halt(1);
end;

var
  c: TColor;
begin
  c.r:=1;
  Fill(c);
end.
