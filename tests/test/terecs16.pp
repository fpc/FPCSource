program terecs16;

{$mode delphi}
type
  TRec = record
    l: longint;
    constructor Create;
  end;


var
  r: TRec;

  constructor TRec.Create;
  begin
    l := 0;
    r.l := 4;
    if l <> 0 then
      halt(1);
    l := 5;
    if r.l <> 4 then
      halt(2);
    r.l := 6;
  end;

begin
  r := TRec.Create;
  if r.l <> 5 then
    halt(3);
end.
