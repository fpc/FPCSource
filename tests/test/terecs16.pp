program terecs16;

{$mode delphi}
type
  TRec = record
    l: longint;
    constructor Create(a: longint);
  end;


var
  r: TRec;

  constructor TRec.Create(a: longint);
  begin
    l := a;
    r.l := 4;
    if l <> a then
      halt(1);
    l := 5;
    if r.l <> 4 then
      halt(2);
    r.l := 6;
  end;

begin
  r := TRec.Create(10);
  if r.l <> 5 then
    halt(3);
end.
