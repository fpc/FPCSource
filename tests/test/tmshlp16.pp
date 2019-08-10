program tmshlp16;

{$mode objfpc}
{$modeswitch multihelpers}

uses
  umshlp15b, umshlp15a;

var
  o: TObject;
begin
  if o.Test <> 1 then
    Halt(1);
end.
