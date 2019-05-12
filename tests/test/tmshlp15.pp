program tmshlp15;

{$mode objfpc}
{$modeswitch multihelpers}

uses
  umshlp15a, umshlp15b;

var
  o: TObject;
begin
  if o.Test <> 2 then
    Halt(1);
end.
