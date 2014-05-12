{ %fail }

{$MODE OBJFPC} {$MODESWITCH PROPERTIES-}
program test;
function GetBar(): Cardinal; begin Result := 0; end;
property Bar: Cardinal read GetBar;
begin Writeln(Bar); end.

