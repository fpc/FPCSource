{ %fail }
{ %version=1.1 }

{$ifdef fpc}
  {$mode objfpc}
  {$threading on}
{$endif}

threadvar
   thri : longint;

begin
  { Delphi does not allow threadvars as for loop control variable }
  for thri:=1 to 1000 do;
end.
