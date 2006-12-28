{$ifdef fpc}
{$mode delphi}
{$else}
{$apptype console}
{$endif}

Uses Sysutils,variants;

var v : variant;
   s : string;

begin
  v := 1;
  s:=inttostr(v);
  if (s <> '1') then
    halt(1);
end.
