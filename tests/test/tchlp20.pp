{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  strict protected }
program tchlp20;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest3;
begin
  t.Test;
end.

