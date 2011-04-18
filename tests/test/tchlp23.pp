{ %NORUN }

{ usage of nested helpers adheres to visibility rules as well - here:
  published }
program tchlp23;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest6;
begin
  t.Test;
end.

