{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  strict private }
program tchlp18;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest1;
begin
  t.Test;
end.
