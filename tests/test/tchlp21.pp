{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  protected }
program tchlp18;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest4;
begin
  t.Test;
end.

