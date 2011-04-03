{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  private }
program tchlp19;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest2;
begin
  t.Test;
end.

