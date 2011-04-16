{ %NORUN }

{ usage of nested helpers adheres to visibility rules as well - here:
  public }
program tchlp22;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uchlp18;

var
  t: TTest5;
begin
  t.Test;
end.

