{ %norun }
{ %opt=-O-1 }

{ helpers can extend type parameters if they can only be classes }
program Arm64Peep;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type bob = class
  function doInc: integer;
end;

function bob.doInc: integer;
begin
Result := 0;
Inc(Result, 32);
end;

begin
end.
