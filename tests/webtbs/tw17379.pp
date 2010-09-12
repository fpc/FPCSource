{ %norun }

{$mode macpas}
{$warnings off}
program recursivefunctionparam;

function first( function test( theint: integer): boolean): integer;
begin {not implemented} end;

function find: integer;

  function test( theint: integer): boolean;
  begin
    first( test)
  end;

begin
  {not implemented}
end;

begin
end.
