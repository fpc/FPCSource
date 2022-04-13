{ %FAIL }

program talignrecbad1;

{ Alignment must be a power of 2 between 1 and 64... 3 should return a compiler error }

type BadAlignment = record
  Field: Integer;
end align 3;

begin
end.