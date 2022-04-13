{ %FAIL }

program talignrecbad4;

{ Alignment must be a power of 2 between 1 and 64... 0 should return a compiler error }

type BadAlignment = record
  Field: Integer;
end align 0;

begin
end.