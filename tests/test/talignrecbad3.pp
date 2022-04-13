{ %FAIL }

program talignrecbad3;

{ Alignment must be a power of 2 between 1 and 64... 128 should return a compiler error }

type BadAlignment = record
  Field: Integer;
end align 128;

begin
end.