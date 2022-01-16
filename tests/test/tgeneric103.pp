unit tgeneric103;

{$mode objfpc}{$H+}

interface

implementation

generic procedure Test<T>(aArg: T);
begin

end;

initialization
finalization
  specialize Test<LongInt>(42);
end.

