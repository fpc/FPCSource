{ %FAIL }

unit tgenconst23;

{$mode objfpc}

interface

implementation

generic procedure Test<A; const N: LongInt>; forward;

generic procedure Test<A; const N: String>;
begin
end;


end.

