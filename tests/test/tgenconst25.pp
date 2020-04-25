{ %FAIL }

unit tgenconst25;

{$mode objfpc}

interface

implementation

generic procedure Test<A; const N: LongInt>; forward;

generic procedure Test<A; N>;
begin
end;

end.

