{ %FAIL }

unit tgenconst24;

{$mode delphi}

interface

implementation

procedure Test<A; const N: LongInt>; forward;

procedure Test<A; const N: String>;
begin
end;


end.

