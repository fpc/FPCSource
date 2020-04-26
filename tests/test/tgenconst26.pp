{ %FAIL }

unit tgenconst26;

{$mode delphi}

interface

implementation

procedure Test<A; const N: LongInt>; forward;

procedure Test<A; N>;
begin
end;

end.

