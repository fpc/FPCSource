{ %NORUN }
unit tgenconst19;

{$mode objfpc}

interface

generic procedure Test<const A, B: LongInt>;
generic procedure Test2<const A, B: LongInt>;

implementation

{ currently it does not matter whether , or ; is used in the definition (Delphi
  compatible) }

generic procedure Test<A, B>;
begin
end;

generic procedure Test2<A; B>;
begin
end;

end.
