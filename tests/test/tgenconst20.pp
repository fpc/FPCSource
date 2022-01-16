{ %NORUN }
unit tgenconst20;

{$mode delphi}

interface

procedure Test<const A, B: LongInt>;
procedure Test2<const A, B: LongInt>;

implementation

{ currently it does not matter whether , or ; is used in the definition (Delphi
  compatible) }

procedure Test<A, B>;
begin
end;

procedure Test2<A; B>;
begin
end;

end.
