{$mode objfpc}
uses Variants;

procedure test;
var
  Bounds: Array [0..1] of TVarArrayBound;
  V1, V2: Variant;
begin
  Bounds[0].lowbound := 0;
  Bounds[0].elementcount := 1;
  Bounds[1].lowbound := 0;
  Bounds[1].elementcount := 0;
  V1 := VarArrayCreate(@Bounds, 2, varVariant);
  V2 := V1; // <- Exception EVariantBadIndexError!!!!!
end;
 
begin
  test;
end.