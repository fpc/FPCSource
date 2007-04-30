program test;

{$mode objfpc}

function CreateVarArray( const DimCount: integer;
                         const Bounds: array of integer;
                         const VarType: integer ): Variant;
var
  Dim01: array [0.. 1] of integer absolute Bounds;
  Dim02: array [0.. 3] of integer absolute Bounds;
  Dim03: array [0.. 5] of integer absolute Bounds;
  Dim04: array [0.. 7] of integer absolute Bounds;
  Dim05: array [0.. 9] of integer absolute Bounds;
  Dim06: array [0..11] of integer absolute Bounds;
  Dim07: array [0..13] of integer absolute Bounds;
  Dim08: array [0..15] of integer absolute Bounds;
  Dim09: array [0..17] of integer absolute Bounds;
  Dim10: array [0..19] of integer absolute Bounds;
  Dim11: array [0..21] of integer absolute Bounds;
  Dim12: array [0..23] of integer absolute Bounds;
  Dim13: array [0..25] of integer absolute Bounds;
  Dim14: array [0..27] of integer absolute Bounds;
  Dim15: array [0..29] of integer absolute Bounds;
  Dim16: array [0..31] of integer absolute Bounds;
begin
end;

begin
end.