var x:uint64;
    y:longword;
begin
 y:=12;
 x:=y*uint64(10116239910488455739);
 if x<>10714414483604159172 then
   halt(1);
 x:=y*uint64($8000000000000000);
 if x<>(uint64(12) shl 63) then
   halt(2);
end.

