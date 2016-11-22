{ %norun }
{ %cpu=i386,x86_64 }
{ %opt=-Sew -vw }
{$mode objfpc}
{$asmmode intel}
{ The test checks that MOVSS instruction assembles without warning.
  Running it could be a nice bonus, but it turns out that we have no portable
  way to detect SSE4.1 support (for DPPS), so disabled for now. }
uses cpu;

type
   TVector4 = packed record
     X, Y, Z, W: Single;
   end;

 function _VectorDotProductSSE4(Vector1, Vector2: TVector4): Single; assembler;
 asm
   MOVUPS XMM0, [Vector1]
   MOVUPS XMM1, [Vector2]
   DPPS XMM0, XMM1, $71 { Only perform calculations on the X, Y and Z coordinates; only store result in the first element }
   MOVSS Result, XMM0 { Store result - first element of XMM0 }
 end;

var
  v: tvector4;
  r: single;
begin
  v.x:=1;
  v.y:=1;
  v.z:=1;
  v.w:=1;
  r:=_vectordotproductSSE4(v,v);
  if r<>3 then
    halt(1);
  writeln('ok');
end.

