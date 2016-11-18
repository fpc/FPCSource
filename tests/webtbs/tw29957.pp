{ %cpu=i386,x86_64 }
{$mode objfpc}
{$asmmode intel}
uses cpu;

type
   TVector4 = packed record
     X, Y, Z, W: Single;
   end;

 function _VectorDotProductAVX(Vector1, Vector2: TVector4): Single; assembler;
 asm
   VMOVUPS XMM0, [Vector1]
   VMOVUPS XMM1, [Vector2]
   VDPPS XMM0, XMM0, XMM1, $71 { Only perform calculations on the X, Y and Z coordinates; only store result in the first element }
   VMOVSS Result, XMM0 { Store result - first element of XMM0 }
 end;

var
  v: tvector4;
  r: single;
begin
  v.x:=1;
  v.y:=1;
  v.z:=1;
  v.w:=1;
  if AVXSupport then
  begin
    r:=_vectordotproductavx(v,v);
    if r<>3 then
      halt(1);
    writeln('ok');
  end
  else
    writeln('No AVX support');
end.

