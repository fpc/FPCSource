{$ifdef fpc}
{$mode delphi}
{$endif}

{$r+}

CONST MaxBitmaps=129;

TYPE  tbitmap = longint;
      TBack =CLASS
                 constructor create;
                 PRIVATE
                   FBitmaps :ARRAY [0..MaxBitmaps] OF TBitmap;

                 PUBLIC
                   PROPERTY Bitmap :TBitmap READ FBitmaps[0];
                   PROPERTY LightBitmap :TBitmap READ FBitmaps[1];
                   PROPERTY ShadowBitmap:TBitmap READ FBitmaps[2];
            end;

constructor tback.create;
var
  i: longint;
begin
  for i := low(fbitmaps) to high(fbitmaps) do
    fbitmaps[i] := i;
end;

var
  b: tback;
begin
  b:=tback.create;
  if (b.Bitmap <> 0) or
     (b.LightBitmap <> 1) or
     (b.ShadowBitmap <> 2) then
    halt(1);
  b.free;
end.
