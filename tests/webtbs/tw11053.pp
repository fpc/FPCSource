program Project1;

{$mode delphi}

const height = 1;
      width = 1;

var pix: array [0..height-1,0..width-1] of Integer;

procedure Main;
var
    dx, dy: Integer;
    Color, digest: cardinal;
    cx, cy, zx, zy: Double;
    scale: Double;
    deep: Integer;

begin
  FillChar(pix, SizeOf(pix), $f0);
        scale := 0.05;
        deep := 30;
        Digest := 0;


      for dy := 0 to height -1 do
      begin
        cy := (dy - height / 2) * scale;
        for dx := 0 to width - 1 do
        begin
          color := 0;
          cx := (dx - width / 2) * scale;

          zx := cx;
          zy := cy;

          while zx * zx + zy * zy < 1 do
          begin
            zx := zx * zx - zy * zy + cx;
            zy := 2 * zx * zy + cy;
            Inc( color );
            if color > Cardinal(deep) then break;
          end;
          pix[ dy, dx ] := color;
        end;
      end;

  pix[ 0, 0 ] := 80;

  Digest := 0;
  for dy := 0 to height -1 do for dx := 0 to width - 1 do Digest := Digest + pix[dy, dx];

  if (digest<>80) then
    halt(1);
end;
begin
  Main;
end.
