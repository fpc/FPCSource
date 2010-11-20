{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Pixel example for OpenPTC 1.0 C++ API
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

program PixelExample;

{$MODE objfpc}

uses
  ptc;

procedure putpixel(surface: TPTCSurface; x, y: Integer; r, g, b: Uint8);
var
  pixels: PUint32;
  color: Uint32;
begin
  { lock surface }
  pixels := surface.lock;
  try
    { pack the color integer from r,g,b components }
    color := (r shl 16) or (g shl 8) or b;

    { plot the pixel on the surface }
    pixels[x + y * surface.width] := color;
  finally
    { unlock surface }
    surface.unlock;
  end;
end;

var
  console: TPTCConsole = nil;
  surface: TPTCSurface = nil;
  format: TPTCFormat = nil;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { create format }
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Pixel example', format);

      { create surface matching console dimensions }
      surface := TPTCSurface.Create(console.width, console.height, format);

      { plot a white pixel in the middle of the surface }
      putpixel(surface, surface.width div 2, surface.height div 2, 255, 255, 255);

      { copy to console }
      surface.copy(console);

      { update console }
      console.update;

      { read key }
      console.ReadKey;
    finally
      console.close;
      console.Free;
      surface.Free;
      format.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
