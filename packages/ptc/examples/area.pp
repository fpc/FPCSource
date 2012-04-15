{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Area example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program AreaExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  format: IPTCFormat;
  surface: IPTCSurface;
  pixels: PDWord;
  width, height: Integer;
  i: Integer;
  x, y, r, g, b: Integer;
  area: IPTCArea;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { create console }
      console.open('Area example', format);

      { create surface half the size of the console }
      surface := TPTCSurfaceFactory.CreateNew(console.width div 2, console.height div 2, format);

      { setup destination area }
      x := console.width div 4;
      y := console.height div 4;
      area := TPTCAreaFactory.CreateNew(x, y, x + surface.width, y + surface.height);

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
        { lock surface }
        pixels := surface.lock;
        try
          { get surface dimensions }
          width := surface.width;
          height := surface.height;

          { draw random pixels }
          for i := 1 to 100 do
          begin
            { get random position }
            x := Random(width);
            y := Random(height);

            { get random color }
            r := Random(256);
            g := Random(256);
            b := Random(256);

            { draw color [r,g,b] at position [x,y] }
            pixels[x + y * width] := (r shl 16) + (g shl 8) + b;
          end;
        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy surface to console destination area }
        surface.copy(console, surface.area, area);

        { update console area }
        console.update;
      end;
    finally
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
