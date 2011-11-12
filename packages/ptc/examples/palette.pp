{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Palette example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program PaletteExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  palette: IPTCPalette;
  data: array [0..255] of Uint32;
  pixels: PUint8;
  width, height: Integer;
  i: Integer;
  x, y, index: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(8);

      { open console }
      console.open('Palette example', format);

      { create surface }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { create palette }
      palette := TPTCPaletteFactory.CreateNew;

      { generate palette }
      for i := 0 to 255 do
        data[i] := i;

      { load palette data }
      palette.Load(data);

      { set console palette }
      console.Palette(palette);

      { set surface palette }
      surface.Palette(palette);

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

            { get random color index }
            index := Random(256);

            { draw color [index] at position [x,y] }
            pixels[x + y * width] := index;
          end;
        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy to console }
        surface.copy(console);

        { update console }
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
