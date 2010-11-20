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
  console: TPTCConsole = nil;
  surface: TPTCSurface = nil;
  format: TPTCFormat = nil;
  palette: TPTCPalette = nil;
  data: array [0..255] of Uint32;
  pixels: PUint8;
  width, height: Integer;
  i: Integer;
  x, y, index: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { create format }
      format := TPTCFormat.Create(8);

      { open console }
      console.open('Palette example', format);

      { create surface }
      surface := TPTCSurface.Create(console.width, console.height, format);

      { create palette }
      palette := TPTCPalette.Create;

      { generate palette }
      for i := 0 to 255 do
        data[i] := i;

      { load palette data }
      palette.load(data);

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

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
      console.close;
      console.Free;
      surface.Free;
      palette.Free;
      format.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
