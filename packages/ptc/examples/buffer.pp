{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Buffer example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program BufferExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  format: IPTCFormat;
  width, height: Integer;
  pixels: PUint32 = nil;
  x, y, r, g, b: Integer;
  i: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.Open('Buffer example', format);

      { get console dimensions }
      width := console.Width;
      height := console.Height;

      { allocate a buffer of pixels }
      pixels := GetMem(width * height * SizeOf(Uint32));
      FillChar(pixels^, width * height * SizeOf(Uint32), 0);

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
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
          pixels[x + y * width] := (r shl 16) or (g shl 8) or b;
        end;

        { load pixels to console }
        console.Load(pixels, width, height, width * 4, format, TPTCPaletteFactory.CreateNew);

        { update console }
        console.Update;
      end;
    finally
      { free pixels buffer }
      FreeMem(pixels);
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
