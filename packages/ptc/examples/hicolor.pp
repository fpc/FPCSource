{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 HiColor example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program HiColorExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  pixels: PUint16;
  width, height: Integer;
  i: Integer;
  x, y, r, g, b: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(16, $F800, $07E0, $001F);

      { open the console }
      console.open('HiColor example', format);

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

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
            pixels[x + y * width] := ((r and $00F8) shl 8) or
                                     ((g and $00FC) shl 3) or
                                     ((b and $00F8) shr 3);
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
