{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Clip example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program ClipExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  x1, y1, x2, y2: Integer;
  pixels: PUint32;
  width, height: Integer;
  i: Integer;
  x, y, r, g, b: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Clip example', format);

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { calculate clip coordinates }
      x1 := console.width div 4;
      y1 := console.height div 4;
      x2 := console.width - x1;
      y2 := console.height - y1;

      { set clip area }
      console.clip(TPTCAreaFactory.CreateNew(x1, y1, x2, y2));

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
