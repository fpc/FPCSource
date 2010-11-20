{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Clear example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program ClearExample;

{$MODE objfpc}

uses
  SysUtils, ptc;

var
  console: TPTCConsole = nil;
  format: TPTCFormat = nil;
  surface: TPTCSurface = nil;
  width, height: Integer;
  x, y: Integer;
  size: Integer;
  area: TPTCArea = nil;
  color: TPTCColor = nil;
begin
  try
    { create console }
    console := TPTCConsole.Create;

    { create format }
    format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

    { open the console }
    console.open('Clear example', format);

    { create surface matching console dimensions }
    surface := TPTCSurface.Create(console.width, console.height, format);

    { loop until a key is pressed }
    while not console.KeyPressed do
    begin
      { get surface dimensions }
      width := surface.width;
      height := surface.height;

      { get random position }
      x := Random(width);
      y := Random(height);

      { get random area size }
      size := Random(width div 8);

      try
        { setup clear area }
        area := TPTCArea.Create(x-size, y-size, x+size, y+size);

        { create random color }
        color := TPTCColor.Create(Random, Random, Random);

        { clear surface area with color }
        surface.clear(color, area);

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;
      finally
        FreeAndNil(area);
        FreeAndNil(color);
      end;
    end;
    console.close;
    console.Free;
    surface.Free;
    format.Free;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
