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
  console: IPTCConsole;
  format: IPTCFormat;
  surface: IPTCSurface;
  width, height: Integer;
  x, y: Integer;
  size: Integer;
  area: IPTCArea;
  color: IPTCColor;
begin
  try
    { create console }
    console := TPTCConsoleFactory.CreateNew;

    { create format }
    format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

    { open the console }
    console.open('Clear example', format);

    { create surface matching console dimensions }
    surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

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

      { setup clear area }
      area := TPTCAreaFactory.CreateNew(x-size, y-size, x+size, y+size);

      { create random color }
      color := TPTCColorFactory.CreateNew(Random, Random, Random);

      { clear surface area with color }
      surface.clear(color, area);

      { copy to console }
      surface.copy(console);

      { update console }
      console.update;
    end;
    if Assigned(console) then
      console.close;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
