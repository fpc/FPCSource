{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Flower demo for OpenPTC 1.0 C++ API
 Copyright (c) Scott Buchanan (aka Goblin)
 This source code is licensed under the GNU GPL
}

program Flower;

{$MODE objfpc}

uses
  ptc, Math;

function pack(r, g, b: Uint32): Uint32;
begin
  { pack color integer }
  pack := (r shl 16) or (g shl 8) or b;
end;

procedure generate_flower(flower: IPTCSurface);
var
  data: PUint8;
  x, y, fx, fy, fx2, fy2: Integer;
  TWO_PI: Single;
begin
  { lock surface }
  data := flower.lock;

  try
    { surface width and height constants for cleaner code }
    fx := flower.width;
    fy := flower.height;
    fx2 := fx div 2;
    fy2 := fy div 2;

    { useful 2*pi constant }
    TWO_PI := 2 * PI;

    { generate flower image }
    for y := 0 to fy - 1 do
      for x := 0 to fx - 1 do
        data[x + y * fx] := Trunc(1.0 * Cos(18*ArcTan2((y - fy2),(x - fx2))) * 255 / TWO_PI +
                                  0.3 * Sin(15*ArcTan2((y - fy2),(x - fx2))) * 255 / TWO_PI +
                                  Sqrt((y - fy2) * (y - fy2) + (x - fx2) * (x - fx2))) and $FF;

    { You might want to move the 1.0 and 0.3 and the 18 and the 15
      to parameters passed to the generate function...
      the 1.0 and the 0.3 define the 'height' of the flower, while the
      18 and 15 control the number of 'petals' }
  finally
    flower.unlock;
  end;
end;

procedure generate(palette: IPTCPalette);
var
  data: PUint32;
  i, c: Integer;
begin
  { lock palette data }
  data := palette.Lock;

  try
    { black to yellow }
    i := 0;
    c := 0;
    while i < 64 do
    begin
      data[i] := pack(c, c, 0);
      Inc(c, 4);
      Inc(i);
    end;

    { yellow to red }
    c := 0;
    while i < 128 do
    begin
      data[i] := pack(255, 255 - c, 0);
      Inc(c, 4);
      Inc(i);
    end;

    { red to white }
    c := 0;
    while i < 192 do
    begin
      data[i] := pack(255, c, c);
      Inc(c, 4);
      Inc(i);
    end;

    { white to black }
    c := 0;
    while i < 256 do
    begin
      data[i] := pack(255 - c, 255 - c, 255 - c);
      Inc(c, 4);
      Inc(i);
    end;
  finally
    { unlock palette }
    palette.Unlock;
  end;
end;

var
  console: IPTCConsole;
  format: IPTCFormat;
  flower_surface: IPTCSurface;
  surface: IPTCSurface;
  palette: IPTCPalette;
  area: IPTCArea;
  time, delta: Single;
  scr, map: PUint8;
  width, height, mapWidth: Integer;
  xo, yo, xo2, yo2, xo3, yo3: Single;
  offset1, offset2, offset3: Integer;
  x, y: Integer;
begin
  try
    try
      { create format }
      format := TPTCFormatFactory.CreateNew(8);

      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create flower surface }
      flower_surface := TPTCSurfaceFactory.CreateNew(640, 400, format);

      { generate flower }
      generate_flower(flower_surface);

      { open console }
      console.open('Flower demo', 320, 200, format);

      { create surface }
      surface := TPTCSurfaceFactory.CreateNew(320, 200, format);

      { create palette }
      palette := TPTCPaletteFactory.CreateNew;

      { generate palette }
      generate(palette);

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

      { setup copy area }
      area := TPTCAreaFactory.CreateNew(0, 0, 320, 200);

      { time data }
      time := 0;
      delta := 0.04;

      { main loop }
      while not console.KeyPressed do
      begin
        { lock surface pixels }
        scr := surface.lock;
        try
          map := flower_surface.lock;
          try
            { get surface dimensions }
            width := surface.width;
            height := surface.height;
            mapWidth := flower_surface.width;

            xo := (width / 2) + 120 * sin(time * 1.1 + 1.5);
            yo := (height / 2) + 90 * cos(time * 0.8 + 1.1);
            offset1 := Trunc(xo) + Trunc(yo) * mapWidth;

            xo2 := (width / 2) + 120 * sin(time * 0.9 + 4.2);
            yo2 := (height / 2) + 90 * cos(time * 0.7 + 6.9);
            offset2 := Trunc(xo2) + Trunc(yo2) * mapWidth;

            xo3 := (width / 2) + 120 * sin(time * 0.9 + 3.1);
            yo3 := (height / 2) + 90 * cos(time * 1.1 + 1.2);
            offset3 := Trunc(xo3) + Trunc(yo3) * mapWidth;

            { vertical loop }
            for y := 0 to height - 1 do
              { horizontal loop }
              for x := 0 to width - 1 do
                scr[x + y * width] := (map[x + y * mapWidth + offset1] +
                                       map[x + y * mapWidth + offset2] +
                                       map[x + y * mapWidth + offset3]) and $FF;
          finally
            { unlock surface }
            flower_surface.unlock;
          end;
        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy surface to console }
        surface.copy(console, area, area);

        { update console }
        console.update;

        { update time }
        time := time + delta;
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
