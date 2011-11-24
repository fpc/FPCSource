{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Fire demo for OpenPTC 1.0 C++ API
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

program Fire;

{$MODE objfpc}

uses
  ptc;

function pack(r, g, b: Uint32): Uint32;
begin
  { pack color integer }
  pack := (r shl 16) or (g shl 8) or b;
end;

procedure generate(palette: IPTCPalette);
var
  data: PUint32;
  i, c: Integer;
begin
  { lock palette data }
  data := palette.Lock;

  try
    { black to red }
    i := 0;
    c := 0;
    while i < 64 do
    begin
      data[i] := pack(c, 0, 0);
      Inc(c, 4);
      Inc(i);
    end;

    { red to yellow }
    c := 0;
    while i < 128 do
    begin
      data[i] := pack(255, c, 0);
      Inc(c, 4);
      Inc(i);
    end;

    { yellow to white }
    c := 0;
    while i < {192}128 do
    begin
      data[i] := pack(255, 255, c);
      Inc(c, 4);
      Inc(i);
    end;

    { white }
    while i < 256 do
    begin
      data[i] := pack(255, 255, 255);
      Inc(i);
    end;

  finally
    { unlock palette }
    palette.Unlock;
  end;
end;

var
  format: IPTCFormat;
  console: IPTCConsole;
  surface: IPTCSurface;
  palette: IPTCPalette;
  state: Integer;
  intensity: Single;
  pixels, pixel, p: PUint8;
  width, height: Integer;
  x, y: Integer;
  top, bottom, c1, c2: Uint32;
  generator: PUint8;
  color: Integer;
  area: IPTCArea;
begin
  try
    try
      { create format }
      format := TPTCFormatFactory.CreateNew(8);

      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { open console }
      console.open('Fire demo', 320, 200, format);

      { create surface }
      surface := TPTCSurfaceFactory.CreateNew(320, 208, format);

      { create palette }
      palette := TPTCPaletteFactory.CreateNew;

      { generate palette }
      generate(palette);

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

      { flame data }
      state := 0;
      intensity := 0;

      { setup copy area }
      area := TPTCAreaFactory.CreateNew(0, 0, 320, 200);

      { main loop }
      repeat
        { lower flame on keypress }
        if console.KeyPressed then
          state := 2;

        { state machine }
        case state of
          0: begin
            { raise flame }
            intensity := intensity + 0.007;

            { maximum flame height }
            if intensity > 0.8 then
              state := 1;
          end;
          1: begin
            { constant flame }
          end;
          2: begin
            { lower flame }
            intensity := intensity - 0.005;

            { exit program when flame is out }
            if intensity < 0.01 then
            begin
              console.close;
              exit;
            end;
          end;
        end;

        { lock surface pixels }
        pixels := surface.lock;

        try
          { get surface dimensions }
          width := surface.width;
          height := surface.height;

          { flame vertical loop }
          y := 1;
          while y < height - 4 do
          begin
            { current pixel pointer }
            pixel := pixels + y * width;

            { flame horizontal loop }
            for x := 0 to width - 1 do
            begin
              { sum top pixels }
              p := pixel + (width shl 1);
              top := p^;
              Inc(top, (p - 1)^);
              Inc(top, (p + 1)^);

              { bottom pixel }
              bottom := (pixel + (width shl 2))^;

              { combine pixels }
              c1 := (top + bottom) shr 2;
              if c1 > 1 then
                Dec(c1);

              { interpolate }
              c2 := (c1 + bottom) shr 1;

              { store pixels }
              pixel^ := c1;
              (pixel + width)^ := c2;

              { next pixel }
              Inc(pixel);
            end;
            Inc(y, 2);
          end;

          { setup flame generator pointer }
          generator := pixels + width * (height - 4);

          { update flame generator bar }
          x := 0;
          while x < width do
          begin
            { random block color taking intensity into account }
            color := random(Integer(Trunc(255 * intensity)));

            { write 4x4 color blocks }
            (generator + 0)^             := color;
            (generator + 1)^             := color;
            (generator + 2)^             := color;
            (generator + 3)^             := color;
            (generator + width + 0)^     := color;
            (generator + width + 1)^     := color;
            (generator + width + 2)^     := color;
            (generator + width + 3)^     := color;
            (generator + width * 2 + 0)^ := color;
            (generator + width * 2 + 1)^ := color;
            (generator + width * 2 + 2)^ := color;
            (generator + width * 2 + 3)^ := color;
            (generator + width * 3 + 0)^ := color;
            (generator + width * 3 + 1)^ := color;
            (generator + width * 3 + 2)^ := color;
            (generator + width * 3 + 3)^ := color;

            { next block }
            Inc(generator, 4);
            Inc(x, 4);
          end;

        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy surface to console }
        surface.copy(console, area, area);

        { update console }
        console.update;
      until False;

    finally
      if Assigned(console) then
        console.Close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
