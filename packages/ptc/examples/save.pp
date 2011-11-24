{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Save example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program SaveExample;

{$MODE objfpc}

uses
  ptc, Math;

procedure save(surface: IPTCSurface; filename: string);
var
  F: File;
  width, height: Integer;
  size: Integer;
  y: Integer;
  pixels: PUint8 = nil;
  format: IPTCFormat;
  { generate the header for a true color targa image }
  header: array [0..17] of Uint8 =
    (0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  { open image file for writing }
  AssignFile(F, filename);
  Rewrite(F, 1);

  try
    { get surface dimensions }
    width := surface.width;
    height := surface.height;

    { set targa image width }
    header[12] := width and $FF;
    header[13] := width shr 8;

    { set targa image height }
    header[14] := height and $FF;
    header[15] := height shr 8;

    { set bits per pixel }
    header[16] := 24;

    { write tga header }
    BlockWrite(F, header, 18);

    { calculate size of image pixels }
    size := width * height * 3;

    { allocate image pixels }
    pixels := GetMem(size);

    {$IFDEF FPC_LITTLE_ENDIAN}
    format := TPTCFormatFactory.CreateNew(24, $00FF0000, $0000FF00, $000000FF);
    {$ELSE FPC_LITTLE_ENDIAN}
    format := TPTCFormatFactory.CreateNew(24, $000000FF, $0000FF00, $00FF0000);
    {$ENDIF FPC_LITTLE_ENDIAN}

    { save surface to image pixels }
    surface.save(pixels, width, height, width * 3, format, TPTCPaletteFactory.CreateNew);

    { write image pixels one line at a time }
    for y := height - 1 DownTo 0 do
      BlockWrite(F, pixels[width * y * 3], width * 3);

  finally
    { free image pixels }
    FreeMem(pixels);

    CloseFile(F);
  end;
end;

function calculate(real, imaginary: Single; maximum: Integer): Integer;
var
  c_r, c_i: Single;
  z_r, z_i: Single;
  z_r_squared, z_i_squared: Single;
  z_squared_magnitude: Single;
  count: Integer;
begin
  { complex number 'c' }
  c_r := real;
  c_i := imaginary;

  { complex 'z' }
  z_r := 0;
  z_i := 0;

  { complex 'z' squares }
  z_r_squared := 0;
  z_i_squared := 0;

  { mandelbrot function iteration loop }
  for count := 0 to maximum - 1 do
  begin
    { square 'z' and add 'c' }
    z_i := 2 * z_r * z_i + c_i;
    z_r := z_r_squared - z_i_squared + c_r;

    { update 'z' squares }
    z_r_squared := z_r * z_r;
    z_i_squared := z_i * z_i;

    { calculate squared magnitude of complex 'z' }
    z_squared_magnitude := z_r_squared + z_i_squared;

    { stop iterating if the magnitude of 'z' is greater than two }
    if z_squared_magnitude > 4 then
    begin
      calculate := Count;
      exit;
    end;
  end;

  { maximum }
  calculate := 0;
end;

procedure mandelbrot(console: IPTCConsole; surface: IPTCSurface;
                     x1, y1, x2, y2: Single);
const
  { constant values }
  entries = 1024;
  maximum = 1024;
var
  { fractal color table }
  table: array [0..entries - 1] of Uint32;
  i: Integer;
  f_index: Single;
  time: Single;
  intensity: Single;
  pixels, pixel: PUint32;
  width, height: Integer;
  dx, dy: Single;
  real, imaginary: Single;
  x, y: Integer;
  count: Integer;
  index: Integer;
  color: Uint32;
  area: IPTCArea;
begin
  { generate fractal color table }
  for i := 0 to entries - 1 do
  begin
    { calculate normalized index }
    f_index := i / entries;

    { calculate sine curve time value }
    time := f_index * pi - pi / 2;

    { lookup sine curve intensity at time and scale to [0,1] }
    intensity := (sin(time) + 1) / 2;

    { raise the intensity to a power }
    intensity := power(intensity, 0.1);

    { store intensity as a shade of blue }
    table[i] := Trunc(255 * intensity);
  end;

  { lock surface pixels }
  pixels := surface.lock;
  try
    { get surface dimensions }
    width := surface.width;
    height := surface.height;

    { current pixel pointer }
    pixel := pixels;

    { calculate real x,y deltas }
    dx := (x2 - x1) / width;
    dy := (y2 - y1) / height;

    { imaginary axis }
    imaginary := y1;

    { iterate down surface y }
    for y := 0 to height - 1 do
    begin
      { real axis }
      real := x1;

      { iterate across surface x }
      for x := 0 to width - 1 do
      begin
        { calculate the mandelbrot interation count }
        count := calculate(real, imaginary, maximum);

        { calculate color table index }
        index := count mod entries;

        { lookup color from iteration }
        color := table[index];

        { store color }
        pixel^ := color;

        { next pixel }
        Inc(pixel);

        { update real }
        real := real + dx;
      end;

      { update imaginary }
      imaginary := imaginary + dy;

      { setup line area }
      area := TPTCAreaFactory.CreateNew(0, y, width, y + 1);

      { copy surface area to console }
      surface.copy(console, area, area);

      { update console area }
      console.update;
    end;
  finally
    { unlock surface }
    surface.unlock;
  end;
end;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  x1, y1, x2, y2: Single;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console with a single page }
      console.open('Save example', format, 1);

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { setup viewing area }
      x1 := -2.00;
      y1 := -1.25;
      x2 := +1.00;
      y2 := +1.25;

      { render the mandelbrot fractal }
      mandelbrot(console, surface, x1, y1, x2, y2);

      { save mandelbrot image }
      save(surface, 'save.tga');

      { read key }
      console.ReadKey;
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
