{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Stretch example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program StretchExample;

{$MODE objfpc}

uses
  ptc;

procedure load(surface: IPTCSurface; filename: string);
var
  F: File;
  width, height: Integer;
  pixels: PByte = nil;
  y: Integer;
  format: IPTCFormat;
begin
  { open image file }
  AssignFile(F, filename);
  Reset(F, 1);

  try
    { skip header }
    Seek(F, 18);

    { get surface dimensions }
    width := surface.width;
    height := surface.height;

    { allocate image pixels }
    pixels := GetMem(width * height * 3);

    { read image pixels one line at a time }
    for y := height - 1 downto 0 do
      BlockRead(F, pixels[width * y * 3], width * 3);

    { load pixels to surface }
    {$IFDEF FPC_LITTLE_ENDIAN}
    format := TPTCFormatFactory.CreateNew(24, $00FF0000, $0000FF00, $000000FF);
    {$ELSE FPC_LITTLE_ENDIAN}
    format := TPTCFormatFactory.CreateNew(24, $000000FF, $0000FF00, $00FF0000);
    {$ENDIF FPC_LITTLE_ENDIAN}
    surface.Load(pixels, width, height, width * 3, format, TPTCPaletteFactory.CreateNew);
  finally
    { free image pixels }
    FreeMem(pixels);

    { close file }
    CloseFile(F);
  end;
end;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  image: IPTCSurface;
  format: IPTCFormat;
  timer: IPTCTimer;
  area: IPTCArea;
  time: Double;
  zoom: Single;
  x, y, x1, y1, x2, y2, dx, dy: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Stretch example', format);

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { create image surface }
      image := TPTCSurfaceFactory.CreateNew(320, 140, format);

      { load image to surface }
      load(image, 'stretch.tga');

      { setup stretching parameters }
      x := surface.width div 2;
      y := surface.height div 2;
      dx := surface.width div 2;
      dy := surface.height div 3;

      { create timer }
      timer := TPTCTimerFactory.CreateNew;

      { start timer }
      timer.start;

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
        { get current time from timer }
        time := timer.time;

        { clear surface to white background }
        surface.clear(TPTCColorFactory.CreateNew(1, 1, 1));

        { calculate zoom factor at current time }
        zoom := 2.5 * (1 - cos(time));

        { calculate zoomed image coordinates }
        x1 := Trunc(x - zoom * dx);
        y1 := Trunc(y - zoom * dy);
        x2 := Trunc(x + zoom * dx);
        y2 := Trunc(y + zoom * dy);

        { setup image copy area }
        area := TPTCAreaFactory.CreateNew(x1, y1, x2, y2);

        { copy and stretch image to surface }
        image.copy(surface, image.area, area);

        { copy surface to console }
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
