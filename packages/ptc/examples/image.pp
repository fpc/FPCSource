{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Image example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program ImageExample;

{$MODE objfpc}

uses
  SysUtils, ptc;

procedure load(surface: IPTCSurface; filename: String);
var
  F: File;
  width, height: Integer;
  pixels: PByte = nil;
  y: Integer;
  img_format: IPTCFormat;
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
    for y := height - 1 DownTo 0 do
      BlockRead(F, pixels[width * y * 3], width * 3);

    { load pixels to surface }
    {$IFDEF FPC_LITTLE_ENDIAN}
    img_format := TPTCFormatFactory.CreateNew(24, $00FF0000, $0000FF00, $000000FF);
    {$ELSE FPC_LITTLE_ENDIAN}
    img_format := TPTCFormatFactory.CreateNew(24, $000000FF, $0000FF00, $00FF0000);
    {$ENDIF FPC_LITTLE_ENDIAN}
    surface.Load(pixels, width, height, width * 3, img_format, TPTCPaletteFactory.CreateNew);

  finally
    CloseFile(F);

    { free image pixels }
    FreeMem(pixels);
  end;
end;

var
  console: IPTCConsole;
  format: IPTCFormat;
  surface: IPTCSurface;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      try
        { try to open the console matching the image resolution }
        console.open('Image example', 320, 200, format);
      except
        on TPTCError do
          { fallback to the default resolution }
          console.open('Image example', format);
      end;

      { create surface }
      surface := TPTCSurfaceFactory.CreateNew(320, 200, format);

      { load image to surface }
      load(surface, 'image.tga');

      { copy surface to console }
      surface.copy(console);

      { update console }
      console.update;

      { read key }
      console.ReadKey;

    finally
      { close console }
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
