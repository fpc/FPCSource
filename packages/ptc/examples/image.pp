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

procedure load(surface: TPTCSurface; filename: String);
var
  F: File;
  width, height: Integer;
  pixels: PByte = nil;
  y: Integer;
  img_format: TPTCFormat = nil;
  img_palette: TPTCPalette = nil;
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
    img_format := TPTCFormat.Create(24, $00FF0000, $0000FF00, $000000FF);
    {$ELSE FPC_LITTLE_ENDIAN}
    img_format := TPTCFormat.Create(24, $000000FF, $0000FF00, $00FF0000);
    {$ENDIF FPC_LITTLE_ENDIAN}
    img_palette := TPTCPalette.Create;
    surface.load(pixels, width, height, width * 3, img_format, img_palette);

  finally
    CloseFile(F);

    { free image pixels }
    FreeMem(pixels);

    img_palette.Free;
    img_format.Free;
  end;
end;

var
  console: TPTCConsole = nil;
  format: TPTCFormat = nil;
  surface: TPTCSurface = nil;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { create format }
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

      try
        { try to open the console matching the image resolution }
        console.open('Image example', 320, 200, format);
      except
        on TPTCError do
          { fallback to the default resolution }
          console.open('Image example', format);
      end;

      { create surface }
      surface := TPTCSurface.Create(320, 200, format);

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
      console.close;

      console.Free;
      surface.Free;
      format.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
