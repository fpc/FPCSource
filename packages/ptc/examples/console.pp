{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Console example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program ConsoleExample;

{$MODE objfpc}

uses
  ptc;

var
  console: TPTCConsole = nil;
  palette: TPTCPalette = nil;
  data: array [0..255] of DWord;
  i: Integer;
  pixels: PByte;
  width, height, pitch: Integer;
  format: TPTCFormat;
  bits, bytes: Integer;
  x, y: Integer;
  color: DWord;
  pixel: PByte;
  _data: PByte;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { open the console with one page }
      console.open('Console example', 1);

      { create palette }
      palette := TPTCPalette.Create;

      { generate palette }
      for i := 0 to 255 do
        data[i] := i;

      { load palette data }
      palette.load(data);

      { set console palette }
      console.palette(palette);

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
        { lock console }
        pixels := console.lock;

        try
          { get console dimensions }
          width := console.width;
          height := console.height;
          pitch := console.pitch;

          { get console format }
          format := console.format;

          { get format information }
          bits := format.bits;
          bytes := format.bytes;

          { draw random pixels }
          for i := 1 to 100 do
          begin
            { get random position }
            x := Random(width);
            y := Random(height);

            { generate random color integer }
            color := (DWord(Random(256)) shl 0) or
                     (DWord(Random(256)) shl 8) or
                     (DWord(Random(256)) shl 16) or
                     (DWord(Random(256)) shl 24);

            { calculate pointer to pixel [x,y] }
            pixel := pixels + y * pitch + x * bytes;

            { check bits }
            case bits of
                   { 32 bits per pixel }
              32: PDWord(pixel)^ := color;
              24: begin
                { 24 bits per pixel }
                _data := pixel;
                _data[0] := (color and $000000FF) shr 0;
                _data[1] := (color and $0000FF00) shr 8;
                _data[2] := (color and $00FF0000) shr 16;
              end;
                   { 16 bits per pixel }
              16: PWord(pixel)^ := color;
                  { 8 bits per pixel }
              8: PByte(pixel)^ := color;
            end;
          end;
        finally
          { unlock console }
          console.unlock;
        end;

        { update console }
        console.update;
      end;
    finally
      palette.Free;
      console.close;
      console.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
