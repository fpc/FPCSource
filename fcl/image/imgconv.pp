{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Image conversion example.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
program ImgConv;

uses FPImage, FPWriteXPM, FPWritePNG, FPReadXPM, FPReadPNG, sysutils;

var img : TFPMemoryImage;
    reader : TFPCustomImageReader;
    Writer : TFPCustomimageWriter;

procedure Init;
var t : char;
begin
  T := upcase (paramstr(1)[1]);
  if T = 'X' then
    Reader := TFPReaderXPM.Create
  else
    Reader := TFPReaderPNG.Create;
  T := upcase (paramstr(3)[1]);
  if T = 'X' then
    Writer := TFPWriterXPM.Create
  else
    Writer := TFPWriterPNG.Create;
  img := TFPMemoryImage.Create(1,1);
end;

procedure ReadImage;
begin
  img.LoadFromFile (paramstr(2), Reader);
end;

procedure WriteImage;
var t : string;
begin
  t := UpperCase(paramstr(3));
  if (t[1] = 'P') then
    with (Writer as TFPWriterPNG) do
      begin
      Grayscale := pos ('G', t) > 0;
      Indexed := pos ('I', t) > 0;
      WordSized := pos('W', t) > 0;
      UseAlpha := pos ('A', t) > 0;
      writeln ('Grayscale ',Grayscale, ' - Indexed ',Indexed,
               ' - WordSized ',WordSized,' - UseAlpha ',UseAlpha);
      end
  else if (t[1] = 'X') then
    with (Writer as TFPWriterXPM) do
      begin
      ColorCharSize := ord(t[2]) - ord('0');
      end;
  img.SaveToFile (paramstr(4), Writer);
end;

procedure Clean;
begin
  Reader.Free;
  Writer.Free;
  Img.Free;
end;

begin
  if paramcount <> 4 then
    begin
    writeln ('Give filename to read and to write, preceded by filetype:');
    writeln ('X for XPM, P for PNG');
    writeln ('example: imgconv X hello.xpm P hello.png');
    writeln ('  The PNG has settings when writing:  G : grayscale,');
    writeln ('    A : use alpha, I : Indexed in palette, W : Word sized.');
    writeln ('  The color size of an XPM can be set after the X as 1,2,3 or 4');
    writeln ('example: imgconv X hello.xpm PIA hello.png');
    writeln ('example: imgconv P hello.png X2 hello.xpm');
    end
  else
    try
      Init;
      ReadImage;
      WriteImage;
      Clean;
    except
      on e : exception do
        writeln ('Error: ',e.message);
    end;
end.
