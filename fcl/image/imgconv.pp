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

uses FPImage, FPWriteXPM, {FPWritePNG,} FPReadXPM, FPReadPNG, sysutils;

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
    Writer := TFPWriterXPM.Create;
  img := TFPMemoryImage.Create(1,1);
end;

procedure ReadImage;
begin
  img.LoadFromFile (paramstr(2), Reader);
end;

procedure WriteImage;
begin
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
