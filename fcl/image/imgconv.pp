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
