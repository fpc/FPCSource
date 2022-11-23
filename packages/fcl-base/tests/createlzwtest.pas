program createlzwtest;
{
  Small program that converts TIFF LZW compressed data to uncompressed data.
  (assumes the fpreadtiff DecodeLZW is correct)
  
}

{$mode objfpc}
{$h+}

uses sysutils,classes,fpreadtiff;

Var
  M : TBytesStream;
  F : TFileStream;
  B : PByte;
  aSize : PtrInt;

begin
  If ParamCount<>2 then
    begin
    Writeln('Usage : ',ExtractFileName(ParamStr(0)),' compressedfile uncompressedfile');
    Halt(1);
    end;
  M:=TBytesStream.Create([]);
  try
    M.LoadFromFile(Paramstr(1));
    DecompressLZW(M.Memory,M.Size,B,aSize);
    F:=TFileStream.Create(ParamStr(2),fmCreate);
    F.WriteBuffer(B^,aSize);
  finally
    F.Free;
    M.Free;
  end;
end.

