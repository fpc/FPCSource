{$MODE objfpc}

program b64test;
uses classes, base64, sysutils;
var
  b64encoder: TBase64EncodingStream;
  b64decoder: TBase64DecodingStream;
  BaseStream: TStream;
  i, j: Integer;
begin
  BaseStream := TMemoryStream.Create;

  WriteLn('Encoded Size / Decoded Size / Data:');

  for i := 1 to 22 do begin
    BaseStream.Position := 0;

    b64encoder := TBase64EncodingStream.Create(BaseStream);
    for j := 1 to i do
      b64encoder.WriteByte(i - j + 65);
    Write(b64encoder.Size: 2, ' ');
    b64encoder.Free;

    BaseStream.Position := 0;

    b64decoder := TBase64DecodingStream.Create(BaseStream);
    Write(b64decoder.Size: 2, ' ');
    for j := 1 to i do
      Write(Chr(b64decoder.ReadByte));
    WriteLn;
    b64decoder.Free;
  end;

  BaseStream.Free;
end.
