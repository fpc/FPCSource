program tbytesstreamtest;

{$mode objfpc}{$H+}
{$apptype console}

uses
  SysUtils, Classes;

var
  BS: TBytesStream;
  MS: TMemoryStream;
  B: TBytes;
begin
  B := TBytes.Create(1, 2, 3);
  BS := TBytesStream.Create(B);
  WriteLn(BS.Size);

  // save it to regular memory stream
  MS := TMemoryStream.Create;
  try
    BS.SaveToStream(MS);
  finally
    BS.Free;
  end;

  // now restore and compare
  BS := TBytesStream.Create;
  try
    MS.Position := 0;
    BS.LoadFromStream(MS);
    B := BS.Bytes;
    if (Length(B) < 3) or (B[0] <> 1) or (B[1] <> 2) or (B[2] <> 3) then
      halt(1);
  finally
    BS.Free;
  end;
  MS.Free;
end.
