program tw16161;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

uses
  Classes, {$ifdef fpc}Types{$else}ActiveX{$endif};

var
  Stream1: TMemoryStream;
  _Stream1, _Stream2: IStream;
  cbRead, cbWritten: LargeInt;
  cbRead1: DWord;
  NewPos: Int64;
  buf: array[0..3] of char;
begin
  Stream1 := TMemoryStream.Create;
  Stream1.Write('test', 4);
  Stream1.Position := 0;
  _Stream1 := TStreamAdapter.Create(Stream1, soReference);
  _Stream1.SetSize(3);
  _Stream2 := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
  _Stream1.CopyTo(_Stream2, 4, cbRead, cbWritten);
  _Stream2.Seek(0, STREAM_SEEK_SET, NewPos);
  if (cbRead <> 3) or (cbWritten <> 3) then
    halt(1);
  _Stream2.Read(@buf[0], cbRead, @cbRead1);
  if (buf[0] <> 't') or (buf[1] <> 'e') or (buf[2] <> 's') then
    halt(2);
  if (cbRead <> 3) or (cbRead1 <> 3) then
    halt(3);
  Stream1.Free;
end.