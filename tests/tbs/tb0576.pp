{ %norun }

{$mode delphi}

type ByteArray = array of byte;

procedure ZeroMemory(var X: array of byte; StartIndex, Count: integer); overload;
begin
end;

procedure ZeroMemory(var X: array of ByteArray; StartIndex, Count: integer); overload;
begin
end;

var
  buffer: array of byte;
begin
  ZeroMemory(Buffer, 0, Length(Buffer));
end.

