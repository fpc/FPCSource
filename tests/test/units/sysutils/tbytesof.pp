program tbytesof;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

function CheckBytes(const B: TBytes): Boolean;
const
  Etalon: array[0..3] of Byte = (84, 101, 115, 116);
var
  I: Integer;
begin
  Result := Length(B) <= Length(Etalon);
  if Result then
    for I := Low(B) to High(B) do
      Result := Result and (B[I] = Etalon[I]);
end;

var
  S: AnsiString;
  U: UnicodeString;
  B: TBytes;
begin
  S := 'Test';
  B := BytesOf(S);
  if not CheckBytes(B) then
    halt(1);
  B := BytesOf(S[1]);
  if not CheckBytes(B) then
    halt(2);
  U := S;
  B := BytesOf(U);
  if not CheckBytes(B) then
    halt(3);
  B := BytesOf(U[1]);
  if not CheckBytes(B) then
    halt(4);
end.
