program tbytesof;

{$mode objfpc}{$H+}
{$apptype console}

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

function CheckWideBytes(const B: TBytes): Boolean;
const
  Etalon: array[0..7] of Byte = (
{$ifdef FPC_BIG_ENDIAN}
   00, 84, 00, 101, 00, 115, 00, 116
{$else}
   84, 00, 101, 00, 115, 00, 116, 00
{$endif}
  );
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
  U := S;
  B := BytesOf(S);
  if not CheckBytes(B) then
    halt(1);
  if StringOf(B) <> U then
    halt(2);
  B := BytesOf(S[1]);
  if not CheckBytes(B) then
    halt(3);
  B := BytesOf(U);
  if not CheckBytes(B) then
    halt(4);
  B := BytesOf(U[1]);
  if not CheckBytes(B) then
    halt(5);
  B := WideBytesOf(U);
  if not CheckWideBytes(B) then
    halt(6);
  if WideStringOf(B) <> U then
    halt(7);
end.