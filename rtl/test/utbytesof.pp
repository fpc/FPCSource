unit utbytesof;

{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes;

Implementation

uses punit,utrtl;

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

Function CheckBytesOf : AnsiString;

var
  S: AnsiString;
  U: UnicodeString;
  B: TBytes;
begin
  Result:='';
  S := 'Test';
  U := S;
  B := BytesOf(S);
  if not CheckBytes(B) then
    Exit('Error at 1');
  if StringOf(B) <> U then
    Exit('Error at 2');
  B := BytesOf(S[1]);
  if not CheckBytes(B) then
    Exit('Error at 3');
  B := BytesOf(U);
  if not CheckBytes(B) then
    Exit('Error at 4');
  B := BytesOf(U[1]);
  if not CheckBytes(B) then
    Exit('Error at 5');
  B := WideBytesOf(U);
  if not CheckWideBytes(B) then
    Exit('Error at 6');
  if WideStringOf(B) <> U then
    Exit('Error at 7');
end;
    
begin
  SysUtilsTest('BytesOf',@CheckBytesOf);    
end.
