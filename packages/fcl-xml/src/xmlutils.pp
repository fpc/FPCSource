{
    This file is part of the Free Component Library

    XML utility routines.
    Copyright (c) 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit xmlutils;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils;

function IsXmlName(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsXmlNames(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsXmlNmToken(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsXmlNmTokens(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsValidXmlEncoding(const Value: WideString): Boolean;
function Xml11NamePages: PByteArray;
procedure NormalizeSpaces(var Value: WideString);

{$i names.inc}

implementation

var
  Xml11Pg: PByteArray = nil;

function Xml11NamePages: PByteArray;
var
  I: Integer;
  p: PByteArray;
begin
  if Xml11Pg = nil then
  begin
    GetMem(p, 512);
    for I := 0 to 255 do
      p^[I] := ord(Byte(I) in Xml11HighPages);
    p^[0] := 2;
    p^[3] := $2c;
    p^[$20] := $2a;
    p^[$21] := $2b;
    p^[$2f] := $29;
    p^[$30] := $2d;
    p^[$fd] := $28;

    Move(p^, p^[256], 256);
    p^[$100] := $19;
    p^[$103] := $2E;
    p^[$120] := $2F;
    Xml11Pg := p;
  end;
  Result := Xml11Pg;
end;

function IsXml11Char(const Value: WideString; var Index: Integer): Boolean;
begin
  if (Value[Index] >= #$D800) and (Value[Index] <= #$DB7F) then
  begin
    Inc(Index);
    Result := (Value[Index] >= #$DC00) and (Value[Index] <= #$DFFF);
  end
  else
    Result := False;
end;

function IsXmlName(const Value: WideString; Xml11: Boolean): Boolean;
var
  Pages: PByteArray;
  I: Integer;
begin
  Result := False;
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;

  I := 1;
  if (Value = '') or not ((Byte(Value[I]) in NamingBitmap[Pages^[hi(Word(Value[I]))]]) or
    (Xml11 and IsXml11Char(Value, I))) then
      Exit;
  Inc(I);
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Xml11 and IsXml11Char(Value, I))) then
        Exit;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNames(const Value: WideString; Xml11: Boolean): Boolean;
var
  Pages: PByteArray;
  I: Integer;
  Offset: Integer;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  Result := False;
  if Value = '' then
    Exit;
  I := 1;
  Offset := 0;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[Offset+hi(Word(Value[I]))]]) or
      (Xml11 and IsXml11Char(Value, I))) then
    begin
      if (I = Length(Value)) or (Value[I] <> #32) then
        Exit;
      Offset := 0;
      Inc(I);
      Continue;
    end;
    Offset := $100;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNmToken(const Value: WideString; Xml11: Boolean): Boolean;
var
  I: Integer;
  Pages: PByteArray;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  Result := False;
  if Value = '' then
    Exit;
  I := 1;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Xml11 and IsXml11Char(Value, I))) then
        Exit;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNmTokens(const Value: WideString; Xml11: Boolean): Boolean;
var
  I: Integer;
  Pages: PByteArray;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  I := 1;
  Result := False;
  if Value = '' then
    Exit;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Xml11 and IsXml11Char(Value, I))) then
    begin
      if (I = Length(Value)) or (Value[I] <> #32) then
        Exit;
    end;
    Inc(I);
  end;
  Result := True;
end;

function IsValidXmlEncoding(const Value: WideString): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Value = '') or (Value[1] > #255) or not (char(ord(Value[1])) in ['A'..'Z', 'a'..'z']) then
    Exit;
  for I := 2 to Length(Value) do
    if (Value[I] > #255) or not (char(ord(Value[I])) in ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) then
      Exit;
  Result := True;
end;

procedure NormalizeSpaces(var Value: WideString);
var
  I, J: Integer;
begin
  I := Length(Value);
  // speed: trim only whed needed
  if (I > 0) and ((Value[1] = #32) or (Value[I] = #32)) then
    Value := Trim(Value);
  I := 1;
  while I < Length(Value) do
  begin
    if Value[I] = #32 then
    begin
      J := I+1;
      while (J <= Length(Value)) and (Value[J] = #32) do Inc(J);
      if J-I > 1 then Delete(Value, I+1, J-I-1);
    end;
    Inc(I);
  end;
end;

initialization

finalization
  if Assigned(Xml11Pg) then
    FreeMem(Xml11Pg);

end.
