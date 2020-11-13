{
    Delphi/Kylix compatibility unit: String handling routines.

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit widestrutils;

{$mode objfpc}
{$H+}
{$inline on}

interface

uses
  SysUtils, Classes;

function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;
function WideReplaceStr(const AText, AFromText, AToText: WideString): WideString; inline;
function WideReplaceText(const AText, AFromText, AToText: WideString): WideString; inline;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString;
function UnicodeReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
function UnicodeReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;

type
  TEncodeType = (etUSASCII, etUTF8, etANSI);

const
  sUTF8BOMString: array[1..3] of char = (#$EF, #$BB, #$BF);

function HasUTF8BOM(S: TStream): boolean; overload;
function HasUTF8BOM(const S: RawByteString): boolean; overload;
function HasExtendCharacter(const S: RawByteString): boolean;
function DetectUTF8Encoding(const S: RawByteString): TEncodeType;
function IsUTF8String(const S: RawByteString): boolean;

type
  TBufferUTF8State = (u8sUnknown, u8sYes, u8sNo);

//PartialAllowed must be set to true if the buffer is smaller than the file.
function IsBufferUTF8(buf: PAnsiChar; bufSize: SizeInt; PartialAllowed: boolean): TBufferUTF8State;

implementation

{
  The IsBufferUtf8 function code was created by Christian Ghisler (ghisler.com)
  Christian gave code to open-source at Total Commander public forum
}

const bytesFromUTF8:array[AnsiChar] of byte = (
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 32
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 64
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 96
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //128
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //160
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //192
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  //224
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5); //256


function IsFirstUTF8Char(thechar:AnsiChar):boolean; inline;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))<>128;
end;

function IsSecondaryUTF8Char(thechar:AnsiChar):boolean; inline;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))=128;
end;

function IsBufferUTF8(buf: PAnsiChar; bufSize: SizeInt; PartialAllowed: boolean): TBufferUTF8State;
{Buffer contains only valid UTF-8 characters, no secondary alone,
no primary without the correct nr of secondary}
var
  p: PAnsiChar;
  i: SizeInt;
  utf8bytes: integer;
  hadutf8bytes: boolean;
begin
  p:=buf;
  hadutf8bytes:=false;
  result:=u8sUnknown;
  utf8bytes:=0;
  for i:= 1 to bufSize do
  begin
    if utf8bytes>0 then
    begin  {Expecting secondary AnsiChar}
      hadutf8bytes:=true;
      if not IsSecondaryUTF8Char(p^) then exit(u8sNo);  {Fail!}
      dec(utf8bytes);
    end
    else
    if IsFirstUTF8Char(p^) then
      utf8bytes:=bytesFromUTF8[p^]
    else
    if IsSecondaryUTF8Char(p^) then
      exit(u8sNo);  {Fail!}
    inc(p);
  end;
  if hadutf8bytes and (PartialAllowed or (utf8bytes=0)) then
    result:=u8sYes;
end;

function WideReplaceStr(const AText, AFromText, AToText: WideString): WideString; inline;
begin
  Result := WideStringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function WideReplaceText(const AText, AFromText, AToText: WideString): WideString; inline;
begin
  Result := WideStringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function UnicodeReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
begin
  Result := UnicodeStringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function UnicodeReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
begin
  Result := UnicodeStringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

Function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;

begin
  Result:= sysutils.WideStringReplace(S,OldPattern,NewPattern,Flags);
end;

Function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString;

begin
  Result:= sysutils.UnicodeStringReplace(S,OldPattern,NewPattern,Flags);
end;

function HasUTF8BOM(S: TStream): boolean;
var
  OldPos: Int64;
  Buf: array[1..3] of char;
begin
  Result := false;
  if S.Size<3 then exit;
  FillChar(Buf, SizeOf(Buf), 0);
  try
    OldPos := S.Position;
    S.Position := 0;
    if S.Read(Buf, 3)<>3 then exit;
    Result :=
      (Buf[1]=sUTF8BOMString[1]) and
      (Buf[2]=sUTF8BOMString[2]) and
      (Buf[3]=sUTF8BOMString[3]);
  finally
    S.Position := OldPos;
  end;
end;

function HasUTF8BOM(const S: RawByteString): boolean;
begin
  Result := (Length(S)>=3) and
    (S[1]=sUTF8BOMString[1]) and
    (S[2]=sUTF8BOMString[2]) and
    (S[3]=sUTF8BOMString[3]);
end;

function HasExtendCharacter(const S: RawByteString): boolean;
var
  i: integer;
begin
  for i := 1 to Length(S) do
    if Ord(S[i])>=$80 then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function DetectUTF8Encoding(const S: RawByteString): TEncodeType;
var
  FirstExtChar, i: integer;
begin
  FirstExtChar := 0;
  for i := 1 to Length(S) do
    if Ord(S[i])>=$80 then
    begin
      FirstExtChar := i;
      Break;
    end;

  if FirstExtChar=0 then
    Result := etUSASCII
  else
  if IsBufferUtf8(@S[FirstExtChar], Length(S)-FirstExtChar+1, false)=u8sYes then
    Result := etUTF8
  else
    Result := etANSI;
end;

function IsUTF8String(const S: RawByteString): boolean;
begin
  Result := DetectUTF8Encoding(S) = etUTF8;
end;


end.

