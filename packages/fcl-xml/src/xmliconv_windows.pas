{
    This file is part of the Free Component Library

    libiconv-based XML decoder (Windows version).
    Binds to the native (not Cygwin or Mingw) build of libiconv.
    Copyright (c) 2009 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit xmliconv_windows;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc}
{$h+}

interface

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  Xml.Read;
{$ELSE FPC_DOTTEDUNITS}
uses
  xmlread;
{$ENDIF FPC_DOTTEDUNITS}

type
  iconv_t = Pointer;

const
  iconvlib = 'iconv.dll';

function iconv_open(ToCode, FromCode: PAnsiChar): iconv_t; cdecl; external iconvlib name 'libiconv_open';
function iconv(__cd: iconv_t; __inbuf: PPAnsiChar; var __inbytesleft: size_t; __outbuf:PPAnsiChar; var __outbytesleft: size_t): size_t; cdecl; external iconvlib name 'libiconv';
function iconv_close(cd: iconv_t): Integer; cdecl; external iconvlib name 'libiconv_close';

function errno_location: PInteger; cdecl; external 'msvcrt.dll' name '_errno';

function Iconv_Decode(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
var
  OutChars: size_t;
  InChars : size_t;
begin
  OutChars := OutCnt * sizeof(WideChar);
  InChars:=InCnt;
  Result := iconv(Context, @InBuf, InChars, @OutBuf, OutChars);
  InCnt:=InChars;
  OutCnt := OutChars div sizeof(WideChar);
  if Result = -1 then
  begin
    case errno_location^ of
// when iconv reports insufficient input or output space, still return
// a positive number of converted chars
      7, 22:  Result := OutCnt - (OutChars div sizeof(WideChar));
    else
      Result := -errno_location^;
    end;
  end;
end;

procedure Iconv_Cleanup(Context: Pointer); stdcall;
begin
  iconv_close(Context);
end;

function GetIconvDecoder(const AEncoding: string; out Decoder: TDecoder): Boolean; stdcall;
var
  f: iconv_t;
begin
  f := iconv_open('UCS-2-INTERNAL', PAnsiChar(AEncoding));
  if f <> Pointer(-1) then
  begin
    Decoder.Context := f;
    Decoder.Decode := @Iconv_Decode;
    Decoder.Cleanup := @Iconv_Cleanup;
    Result := True;
  end
  else
    Result := False;
end;

initialization
  RegisterDecoder(@GetIconvDecoder);

end.
