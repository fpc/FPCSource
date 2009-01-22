{
    This file is part of the Free Component Library

    libiconv-based XML decoder.
    Copyright (c) 2009 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit xmliconv;

interface

implementation

uses
  xmlread, iconvenc, unixtype, baseunix, initc;

const
{$ifdef FPC_LITTLE_ENDIAN}
  utf16_encoding = 'UTF-16LE';
{$else  FPC_LITTLE_ENDIAN}
  utf16_encoding = 'UTF-16BE';
{$endif  FPC_LITTLE_ENDIAN}

function Iconv_Decode(Context: Pointer; InBuf: PChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
var
  OutChars: size_t;
  InChars: size_t;
begin
  OutChars := OutCnt * sizeof(WideChar);
  InChars := InCnt;
  Result := iconv(Context, @InBuf, @InChars, @OutBuf, @OutChars);
  InCnt := InChars;
  OutCnt := OutChars div sizeof(WideChar);
  if Result = -1 then
  begin
    case cerrno of
// when iconv reports insufficient input or output space, still return
// a positive number of converted chars
      ESysE2BIG, ESysEINVAL:
        Result := OutCnt - (OutChars div sizeof(WideChar));
    else
      Result := -cerrno;
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
  f := iconv_open(utf16_encoding, PChar(AEncoding));
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
