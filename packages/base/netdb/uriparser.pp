{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team
    Original author: Sebastian Guenther

    Unit to parse complete URI in its parts or to reassemble an URI

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

unit URIParser;

interface

type
  TURI = record
    Protocol: String;
    Username: String;
    Password: String;
    Host: String;
    Port: Word;
    Path: String;
    Document: String;
    Params: String;
    Bookmark: String;
  end;

function EncodeURI(const URI: TURI): String;
function ParseURI(const URI: String):  TURI;
function ParseURI(const URI, DefaultProtocol: String; DefaultPort: Word):  TURI;


implementation

uses SysUtils;

const
  HexTable: array[0..15] of Char = '0123456789abcdef';


function EncodeURI(const URI: TURI): String;

  function Escape(const s: String): String;
  var
    i: Integer;
  begin
    SetLength(Result, 0);
    for i := 1 to Length(s) do
      if not (s[i] in ['0'..'9', 'A'..'Z', 'a'..'z', ',', '-', '.', '_',
        '/', '\']) then
        Result := Result + '%' + HexTable[Ord(s[i]) shr 4] +
          HexTable[Ord(s[i]) and $f]
      else
        Result := Result + s[i];
  end;

begin
  SetLength(Result, 0);
  if Length(URI.Protocol) > 0 then
    Result := LowerCase(URI.Protocol) + ':';
  if Length(URI.Host) > 0 then
  begin
    Result := Result + '//';
    if Length(URI.Username) > 0 then
    begin
      Result := Result + URI.Username;
      if Length(URI.Password) > 0 then
        Result := Result + ':' + URI.Password;
      Result := Result + '@';
    end;
    Result := Result + URI.Host;
  end;
  if URI.Port <> 0 then
    Result := Result + ':' + IntToStr(URI.Port);
  Result := Result + Escape(URI.Path);
  if Length(URI.Document) > 0 then
  begin
    if (Length(Result) = 0) or (Result[Length(Result)] <> '/') then
      Result := Result + '/';
    Result := Result + Escape(URI.Document);
  end;
  if Length(URI.Params) > 0 then
    Result := Result + '?' + URI.Params;
  if Length(URI.Bookmark) > 0 then
    Result := Result + '#' + Escape(URI.Bookmark);
end;

function ParseURI(const URI: String):  TURI;
begin
  Result := ParseURI(URI, '', 0);
end;

function ParseURI(const URI, DefaultProtocol: String; DefaultPort: Word):  TURI;

  function Unescape(const s: String): String;

    function HexValue(c: Char): Integer;
    begin
      if (c >= '0') and (c <= '9') then
        Result := Ord(c) - Ord('0')
      else if (c >= 'A') and (c <= 'F') then
        Result := Ord(c) - Ord('A') + 10
      else if (c >= 'a') and (c <= 'f') then
        Result := Ord(c) - Ord('a') + 10
      else
        Result := 0;
    end;

  var
    i, RealLength: Integer;
  begin
    SetLength(Result, Length(s));
    i := 1;
    RealLength := 0;
    while i <= Length(s) do
    begin
      Inc(RealLength);
      if s[i] = '%' then
      begin
        Result[RealLength] := Chr(HexValue(s[i + 1]) shl 4 or HexValue(s[i + 2]));
        Inc(i, 3);
      end else
      begin
        Result[RealLength] := s[i];
        Inc(i);
      end;
    end;
    SetLength(Result, RealLength);
  end;

var
  s: String;
  i, LastValidPos: Integer;
begin
  Result.Protocol := LowerCase(DefaultProtocol);
  Result.Port := DefaultPort;

  s := URI;

  // Extract the protocol

  for i := 1 to Length(s) do
    if s[i] = ':' then
    begin
      Result.Protocol := Copy(s, 1, i - 1);
      s := Copy(s, i + 1, Length(s));
      break;
    end else if not (s[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) then
      break;

  // Extract the bookmark name

  for i := Length(s) downto 1 do
    if s[i] = '#' then
    begin
      Result.Bookmark := Unescape(Copy(s, i + 1, Length(s)));
      s := Copy(s, 1, i - 1);
      break;
    end else if s[i] = '/' then
      break;

  // Extract the params

  for i := Length(s) downto 1 do
    if s[i] = '?' then
    begin
      Result.Params := Copy(s, i + 1, Length(s));
      s := Copy(s, 1, i - 1);
      break;
    end else if s[i] = '/' then
      break;

  // Extract the document name

  for i := Length(s) downto 1 do
    if s[i] = '/' then
    begin
      Result.Document := Unescape(Copy(s, i + 1, Length(s)));
      s := Copy(s, 1, i - 1);
      break;
    end else if s[i] = ':' then
      break;

  // Extract the path

  LastValidPos := 0;
  for i := Length(s) downto 1 do
    if (s[i] = '/')
       and ((I>1) and (S[i-1]<>'/'))
       and ((I<Length(S)) and (S[I+1]<>'/')) then
      LastValidPos := i
    else if s[i] in [':', '@'] then
      break;

  if (LastValidPos > 0) and
     (Length(S)>LastValidPos) and
     (S[LastValidPos+1]<>'/') then
  begin
    Result.Path := Unescape(Copy(s, LastValidPos, Length(s)));
    s := Copy(s, 1, LastValidPos - 1);
  end;

  // Extract the port number

  for i := Length(s) downto 1 do
    if s[i] = ':' then
    begin
      Result.Port := StrToInt(Copy(s, i + 1, Length(s)));
      s := Copy(s, 1, i - 1);
      break;
    end else if s[i] in ['@', '/'] then
      break;

  // Extract the hostname

  if ((Length(s) > 2) and (s[1] = '/') and (s[2] = '/')) or
    ((Length(s) > 1) and (s[1] <> '/')) then
  begin
    if s[1] <> '/' then
      s := '//' + s;
    for i := Length(s) downto 1 do
      if s[i] in ['@', '/'] then
      begin
        Result.Host := Copy(s, i + 1, Length(s));
        s := Copy(s, 3, i - 3);
        break;
      end;

    // Extract username and password
    if Length(s) > 0 then
    begin
      i := Pos(':', s);
      if i = 0 then
        Result.Username := s
      else
      begin
        Result.Username := Copy(s, 1, i - 1);
        Result.Password := Copy(s, i + 1, Length(s));
      end;
    end;
  end;
end;

end.
