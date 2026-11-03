{ Copyright (C) <2005> <Andrew Haines> htmlutil.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
{ modified from jsFastHtmlParser  for use with freepascal

 Original Author:
  James Azarja

 Contributor:
  Lars aka L505
  http://z505.com

 Note: this isn't perfect, it needs to be improved.. see comments  }

{$IFNDEF FPC_DOTTEDUNITS}
unit HTMLUtil;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef fpc}
{$MODE Delphi}
{$H+}
{$endif}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.StrUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, strutils;
{$ENDIF FPC_DOTTEDUNITS}

{ most commonly used }
function GetVal(const tag, attribname_ci: string): string;
function GetTagName(const Tag: string): string;

{ less commonly used, but useful }
function GetUpTagName(const tag: string): string;
function GetNameValPair(const tag, attribname_ci: string): string;
function GetValFromNameVal(const namevalpair: string): string;

{ old buggy code}
function GetVal_JAMES(tag, attribname_ci: string): string;
function GetNameValPair_JAMES(tag, attribname_ci: string): string;

{ rarely needed NAME= case sensitivity }
function GetNameValPair_cs(tag, attribname: string): string;

{ replaces the character references standing for ASCII characters, such as
  &lt; or &#39;, by the character }
function DecodeCharRefs(const s: string): string;


implementation

function CopyBuffer(StartIndex: PAnsiChar; Len: integer): string;
var s : String;
begin
  SetLength(s, Len);
  StrLCopy(@s[1], StartIndex, Len);
  result:= s;
end;

{ Return tag name, case preserved }
function GetTagName(const Tag: string): string;
var
  P : PAnsiChar;
  S : PAnsiChar;
begin
  P := PAnsiChar(Tag);
  while P^ in ['<',' ',#9] do
    inc(P);
  S := P;
  while Not (P^ in [' ','>',#0]) do
    inc(P);
  if P > S then
    Result := CopyBuffer( S, P-S)
  else
   Result := '';
end;

{ Return tag name in uppercase }
function GetUpTagName(const tag: string): string;
var
  P : PAnsiChar;
  S : PAnsiChar;
begin
  P := PAnsiChar(uppercase(Tag));
  while P^ in ['<',' ',#9] do
    inc(P);
  S := P;
  while Not (P^ in [' ','>',#0]) do
    inc(P);
  if P > S then
    Result := CopyBuffer( S, P-S)
  else
   Result := '';
end;


{ Return name=value pair ignoring case of NAME, preserving case of VALUE
  Lars' fixed version }
function GetNameValPair(const tag, attribname_ci: string): string;
var
  P    : PAnsiChar;
  S    : PAnsiChar;
  UpperTag,
  UpperAttrib   : string;
  Start: integer;
  L    : integer;
  C    : AnsiChar;
begin
  // must be space before case insensitive NAME, i.e. <a HREF="" STYLE=""
  UpperAttrib:= ' ' + Uppercase(attribname_ci);
  UpperTag:= Uppercase(Tag);
  P:= PAnsiChar(UpperTag);
  S:= StrPos(P, PAnsiChar(UpperAttrib));

  if S <> nil then
  begin
    inc(S); // skip space
    P:= S;

    // Skip tag name
    while not (P^ in ['=', ' ', '>', #0]) do
      inc(P);

    // Skip spaces and '='
    while (P^ in ['=', ' ']) do
      inc(P);

    while not (P^ in [' ','>',#0]) do
    begin
      if (P^ in ['"','''']) then
      begin
        C:= P^;
        inc(P); { Skip quote }
      end else
        C:= ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then inc(P); { Skip current character, except '>' }

      break;
    end;

    L:= P - S;
    Start:= S - PAnsiChar(UpperTag);
    P:= PAnsiChar(Tag);
    S:= P;
    inc(S, Start);

    result:= CopyBuffer(S, L);
  end;
end;


{ Get value of attribute, e.g WIDTH=36 -return-> 36, preserves case sensitive }
function GetValFromNameVal(const namevalpair: string): string;
var
  P: PAnsiChar;
  S: PAnsiChar;
  C: AnsiChar;
begin
  Result := '';

  P:= PAnsiChar(namevalpair);
  S:= StrPos(P, '=');

  if S <> nil then
  begin
    inc(S); // skip equal
    while S^ = ' ' do inc(S);  // skip any spaces after =
    P:= S;  // set P to a character after =

    if (P^ in ['"','''']) then
    begin
      C:= P^;
      Inc(P); { Skip current character }
    end else
      C:= ' ';

    S:= P;
    while not (P^ in [C, #0]) do
      inc(P);

    if (P <> S) then { Thanks to Dave Keighan (keighand@yahoo.com) }
      Result:= CopyBuffer(S, P - S);
  end;
end;


{ return value of an attribute (attribname_ci), case ignored for NAME portion, but return value case is preserved }
function GetVal(const tag, attribname_ci: string): string;
var namevalpair: string;
begin
  // returns full name=value pair
  namevalpair:= GetNameValPair(tag, attribname_ci);
  // extracts value portion only
  result:= GetValFromNameVal(namevalpair);
end;


{ ----------------------------------------------------------------------------
  BELOW FUNCTIONS ARE OBSOLETE OR RARELY NEEDED SINCE THEY EITHER CONTAIN BUGS
  OR THEY ARE TOO CASE SENSITIVE (FOR THE TAG NAME PORTION OF THE ATTRIBUTE  }

{ James old buggy code for testing purposes.
  Bug: when finding 'ID', function finds "width", even though width <> "id" }
function GetNameValPair_JAMES(tag, attribname_ci: string): string;
var
  P    : PAnsiChar;
  S    : PAnsiChar;
  UT,
  UA   : string;
  Start: integer;
  L    : integer;
  C    : AnsiChar;
begin
  UA:= Uppercase(attribname_ci);
  UT:= Uppercase(Tag);
  P:= PAnsiChar(UT);
  S:= StrPos(P, PAnsiChar(UA));
  if S <> nil then
  begin

    P := S;

    // Skip attribute name
    while not (P^ in ['=',' ','>',#0]) do
      inc(P);

    if (P^ = '=') then
       inc(P);

    while not (P^ in [' ','>',#0]) do
    begin

      if (P^ in ['"','''']) then
      begin
        C:= P^;
        inc(P); { Skip current character }
      end else
        C:= ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        Inc(P);

      if (P^ <> '>') then inc(P); { Skip current character, except '>' }
      break;
    end;

    L:= P - S;
    Start:= S - PAnsiChar(UT);
    P:= PAnsiChar(Tag);
    S:= P;
    inc(S, Start);
    result:= CopyBuffer(S, L);
  end;
end;


{ James old buggy code for testing purposes }
function GetVal_JAMES(tag, attribname_ci: string): string;
var namevalpair: string;
begin
  namevalpair:= GetNameValPair_JAMES(tag, attribname_ci);
  result:= GetValFromNameVal(namevalpair);
end;

{ return name=value portion, case sensitive, case preserved }
function GetNameValPair_cs(Tag, attribname: string): string;
var
  P    : PAnsiChar;
  S    : PAnsiChar;
  C    : AnsiChar;
begin
  P := PAnsiChar(Tag);
  S := StrPos(P, PAnsiChar(attribname));
  if S<>nil then
  begin
    P := S;

    // Skip attribute name
    while not (P^ in ['=',' ','>',#0]) do
      inc(P);

    if (P^ = '=') then
      inc(P);

    while not (P^ in [' ','>',#0]) do
    begin

      if (P^ in ['"','''']) then
      begin
        C:= P^;
        inc(P); { Skip current character }
      end else
        C:= ' ';

      { thanks to Dmitry [mail@vader.ru] }
      while not (P^ in [C, '>', #0]) do
        inc(P);

      if (P^<>'>') then
        inc(P); { Skip current character, except '>' }
      break;
    end;

    if P > S then
      Result:= CopyBuffer(S, P - S)
    else
      Result:= '';
  end;
end;

{ Only the references to ASCII characters are replaced, as they are the same
  bytes whatever the encoding of the text. The others are left as they are. }
function DecodeCharRefs(const s: string): string;
var
  i, j, k, code: integer;
  ref: string;
begin
  if Pos('&', s) = 0 then
  begin
    result:= s;
    exit;
  end;
  result:= '';
  i:= 1;
  while i <= Length(s) do
  begin
    if s[i] = '&' then
    begin
      j:= i + 1;
      while (j <= Length(s)) and (j - i <= 10) and not (s[j] in [';', '&', ' ']) do
        inc(j);
      if (j <= Length(s)) and (s[j] = ';') then
      begin
        ref:= Copy(s, i + 1, j - i - 1);
        code:= -1;
        if ref = 'lt' then code:= Ord('<')
        else if ref = 'gt' then code:= Ord('>')
        else if ref = 'amp' then code:= Ord('&')
        else if ref = 'quot' then code:= Ord('"')
        else if ref = 'apos' then code:= Ord('''')
        else if (Length(ref) > 1) and (ref[1] = '#') then
        begin
          // digits only: StrToInt would also take "&#0x41;" or "&#$41;"
          code:= 0;
          if ref[2] in ['x', 'X'] then
          begin
            if Length(ref) = 2 then
              code:= -1;
            for k:= 3 to Length(ref) do
              if (code >= 0) and (ref[k] in ['0'..'9', 'a'..'f', 'A'..'F']) then
                code:= code * 16 + Pos(UpCase(ref[k]), '0123456789ABCDEF') - 1
              else
                code:= -1;
          end
          else
            for k:= 2 to Length(ref) do
              if (code >= 0) and (ref[k] in ['0'..'9']) then
                code:= code * 10 + Ord(ref[k]) - Ord('0')
              else
                code:= -1;
        end;
        if (code > 0) and (code < 128) then
        begin
          result:= result + Chr(code);
          i:= j + 1;
          continue;
        end;
      end;
    end;
    result:= result + s[i];
    inc(i);
  end;
end;


end.





(* alternative, not needed

{ return value (case preserved) from a name=value pair, ignores case in given NAME= portion }
function GetValFromNameVal(namevalpair: string): string;

  type
    TAttribPos = record
      startpos: longword; // start pos of value
      len: longword;      // length of value
    end;

  { returns case insensitive start position and length of just the value
    substring in name=value pair}
  function ReturnPos(attribute: string): TAttribPos;
  var
    P    : PAnsiChar;
    S    : PAnsiChar;
    C    : AnsiChar;
  begin
    result.startpos:= 0;
    result.len:= 0;
    P:= PAnsiChar(uppercase(Attribute));
    // get substring including and everything after equal
    S:= StrPos(P, '=');
    result.startpos:= pos('=', P);

    if S <> nil then
    begin
      inc(S);
      // set to character after =
      inc(result.startpos);
      P:= S;

      if (P^ in ['"','''']) then
      begin
        C:= P^;
        // skip quote
        inc(P);
        inc(result.startpos);
      end else
        C:= ' ';

      S:= P;
      // go to end quote or end of value
      while not (P^ in [C, #0]) do
        inc(P);

      if (P <> S) then
      begin
        result.len:= p - s;
      end;
    end;

  end;

var
  found: TAttribPos;
begin
  found:= ReturnPos(namevalpair);
  // extract using coordinates
  result:= MidStr(namevalpair, found.startpos, found.len);
end;

*)




