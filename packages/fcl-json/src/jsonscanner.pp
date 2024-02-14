{
    This file is part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{ $INLINE ON}

{$IFNDEF FPC_DOTTEDUNITS}
unit jsonscanner;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d';
  SErrOpenString = 'string exceeds end of line %d';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joComments,joIgnoreTrailingComma,joIgnoreDuplicates,joBOMCheck,joSingle);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type

  { TJSONScanner }

  TJSONScanner = class
  private
    FCurPos, FSourceStart: PAnsiChar;
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenString: AnsiString; // Calculated lazily from FParts. FNParts = -1 if ready.

    // Describes how to build FCurTokenString if asked.
    // FParts[i] >= 0: piece with start = FSourceStart + FParts[i] and length = FParts[i + 1].
    // FParts[i] = -1 - N: Unicode codepoint N.
    FParts: array of SizeInt;
    FNParts: SizeInt; // -1 if FCurTokenString is ready.

    FOptions : TJSONOptions;
    FCurLine, FCurLineEnd: PAnsiChar; // FCurLineEnd = nil if needs to be calculated.
    FSource: RawByteString;

    class function ToOptions(AUseUTF8: boolean): TJSONOptions; static;
    function GetCurColumn: Integer; inline;
    function GetCurLine: AnsiString;
    function GetO(AIndex: TJSONOption): Boolean;
    function GetAbsolutePos: Integer;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
    function CountChars(start, ed: PAnsiChar): SizeInt;

    function GrowParts(by: SizeInt): PSizeInt;
    procedure AddPiece(start, ed: PAnsiChar);
    procedure AddCodepoint(cp: uint32);
    function GetCurTokenString: ansistring;
    procedure BuildCurTokenString;
    class function CodepointToASCII(cp: uint32; Rp: PAnsiChar): SizeInt; static;

    function ScanNewline(Sp: PAnsiChar): PAnsiChar;
    function ScanString(Sp: PAnsiChar): PAnsiChar;
    function ScanHex(Sp: PAnsiChar; out v: uint32): PAnsiChar;
    function ScanNumber(Sp: PAnsiChar): PAnsiChar;
    function ScanLineComment(Sp: PAnsiChar): PAnsiChar;
    function ScanSlashStarComment(Sp: PAnsiChar): PAnsiChar;
    function RecognizeKeyword(Sp: PAnsiChar; N: SizeInt): TJSONToken;

  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string;  Const Args: array of const);overload;
    procedure InvalidCharacter(Sp: PAnsiChar);

  public
    constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload; deprecated 'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const aSource : RawByteString; AUseUTF8 : Boolean = True); overload; deprecated  'use options form instead';
    constructor Create(const aSource: RawByteString; AOptions: TJSONOptions); overload;

    function FetchToken: TJSONToken;

    property CurLine: Ansistring read GetCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    Property AbsolutePos : Integer Read GetAbsolutePos;
    property CurToken: TJSONToken read FCurToken;
    property CurTokenString: Ansistring read GetCurTokenString;
    // Use strict JSON: " for strings, object members are strings, not identifiers
    Property Strict : Boolean Index joStrict Read GetO Write SetO ; deprecated 'use options instead';
    // if set to TRUE, then strings will be converted to UTF8 ansistrings, not system codepage ansistrings.
    Property UseUTF8 : Boolean index joUTF8 Read GetO Write SetO; deprecated 'Use options instead';
    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation


class function TJSONScanner.ToOptions(AUseUTF8: boolean): TJSONOptions;
begin
  if AUseUTF8 then
    Result := DefaultOptions + [joUTF8]
  else
    Result := DefaultOptions - [joUTF8];
end;

constructor TJSONScanner.Create(Source : TStream; AUseUTF8 : Boolean = True);
begin
  Create(Source, ToOptions(AUseUTF8));
end;

constructor TJSONScanner.Create(Source: TStream; AOptions: TJSONOptions);
begin
  SetLength(FSource,Source.Size-Source.Position);
  Source.ReadBuffer(Pointer(FSource)^,Length(FSource));
  Create(FSource,AOptions);
end;

constructor TJSONScanner.Create(const aSource : RawByteString; AUseUTF8 : Boolean = True);
begin
  Create(aSource,ToOptions(AUseUTF8));
end;

constructor TJSONScanner.Create(const aSource: RawByteString; AOptions: TJSONOptions);
var
  Sp: PAnsiChar;
begin
  FSource:=aSource;
  Sp:=PAnsiChar(FSource);
  if (joBOMCheck in aOptions) and (ord(Sp[0]) = $EF) and (ord(Sp[1]) = $BB) and (ord(Sp[2]) = $BF) then
    inc(Sp,3);
  FSourceStart:=Sp;
  FCurPos:=Sp;
  FCurLine:=Sp;
  FCurRow:=1;
  FOptions:=AOptions;
end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := FCurPos - FCurLine;
end;

procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.InvalidCharacter(Sp: PAnsiChar);
begin
  Error(SErrInvalidCharacter, [CurRow, 1 + CountChars(FCurLine, Sp), Sp^]);
end;

procedure TJSONScanner.Error(const Msg: string; const Args: array of const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.FetchToken: TJSONToken;
var
  Sp, Start: PAnsiChar;
begin
  FNParts := 0;
  Sp := FCurPos;
  // Don't consider such newline a tkWhitespace.
  if Sp^ in [#13, #10] then
    Sp := ScanNewline(Sp);
  case Sp^ of
    #0: Result := tkEOF;
    #9, ' ', #13, #10:
      begin
        while Sp^ in [#9, ' '] do
          Inc(Sp);
        Result := tkWhitespace;
      end;
    '"','''':
      begin
        Sp := ScanString(Sp);
        Result := tkString;
      end;
    ',':
      begin
        Inc(Sp);
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        Sp := ScanNumber(Sp);
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(Sp);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(Sp);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(Sp);
        Result := tkCurlyBraceClose;
      end;
    '[':
      begin
        Inc(Sp);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(Sp);
        Result := tkSquaredBraceClose;
      end;
    '/':
      begin
        if Not (joComments in Options) then
          InvalidCharacter(Sp);
        if Sp[1] = '/' then
          Sp := ScanLineComment(Sp + 2)
        else if Sp[1] = '*' then
          Sp := ScanSlashStarComment(Sp + 2)
        else
          InvalidCharacter(Sp + 1);
        Result := tkComment;
      end;
    'a'..'z','A'..'Z','_':
      begin
        Start := Sp;
        repeat
          Inc(Sp);
        until not (Sp^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        AddPiece(Start, Sp);
        Result := RecognizeKeyword(Start, Sp - Start);
      end;
    else
      InvalidCharacter(Sp);
  end;
  FCurPos := Sp;
  FCurToken := Result;
end;

function TJSONScanner.GetCurLine: Ansistring;
begin
  if not Assigned(FCurLineEnd) then
  begin
    FCurLineEnd := FCurLine;
    while not (FCurLineEnd^ in [#13, #10, #0]) do
      inc(FCurLineEnd);
  end;
  SetString(Result, FCurLine, FCurLineEnd - FCurLine);
end;

function TJSONScanner.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;

function TJSONScanner.GetAbsolutePos: Integer;
begin
  Result:=FCurPos-FSourceStart;
end;

procedure TJSONScanner.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  If AValue then
    Include(Foptions,AIndex)
  else
    Exclude(Foptions,AIndex)
end;

function TJSONScanner.CountChars(start, ed: PAnsiChar): SizeInt;
begin
  if joUTF8 in Options then
  begin
    // Count UTF-8 start bytes.
    Result := 0;
    while start < ed do
    begin
      if ord(start^) and %11000000 <> %10000000 then
        inc(Result);
      inc(start);
    end;
  end else
    Result := ed - start;
end;

function TJSONScanner.GrowParts(by: SizeInt): PSizeInt;
var
  newNParts: SizeInt;
begin
  newNParts := FNParts + by;
  if newNParts > length(FParts) then
    SetLength(FParts, 4 + newNParts + SizeUint(newNParts) div 4);
  Result := @FParts[FNParts];
  FNParts := newNParts;
end;

procedure TJSONScanner.AddPiece(start, ed: PAnsiChar);
var
  pp: PSizeInt;
begin
  if start = ed then
    exit;
  pp := GrowParts(2);
  pp[0] := start - FSourceStart;
  pp[1] := ed - start;
end;

procedure TJSONScanner.AddCodepoint(cp: uint32);
begin
  GrowParts(1)^ := -1 - SizeInt(cp);
end;

function TJSONScanner.GetCurTokenString: ansistring;
begin
  if FNParts >= 0 then
    BuildCurTokenString;
  result := FCurTokenString;
end;

procedure TJSONScanner.BuildCurTokenString;
var
  utf8: boolean;
  iPart, len: SizeInt;
  cp: uint32;
  Rp: PAnsiChar;
begin
  utf8 := (joUTF8 in Options) or (DefaultSystemCodePage=CP_UTF8);
  len := 0;
  // Prepass for length. Exact if utf8, otherwise ceiling.
  iPart := 0;
  while iPart < FNParts do
  begin
    if FParts[iPart] >= 0 then
    begin
      inc(len, FParts[iPart + 1]);
      inc(iPart, 2);
    end else
    begin
      cp := -(FParts[iPart] + 1);
      if cp <= $7F then inc(len) // First 128 characters use 1 byte both in UTF-8 or ANSI encodings.
      // Use 2 in non-utf8 mode as ceiling value, assuming ANSI encodings use at most 2 bytes per codepoint. (Eg: Shift JIS uses 1 or 2.)
      else if (cp <= $7FF) or not utf8 then inc(len, 2)
      else if cp <= $FFFF then inc(len, 3)
      else inc(len, 4);
      inc(iPart);
    end;
  end;
  SetLength(FCurTokenString, len);
  Rp := PAnsiChar(Pointer(FCurTokenString));
  iPart := 0;
  while iPart < FNParts do
  begin
    if FParts[iPart] >= 0 then
    begin
      Move(FSourceStart[FParts[iPart]], Rp^, FParts[iPart + 1]);
      inc(Rp, FParts[iPart + 1]);
      inc(iPart, 2);
    end else
    begin
      cp := -(FParts[iPart] + 1);
      if cp <= $7F then
      begin
        byte(Rp^) := cp;
        inc(Rp);
      end else
        if utf8 then
          if cp <= $7FF then
          begin
            byte(Rp^) := %11000000 or cp shr 6;
            byte(Rp[1]) := %10000000 or cp and %111111;
            Inc(Rp, 2);
          end
          else if cp <= $FFFF then
          begin
            byte(Rp^) := %11100000 or (cp shr 12);
            byte(Rp[1]) := %10000000 or cp shr 6 and %111111;
            byte(Rp[2]) := %10000000 or cp and %111111;
            Inc(Rp, 3);
          end else
          begin
            byte(Rp^) := %11110000 or cp shr 18;
            byte(Rp[1]) := %10000000 or cp shr 12 and %111111;
            byte(Rp[2]) := %10000000 or cp shr 6 and %111111;
            byte(Rp[3]) := %10000000 or cp and %111111;
            Inc(Rp, 4);
          end
        else
          Inc(Rp, CodepointToASCII(cp, Rp));
      inc(iPart);
    end;
  end;
  SetLength(FCurTokenString, Rp - PAnsiChar(Pointer(FCurTokenString)));
  FNParts := -1;
end;

class function TJSONScanner.CodepointToASCII(cp: uint32; Rp: PAnsiChar): SizeInt;
var
  s: ansistring;
begin
  if (cp <= $D7FF) or ((cp >= $E000) and (cp <= $FFFF)) then
    s := ansistring(unicodechar(cp))
  else
    s := ansistring(unicodechar($D800 + (cp - $10000) shr 10) + unicodechar($DC00 + (cp - $10000) and (1 shl 10 - 1)));
  result := length(s);
  Move(pointer(s)^, Rp^, result);
end;

function TJSONScanner.ScanNewline(Sp: PAnsiChar): PAnsiChar;
begin
  Result := Sp + 1 + ord((Sp[0] = #13) and (Sp[1] = #10));
  Inc(FCurRow);
  FCurLine := Result;
  FCurLineEnd := nil;
end;

function TJSONScanner.ScanString(Sp: PAnsiChar): PAnsiChar;
const
  SimpleEscapes_Spell: array[0 .. 8] of ansichar = 'tbnrf"''\/';
  SimpleEscapes_Meant: array[0 .. High(SimpleEscapes_Spell)] of ansichar = #9#8#10#13#12'"''\/';
var
  StartChar: AnsiChar;
  LiteralStart: PAnsiChar;
  iEsc: SizeInt;
  u, u1: uint32;
begin
  StartChar := Sp^;
  if (StartChar = '''') and (joStrict in Options) then
    InvalidCharacter(Sp);
  LiteralStart := Sp + 1;
  repeat
    Inc(Sp);
    // Fast test for irregularities instead of jumping through several 'if's each time.
    // Loop starts with an increment to improve this common case further at the cost of a bit of comprehensibility in other cases.
    if not (Sp^ in [#0 .. #31, '\', '''', '"']) then
      continue;

    if Sp^ = '\' then
    begin
      AddPiece(LiteralStart, Sp);
      if Sp[1] = 'u' then
      begin
        Sp := ScanHex(Sp + 2, u);
        if (u >= $D800) and (u <= $DBFF) then
          // High surrogate. Expect low surrogate.
          if (Sp[0] = '\') and (Sp[1] = 'u') then
          begin
            Sp := ScanHex(Sp + 2, u1);
            if (u1 >= $DC00) and (u1 <= $DFFF) then
              AddCodepoint($10000 + (u - $D800) shl 10 + (u1 - $DC00))
            else
              Error(SErrInvalidCharacter, [CurRow, 1 + CountChars(FCurLine, Sp), IntToStr(u1)]);
          end else
            Error(SErrInvalidCharacter, [CurRow, 1 + CountChars(FCurLine, Sp), IntToStr(u) + ' + ???'])
        else
          AddCodepoint(u);
        LiteralStart := Sp;
        dec(Sp);
        continue;
      end;

      iEsc := IndexByte(SimpleEscapes_Spell[0], length(SimpleEscapes_Spell), ord(Sp[1]));
      if iEsc >= 0 then
      begin
        Inc(Sp);
        LiteralStart := Sp + 1;
        if SimpleEscapes_Meant[iEsc] = SimpleEscapes_Spell[iEsc] then
          dec(LiteralStart) // Just start next literal from this very character instead of handling it explicitly somehow.
        else
          GrowParts(1)^ := -1 - ord(SimpleEscapes_Meant[iEsc]);
        continue;
      end;

      if Sp[1] = #0 then
        Error(SErrOpenString, [CurRow])
      else
        InvalidCharacter(Sp + 1);
    end;

    if Sp^ = StartChar then
      break;

    if Sp^ < #20 then
      if Sp^ = #0 then
        Error(SErrOpenString, [FCurRow])
      else if joStrict in Options then
        InvalidCharacter(Sp)
      else if Sp^ in [#13, #10] then
        Sp := ScanNewline(Sp) - 1; // Account for newlines when not joStrict.
  until false;
  AddPiece(LiteralStart, Sp);
  Result := Sp + 1;
end;

function TJSONScanner.ScanHex(Sp: PAnsiChar; out v: uint32): PAnsiChar;
var
  n: SizeInt;
begin
  v := 0;
  for n := 0 to 3 do
  begin
    if not (Sp^ in ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) then
      InvalidCharacter(Sp);
    v := v * 16;
    if Sp^ >= 'A' then // "ord('0' ~ '9') and 15" gives the corresponding number; "ord('A' ~ 'F', 'a' ~ 'f') and 15" gives the 1-based letter number.
      v := v + 9;
    v := v + ord(Sp^) and 15;
    Inc(Sp);
  end;
  Result := Sp;
end;

function TJSONScanner.ScanNumber(Sp: PAnsiChar): PAnsiChar;
var
  Start: PAnsiChar;
begin
  Start := Sp;
  if Sp^ = '-' then
    Inc(Sp);
  if Sp^ in ['0' .. '9'] then
  begin
    if (Sp^ = '0') and (Sp[1] in ['0' .. '9']) and (joStrict in FOptions) then
      InvalidCharacter(Sp);
    repeat
      Inc(Sp);
    until not (Sp^ in ['0' .. '9']);
  end
  else if not ((Sp^ = '.') and not (joStrict in Options)) then
    InvalidCharacter(Sp);
  if Sp^ = '.' then
  begin
    Inc(Sp);
    if Sp^ in ['0' .. '9'] then
      repeat
        Inc(Sp);
      until not (Sp^ in ['0' .. '9'])
    else if joStrict in FOptions then
      InvalidCharacter(Sp);
  end;
  if Sp^ in ['e', 'E'] then
  begin
    Inc(Sp);
    if Sp^ in ['+', '-'] then
      Inc(Sp);
    if not (Sp^ in ['0' .. '9']) then
      InvalidCharacter(Sp);
    repeat
      Inc(Sp);
    until not (Sp^ in ['0' .. '9']);
  end;
  if not (Sp^ in [#13, #10, #0, '}', ']', ',', #9, ' ']) then
    InvalidCharacter(Sp);
  if Start^ = '.' then
    GrowParts(1)^ := -1 - ord('0');
  AddPiece(Start, Sp);
  Result := Sp;
end;

function TJSONScanner.ScanLineComment(Sp: PAnsiChar): PAnsiChar;
var
  Start: PAnsiChar;
begin
  Start := Sp;
  while not (Sp^ in [#0, #13, #10]) do
    Inc(Sp);
  AddPiece(Start, Sp);
  if Sp^ in [#13, #10] then
    Sp := ScanNewline(Sp);
  Result := Sp;
end;

function TJSONScanner.ScanSlashStarComment(Sp: PAnsiChar): PAnsiChar;
var
  Start: PAnsiChar;
begin
  Start := Sp;
  repeat
    while not (Sp^ in [#0, '*', #13, #10]) do
      Inc(Sp);
    if Sp^ = '*' then
    begin
      Inc(Sp);
      if Sp^ = '/' then
        break;
    end
    else if Sp^ in [#13, #10] then
      Sp := ScanNewline(Sp)
    else
      Error(SUnterminatedComment, [CurRow, 1 + CountChars(FCurLine, Sp)]);
  until false;
  AddPiece(Start, Sp - 1); // Loop breaks with Sp pointing at / in final */.
  Result := Sp + 1;
end;

function TJSONScanner.RecognizeKeyword(Sp: PAnsiChar; N: SizeInt): TJSONToken;
const
  U32Byte0Shift = {$ifdef ENDIAN_BIG} 24 {$else} 0 {$endif};
  U32Byte1Shift = {$ifdef ENDIAN_BIG} 16 {$else} 8 {$endif};
  U32Byte2Shift = {$ifdef ENDIAN_BIG} 8 {$else} 16 {$endif};
  U32Byte3Shift = {$ifdef ENDIAN_BIG} 0 {$else} 24 {$endif};
var
  sample: uint32;
begin
  Result := tkIdentifier;
  if N = 4 then
  begin
    sample := unaligned(PUint32(Sp)^);
    if sample = ord('t') shl U32Byte0Shift or ord('r') shl U32Byte1Shift or ord('u') shl U32Byte2Shift or ord('e') shl U32Byte3Shift then
      Result := tkTrue
    else if sample = ord('n') shl U32Byte0Shift or ord('u') shl U32Byte1Shift or ord('l') shl U32Byte2Shift or ord('l') shl U32Byte3Shift then
      Result := tkNull;
  end
  else if (N = 5) and
      (unaligned(PUint32(Sp)^) = ord('f') shl U32Byte0Shift or ord('a') shl U32Byte1Shift or ord('l') shl U32Byte2Shift or ord('s') shl U32Byte3Shift) and
      (Sp[4] = 'e') then
    Result := tkFalse;
end;

end.
