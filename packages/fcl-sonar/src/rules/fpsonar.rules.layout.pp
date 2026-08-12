{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    LEX-tier source-layout analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Layout;

{ LEX-tier layout rules:
  line length, trailing whitespace, tabs, long-numeric-literal underscores, digit grouping.
}

{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Ingest, FpSonar.Rules.Consts;

type
  { Flags trailing whitespace on a physical line (range over the run). }
  TRuleNoTrailingWhitespace = class(TRuleBase)
  public
    // Emits one issue per line ending in space/TAB, spanning the trailing run.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a TAB in a source line, excluding tabs inside string/char literals. }
  TRuleNoTabs = class(TRuleBase)
  public
    // Emits one issue per line containing a non-string TAB.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a physical line whose display length exceeds the limit. }
  TRuleLineTooLong = class(TRuleBase)
  public
    // Emits one issue per over-length line (point at column cMaxLength+1).
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a long numeric literal written without '_' separators. }
  TRuleLongNumericLiteralUnderscores = class(TRuleBase)
  public
    // Emits one issue per separator-free literal with >= cMinDigits digits.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a separated numeric literal whose digit grouping is irregular. }
  TRuleDigitGroupingStandard = class(TRuleBase)
  public
    // Emits one issue per '_'-separated literal with non-standard group sizes.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  FpSonar.Config;

const
  // Rule-spec default values.
  cMaxLength = 120;
  cTabWidth = 2;
  cReportEveryTab = False;
  cMinDigits = 5;
  cDecimalGroup = 3;
  cHexGroup = 4;
  cBinGroup = 4;

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyTrailing = 'rule.NoTrailingWhitespace.message';
  cKeyTabs = 'rule.NoTabs.message';
  cKeyLineLong = 'rule.LineTooLong.message';
  cKeyNumUnderscores = 'rule.LongNumericLiteralUnderscores.message';
  cKeyGrouping = 'rule.DigitGroupingStandard.message';

  { ---- UTF-8 / display-length primitives (own your primitives — no locale RTL) }

// Number of UTF-8 code points in aText. The 1-based column count of a whole line.
function CodePointCount(const aText: string): integer;
var
  i: integer;
  lByte: byte;
begin
  Result := 0;
  for i := 1 to Length(aText) do
  begin
    lByte := byte(aText[i]);
    if (lByte < $80) or (lByte > $BF) then
      Inc(Result);
  end;
end;


// Display length of aText: each code point is one column, except a TAB counts aTabWidth columns.
function DisplayLength(const aText: string; aTabWidth: integer): integer;
var
  i: integer;
  lByte: byte;
begin
  Result := 0;
  for i := 1 to Length(aText) do
  begin
    lByte := byte(aText[i]);
    if (lByte < $80) or (lByte > $BF) then
    begin
      if aText[i] = #9 then
        Inc(Result, aTabWidth)
      else
        Inc(Result);
    end;
  end;
end;


// 1-based code-point column of the byte at aByteIndex. Converts a byte position to a char column.
function ColumnOfByte(const aText: string; aByteIndex: integer): integer;
var
  i: integer;
  lByte: byte;
begin
  Result := 1;
  for i := 1 to aByteIndex - 1 do
  begin
    lByte := byte(aText[i]);
    if (lByte < $80) or (lByte > $BF) then
      Inc(Result);
  end;
end;


// 1-based byte index where the aCol-th code point starts, or 0 if the line has fewer than aCol code points.
function ByteOfColumn(const aText: string; aCol: integer): integer;
var
  i, lCol: integer;
  lByte: byte;
begin
  Result := 0;
  lCol := 0;
  for i := 1 to Length(aText) do
  begin
    lByte := byte(aText[i]);
    if (lByte < $80) or (lByte > $BF) then
    begin
      Inc(lCol);
      if lCol = aCol then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;


{ numeric-literal primitives  }

// The maximal human-written numeric lexeme on aLine starting at byte aByteStart:
// an optional base prefix ($/%/&) then the maximal run of base-appropriate
// digits and '_' separators. Decimal stops at 'e'/'E'/'.'; hex keeps A-F. Empty
// when aByteStart is out of range.
function ReconstructLexeme(const aLine: string; aByteStart: integer): string;
var
  i: integer;
  lPrefix: char;

  function InBase(aChar: char): boolean;
  begin
    case lPrefix of
      '$': Result := aChar in ['0'..'9', 'A'..'F', 'a'..'f'];
      '%': Result := aChar in ['0'..'1'];
      '&': Result := aChar in ['0'..'7'];
      else
        Result := aChar in ['0'..'9'];
    end;
  end;

begin
  Result := '';
  i := aByteStart;
  if (i < 1) or (i > Length(aLine)) then
    Exit;
  lPrefix := #0;
  if aLine[i] in ['$', '%', '&'] then
  begin
    lPrefix := aLine[i];
    Result := aLine[i];
    Inc(i);
  end;
  while (i <= Length(aLine)) and (InBase(aLine[i]) or (aLine[i] = '_')) do
  begin
    Result := Result + aLine[i];
    Inc(i);
  end;
end;


// The base prefix char of aLexeme ($/%/&), or #0 for a decimal literal.
function LexemePrefix(const aLexeme: string): char;
begin
  if (Length(aLexeme) > 0) and (aLexeme[1] in ['$', '%', '&']) then
    Result := aLexeme[1]
  else
    Result := #0;
end;


// Significant-digit count: every char except the optional base prefix and the '_' separators.
function SignificantDigits(const aLexeme: string): integer;
var
  i, lStart: integer;
begin
  Result := 0;
  if LexemePrefix(aLexeme) <> #0 then
    lStart := 2
  else
    lStart := 1;
  for i := lStart to Length(aLexeme) do
    if aLexeme[i] <> '_' then
      Inc(Result);
end;


// True iff aLexeme contains a '_' digit separator.
function HasSeparator(const aLexeme: string): boolean;
begin
  Result := Pos('_', aLexeme) > 0;
end;


// The standard group size for aPrefix's base, or 0 for a base with no defined
// grouping standard (octal '&') — which is never validated.
function GroupSizeFor(aPrefix: char): integer;
begin
  case aPrefix of
    '$': Result := cHexGroup;
    '%': Result := cBinGroup;
    '&': Result := 0;
    else
      Result := cDecimalGroup;
  end;
end;


// True iff aLexeme's '_'-separated groups follow the standard size for its base:
// the least-significant and every interior group are exactly the group size;
function IsGroupingRegular(const aLexeme: string): boolean;
var
  lPrefix: char;
  lGroup, i, lCur: integer;
  lDigits: string;
  lGroups: array of integer;
begin
  lPrefix := LexemePrefix(aLexeme);
  lGroup := GroupSizeFor(lPrefix);
  if lGroup <= 0 then
  begin
    Result := True;
    Exit;
  end;

  if lPrefix <> #0 then
    lDigits := Copy(aLexeme, 2, Length(aLexeme) - 1)
  else
    lDigits := aLexeme;

  // Split the digit part into group lengths on '_' (in left-to-right order).
  SetLength(lGroups, 0);
  lCur := 0;
  for i := 1 to Length(lDigits) do
    if lDigits[i] = '_' then
    begin
      SetLength(lGroups, Length(lGroups) + 1);
      lGroups[High(lGroups)] := lCur;
      lCur := 0;
    end
    else
      Inc(lCur);
  SetLength(lGroups, Length(lGroups) + 1);
  lGroups[High(lGroups)] := lCur;

  Result := True;
  for i := 0 to High(lGroups) do
  begin
    if lGroups[i] = 0 then
    begin
      // Empty group: leading/trailing/double '_' — always irregular.
      Result := False;
      Exit;
    end;
    if i = 0 then
    begin
      if lGroups[i] > lGroup then
      begin
        Result := False;
        Exit;
      end;
    end
    else if lGroups[i] <> lGroup then
    begin
      Result := False;
      Exit;
    end;
  end;
end;


{ TRuleNoTrailingWhitespace }

procedure TRuleNoTrailingWhitespace.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, lByteStart: integer;
  lLine: string;
  lStartCol, lEndCol: integer;
begin
  for i := 0 to High(aContext.Lines) do
  begin
    lLine := aContext.Lines[i];
    if Length(lLine) = 0 then
      Continue;
    // Trailing run only exists when the last byte is a space or TAB.
    if not (lLine[Length(lLine)] in [#32, #9]) then
      Continue;
    // Walk back over the maximal trailing space/TAB run to its first byte.
    lByteStart := Length(lLine);
    while (lByteStart > 1) and (lLine[lByteStart - 1] in [#32, #9]) do
      Dec(lByteStart);
    lStartCol := ColumnOfByte(lLine, lByteStart);
    lEndCol := CodePointCount(lLine);
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      i + 1, lStartCol, i + 1, lEndCol,
      FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
      FMetadata.MessageKey, [], Trim(lLine));
  end;
end;


{ TRuleNoTabs }

procedure TRuleNoTabs.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, j, lRow, lCol: integer;
  lLine: string;

// True iff a string/char literal token on aRow spans char-column aCol.
  function InStringLiteral(aRow, aCol: integer): boolean;
  var
    k, lSpanStart, lSpanEnd: integer;
  begin
    Result := False;
    for k := 0 to High(aContext.Tokens) do
      if aContext.Tokens[k].IsString
        and (aContext.Tokens[k].Row = aRow) then
      begin
        lSpanStart := aContext.Tokens[k].Col;
        lSpanEnd := lSpanStart + CodePointCount(aContext.Tokens[k].Text) - 1;
        if (aCol >= lSpanStart) and (aCol <= lSpanEnd) then
        begin
          Result := True;
          Exit;
        end;
      end;
  end;

begin
  for i := 0 to High(aContext.Lines) do
  begin
    lLine := aContext.Lines[i];
    lRow := i + 1;
    for j := 1 to Length(lLine) do
      if lLine[j] = #9 then
      begin
        lCol := ColumnOfByte(lLine, j);
        if not InStringLiteral(lRow, lCol) then
        begin
          aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
            lRow, lCol, lRow, lCol,
            FMetadata.Severity, FMetadata.Category,
            FMetadata.DefaultConfidence, FMetadata.MessageKey, [],
            Trim(lLine));
          // One issue per line unless reportEveryTab.
          if not cReportEveryTab then
            Break;
        end;
      end;
  end;
end;


{ TRuleLineTooLong }

procedure TRuleLineTooLong.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, lDisplay, lMax: integer;
  lLine: string;
begin
  lMax := aContext.Config.RuleParamInt(FMetadata.RuleId, 'maxLength',
    cMaxLength);
  for i := 0 to High(aContext.Lines) do
  begin
    lLine := aContext.Lines[i];
    lDisplay := DisplayLength(lLine, cTabWidth);
    if lDisplay > lMax then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        i + 1, lMax + 1, i + 1, lMax + 1,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey,
        [IntToStr(lDisplay), IntToStr(lMax)], Trim(lLine));
  end;
end;


{ TRuleLongNumericLiteralUnderscores }

procedure TRuleLongNumericLiteralUnderscores.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, lRow, lCol, lByte, lDigits: integer;
  lLine, lLexeme: string;
begin
  for i := 0 to High(aContext.Tokens) do
    if aContext.Tokens[i].IsNumber then
    begin
      lRow := aContext.Tokens[i].Row;
      lCol := aContext.Tokens[i].Col;
      if (lRow < 1) or (lRow > Length(aContext.Lines)) then
        Continue;
      lLine := aContext.Lines[lRow - 1];
      lByte := ByteOfColumn(lLine, lCol);
      if lByte < 1 then
        Continue;
      lLexeme := ReconstructLexeme(lLine, lByte);
      if lLexeme = '' then
        Continue;
      // Fires only on a separator-free literal (has '_' is DigitGroupingStandard's job).
      if HasSeparator(lLexeme) then
        Continue;
      lDigits := SignificantDigits(lLexeme);
      if lDigits >= cMinDigits then
        aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
          lRow, lCol, lRow, lCol + CodePointCount(lLexeme) - 1,
          FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
          FMetadata.MessageKey, [IntToStr(lDigits)], lLexeme);
    end;
end;


{ TRuleDigitGroupingStandard }

procedure TRuleDigitGroupingStandard.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, lRow, lCol, lByte: integer;
  lLine, lLexeme: string;
begin
  for i := 0 to High(aContext.Tokens) do
    if aContext.Tokens[i].IsNumber then
    begin
      lRow := aContext.Tokens[i].Row;
      lCol := aContext.Tokens[i].Col;
      if (lRow < 1) or (lRow > Length(aContext.Lines)) then
        Continue;
      lLine := aContext.Lines[lRow - 1];
      lByte := ByteOfColumn(lLine, lCol);
      if lByte < 1 then
        Continue;
      lLexeme := ReconstructLexeme(lLine, lByte);
      if lLexeme = '' then
        Continue;
      // Fires only on a separated literal (no '_' is LongNumericLiteralUnderscores's job).
      if not HasSeparator(lLexeme) then
        Continue;
      if not IsGroupingRegular(lLexeme) then
        aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
          lRow, lCol, lRow, lCol + CodePointCount(lLexeme) - 1,
          FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
          FMetadata.MessageKey, [], lLexeme);
    end;
end;


// Builds the LineTooLong metadata declaring its integer 'maxLength' param.
function LineTooLongMeta: TRuleMetadata;
begin
  Result := TRuleMetadata.Make('LineTooLong', rtLex, rfLineText, sevMinor, itCodeSmell,
    cfHigh, True, cKeyLineLong);
  Result.AddParam('maxLength', rpkInt, cMaxLength);
  Result.Description :=
    'Flags a line longer than the configured maximum length.';
end;


initialization
  RegisterRule(TRuleNoTrailingWhitespace.Create(TRuleMetadata.Make(
    'NoTrailingWhitespace', rtLex, rfLineText, sevMinor, itCodeSmell, cfHigh,
    True, cKeyTrailing).WithDescription(
    'Flags trailing whitespace at the end of a line.')));
  RegisterMessage(cKeyTrailing, SNoTrailingWhitespace);

  RegisterRule(TRuleNoTabs.Create(TRuleMetadata.Make(
    'NoTabs', rtLex, rfLineText, sevMinor, itCodeSmell, cfHigh, True, cKeyTabs).WithDescription(
    'Flags tab characters; use spaces.')));
  RegisterMessage(cKeyTabs, SNoTabs);

  RegisterRule(TRuleLineTooLong.Create(LineTooLongMeta));
  RegisterMessage(cKeyLineLong, SLineTooLong);

  RegisterRule(TRuleLongNumericLiteralUnderscores.Create(TRuleMetadata.Make(
    'LongNumericLiteralUnderscores', rtLex, rfTokenStream, sevInfo, itCodeSmell,
    cfHigh, True, cKeyNumUnderscores).WithDescription(
    'Flags a long numeric literal that should use _ digit separators.')));
  RegisterMessage(cKeyNumUnderscores, SLongNumericLiteralUnderscores);

  RegisterRule(TRuleDigitGroupingStandard.Create(TRuleMetadata.Make(
    'DigitGroupingStandard', rtLex, rfTokenStream, sevInfo, itCodeSmell, cfHigh,
    True, cKeyGrouping).WithDescription(
    'Flags irregular digit grouping in a numeric literal.')));
  RegisterMessage(cKeyGrouping, SDigitGroupingStandard);

end.
