{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown utils tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.Utils;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, contnrs,
  markdown.utils;

type

  { TTestMarkdownUtils }

  TTestMarkdownUtils = class(TTestCase)
  private
    FEntities: TFPStringHashTable;
    FBuilder: TStringBuilder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsWhitespaceChar;
    procedure TestIsWhitespace;
    procedure TestMustEscape;
    procedure TestIsStringOfChar;
    procedure TestCopyUpTo;
    procedure TestCopySkipped;
    procedure TestCopyMatching;
    procedure TestStartsWithWS;
    procedure TestLeadingWhitespace;
    procedure TestLengthWhiteSpaceCorrected;
    procedure TestRemoveWS;
    procedure TestStripWhitespace;
    procedure TestHtmlEscape;
    procedure TestUrlEscape;
    procedure TestParseEntityString;
    procedure TestCheckForEntity;
    procedure TestIsRegexMatch;
    procedure TestIsUnicodePunctuation;
    procedure TestCountStartChars;
    procedure TestToUnicodeChars;
    procedure TestTransformTabs;
  end;

implementation

procedure TTestMarkdownUtils.SetUp;
begin
  FEntities := TFPStringHashTable.Create;
  FEntities.Add('amp', '&');
  FEntities.Add('lt', '<');
  FBuilder := TStringBuilder.Create;
end;

procedure TTestMarkdownUtils.TearDown;
begin
  FEntities.Free;
  FBuilder.Free;
end;

procedure TTestMarkdownUtils.TestIsWhitespaceChar;
begin
  AssertTrue('Space should be whitespace', isWhitespaceChar(' '));
  AssertTrue('Tab should be whitespace', isWhitespaceChar(#9));
  AssertTrue('Line Feed should be whitespace', isWhitespaceChar(#10));
  AssertFalse('Letter "a" should not be whitespace', isWhitespaceChar('a'));
  AssertFalse('Carriage Return is not considered whitespace by this implementation', isWhitespaceChar(#13));
end;

procedure TTestMarkdownUtils.TestIsWhitespace;
begin
  AssertTrue('Empty string is considered whitespace', isWhitespace(''));
  AssertTrue('String with only spaces is whitespace', isWhitespace(' '));
  AssertTrue('String with mixed whitespace is whitespace', isWhitespace(#9#10' '));
  AssertFalse('String with non-whitespace characters is not whitespace', isWhitespace(' a '));
end;

procedure TTestMarkdownUtils.TestMustEscape;
begin
  AssertTrue('! must be escaped', MustEscape('!'));
  AssertTrue('& must be escaped', MustEscape('&'));
  AssertTrue('\ must be escaped', MustEscape('\'));
  AssertTrue('` must be escaped', MustEscape('`'));
  AssertFalse('a must not be escaped', MustEscape('a'));
  AssertFalse('1 must not be escaped', MustEscape('1'));
  AssertFalse('space must not be escaped', MustEscape(' '));
end;

procedure TTestMarkdownUtils.TestIsStringOfChar;
begin
  AssertTrue('Empty string', IsStringOfChar(''));
  AssertTrue('Single character string', IsStringOfChar('a'));
  AssertTrue('String of identical chars', IsStringOfChar('---'));
  AssertFalse('String of non-identical chars', IsStringOfChar('--a'));
end;

procedure TTestMarkdownUtils.TestCopyUpTo;
begin
  AssertEquals('Stop at first excluded char', 'abc', CopyUpTo('abc#def', ['#', ';']));
  AssertEquals('No excluded chars present', 'abcdef', CopyUpTo('abcdef', ['#', ';']));
  AssertEquals('Excluded char at start', '', CopyUpTo('#abcdef', ['#', ';']));
  AssertEquals('Empty string', '', CopyUpTo('', ['#', ';']));
end;

procedure TTestMarkdownUtils.TestCopySkipped;
begin
  AssertEquals('Skip leading spaces', 'abc', CopySkipped('  abc', [' ']));
  AssertEquals('Skip leading tabs', 'abc', CopySkipped(#9#9'abc', [#9]));
  AssertEquals('Skip mixed leading whitespace', 'abc', CopySkipped(#9' abc', [' ', #9]));
  AssertEquals('No chars to skip', 'abc', CopySkipped('abc', [' ']));
  AssertEquals('String with only skippable chars', '', CopySkipped('  ', [' ']));
  AssertEquals('Empty string', '', CopySkipped('', [' ']));
end;

procedure TTestMarkdownUtils.TestCopyMatching;
begin
  AssertEquals('Match leading digits', '123', CopyMatching('123abc', ['0'..'9']));
  AssertEquals('No matching chars at start', '', CopyMatching('abc123', ['0'..'9']));
  AssertEquals('String with only matching chars', '123', CopyMatching('123', ['0'..'9']));
  AssertEquals('Empty string', '', CopyMatching('', ['0'..'9']));
end;

procedure TTestMarkdownUtils.TestStartsWithWS;
var
  Len: Integer;
begin
  AssertTrue('Char: one space', StartsWithWhitespace(' >', '>', Len));
  AssertEquals('Char: Length for one space', 1, Len);

  AssertTrue('Char: three spaces', StartsWithWhitespace('   >', '>', Len));
  AssertEquals('Char: Length for three spaces', 3, Len);

  AssertFalse('Char: four spaces (more than default wsLen=3)', StartsWithWhitespace('    >', '>', Len));

  AssertTrue('Char: no space', StartsWithWhitespace('>', '>', Len));
  AssertEquals('Char: Length for no space', 0, Len);

  AssertFalse('Char: wrong prefix', StartsWithWhitespace('x>', '>', Len));

  AssertTrue('String: one space', StartsWithWhitespace(' item', 'item', Len));
  AssertEquals('String: Length for one space', 1, Len);

  AssertTrue('String: three spaces', StartsWithWhitespace('   item', 'item', Len));
  AssertEquals('String: Length for three spaces', 3, Len);
  // Todo: take into account tabs
  // AssertTrue('String: one tab', StartsWithWhitespace(#9'item', 'item', Len));
  // AssertEquals('String: Length for one tab', 1, Len);
end;

procedure TTestMarkdownUtils.TestLeadingWhitespace;
var
  Tabs, Chars: Integer;
begin
  AssertEquals('Should be 0 for "abc"', 0, LeadingWhitespace('abc'));
  AssertEquals('Should be 2 for "  abc"', 2, LeadingWhitespace('  abc'));
  AssertEquals('Should be 4 for tab', 4, LeadingWhitespace(#9'abc'));
  AssertEquals('Should be 4 for space-tab', 4, LeadingWhitespace(' '#9'abc'));
  AssertEquals('Should be 4 for 2space-tab', 4, LeadingWhitespace('  '#9'abc'));
  AssertEquals('Should be 4 for 3space-tab', 4, LeadingWhitespace('   '#9'abc'));
  AssertEquals('Should be 8 for 4space-tab', 8, LeadingWhitespace('    '#9'abc'));
  AssertEquals('Should be 8 for 2 tabs', 8, LeadingWhitespace(#9#9'abc'));

  AssertEquals('Check returned spaces', 4, LeadingWhitespace(' '#9'abc', Tabs, Chars));
  AssertEquals('Check returned tabs', 1, Tabs);
  AssertEquals('Check returned whitespace chars', 2, Chars);
end;

procedure TTestMarkdownUtils.TestLengthWhiteSpaceCorrected;
begin
  AssertEquals('Length of "abc"', 3, lengthWhitespaceCorrected('abc'));
  AssertEquals('Length of tab', 4, lengthWhiteSpaceCorrected(#9));
  AssertEquals('Length of "a<tab>b"', 5, lengthWhiteSpaceCorrected('a'#9'b')); // 1 + 3 (tab at col 2) + 1
  AssertEquals('Length of "abcd<tab>"', 8, lengthWhiteSpaceCorrected('abcd'#9)); // 4 + 4 (tab at col 5)
end;

procedure TTestMarkdownUtils.TestRemoveWS;
begin
  AssertEquals('Remove 2 spaces', 'abc', RemoveLeadingWhiteSpace('  abc', 2));
  AssertEquals('Remove 2 of 4 spaces', '  abc', RemoveLeadingWhiteSpace('    abc', 2));
  AssertEquals('Remove 1 tab (width 4)', 'abc', RemoveLeadingWhiteSpace(#9'abc', 4));
  AssertEquals('Remove 2 spaces from tab (width 4)', '  abc', RemoveLeadingWhiteSpace(#9'abc', 2));
  AssertEquals('Remove space and tab (total width 4)', 'abc', RemoveLeadingWhiteSpace(' '#9'abc', 4));
end;

procedure TTestMarkdownUtils.TestStripWhitespace;
begin
  AssertEquals('Strip from " a b c "', 'abc', stripWhitespace(' a b c '));
  AssertEquals('Strip with tabs and newlines', 'abc', stripWhitespace(#9'a'#10'b c'));
  AssertEquals('Strip from "abc"', 'abc', stripWhitespace('abc'));
  AssertEquals('Strip only whitespace', '', stripWhitespace(' '#9#10));
end;

procedure TTestMarkdownUtils.TestHtmlEscape;
begin
  AssertEquals('Escape <', '&lt;', HtmlEscape('<'));
  AssertEquals('Escape >', '&gt;', HtmlEscape('>'));
  AssertEquals('Escape "', '&quot;', HtmlEscape('"'));
  AssertEquals('Escape &', '&amp;', HtmlEscape('&'));
  AssertEquals('Escape a', 'a', HtmlEscape('a'));
  AssertEquals('Escape full string', '&lt;a href=&quot;url&quot;&gt; &amp; b', HtmlEscape('<a href="url"> & b'));
end;

procedure TTestMarkdownUtils.TestUrlEscape;
begin
  AssertEquals('URL Escape "a"', 'a', urlEscape('a'));
  AssertEquals('URL Escape "&"', '&amp;', urlEscape('&')); // Ampersand should be HTML-escaped
  AssertEquals('URL Escape "["', '%5B', urlEscape('[')); // Square bracket
  AssertEquals('URL Escape "`"', '%60', urlEscape('`')); // Backtick
  AssertEquals('URL Escape "é"', '%C3%A9', urlEscape('é')); // Unicode char é (UTF-8 bytes C3 A9)
  AssertEquals('URL Escape "€"', '%E2%82%AC', urlEscape('€')); // Unicode char € (UTF-8 bytes E2 82 AC)
  AssertEquals('URL Escape full string', 'a path with &amp; %E2%82%AC', urlEscape('a path with & €'));
end;

procedure TTestMarkdownUtils.TestParseEntityString;
begin
  AssertEquals('Parse &amp;', '&', parseEntityString(FEntities, '&amp;'));
  AssertEquals('Parse &lt;', '<', parseEntityString(FEntities, '&lt;'));
  AssertEquals('Parse unknown', '', parseEntityString(FEntities, '&unknown;'));
  AssertEquals('Parse numeric &#60;', '<', parseEntityString(FEntities, '&#60;'));
  AssertEquals('Parse numeric &#8364;', '€', parseEntityString(FEntities, '&#8364;'));
  AssertEquals('Parse invalid numeric &#0;', #$FFFD, UTF8Decode(parseEntityString(FEntities, '&#0;')));
end;

procedure TTestMarkdownUtils.TestCheckForEntity;
begin
  AssertEquals('Check for &amp without ;', 5, CheckForTrailingEntity('&amp;'));
  AssertEquals('Check for test&amp without ;', 5, CheckForTrailingEntity('test&amp;'));
  AssertEquals('Check for &amp with ;', 5, CheckForTrailingEntity('&amp;')); // Semicolon is not alphanumeric, so it fails
  AssertEquals('Check with space', 5, CheckForTrailingEntity('test &amp;')); // Space fails the check
end;

procedure TTestMarkdownUtils.TestIsRegexMatch;
begin
  AssertTrue('Substring match', isRegexMatch('content', 'ont'));
  AssertTrue('Start anchor match', isRegexMatch('content', '^con'));
  AssertFalse('Start anchor fail', isRegexMatch('content', '^ont'));
  AssertFalse('Empty content never matches', isRegexMatch('', 'a'));
end;

procedure TTestMarkdownUtils.TestIsUnicodePunctuation;
begin
  AssertTrue('. should be punctuation', isUnicodePunctuation('.'));
  AssertTrue('! should be punctuation', isUnicodePunctuation('!'));
  AssertFalse('"a" should not be punctuation', isUnicodePunctuation('a'));
  AssertFalse('"7" should not be punctuation', isUnicodePunctuation('7'));
end;

procedure TTestMarkdownUtils.TestCountStartChars;
begin
  AssertEquals('Count 3 chars', 3, CountStartChars('---abc', '-'));
  AssertEquals('Count 0 chars', 0, CountStartChars('abc---', '-'));
  AssertEquals('Count 4 chars', 4, CountStartChars('----', '-'));
  AssertEquals('Count in empty string', 0, CountStartChars('', '-'));
end;

procedure TTestMarkdownUtils.TestToUnicodeChars;
var
  arr: TUnicodeCharDynArray;
begin
  arr := ToUnicodeChars('a€b');
  AssertEquals('Array length for "a€b"', 3, Length(arr));
  if Length(arr) = 3 then
  begin
    AssertEquals('First char should be a', 'a', arr[0]);
    AssertEquals('Second char should be €','€', arr[1]);
    AssertEquals('Third char should be b', 'b', arr[2]);
  end;
  AssertEquals('Array length for empty string', 0, Length(ToUnicodeChars('')));
end;

procedure TTestMarkdownUtils.TestTransformTabs;
begin
  AssertEquals('Transform leading tab', '    abc', TransformTabs(#9'abc'));
  AssertEquals('Transform leading space and tab', '    abc', TransformTabs(' '#9'abc'));
  AssertEquals('No change for no tabs', 'abc', TransformTabs('abc'));
  AssertEquals('No change for tab in middle', 'abc'#9'def', TransformTabs('abc'#9'def'));
end;

initialization
  RegisterTest(TTestMarkdownUtils);
end.
