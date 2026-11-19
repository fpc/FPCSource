{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.c;

interface

uses
  syntax.highlighter, syntax.table;

const
  CKeywords =
    'alignas alignof auto break case const constexpr continue default do else enum extern for goto if inline register restrict return sizeof static static_assert struct switch thread_local typedef typeof typeof_unqual union volatile while _Alignas _Alignof _Atomic _BitInt _Generic _Noreturn _Static_assert _Thread_local';
  CTypes =
    'bool char double float int long short signed unsigned void _Bool _Complex _Decimal32 _Decimal64 _Decimal128 _Imaginary true false nullptr NULL';

type

  { TCSyntaxHighlighter }

  TCSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

// Add the comment, string and number forms shared by C and C++
procedure AddCForms(var aDesc: TLanguageDescription);

implementation

procedure AddCForms(var aDesc: TLanguageDescription);

begin
  LineComment(aDesc, '//');
  BlockComment(aDesc, '/*', '*/');
  StringForm(aDesc, 'u8"', '"');
  StringForm(aDesc, 'u"', '"');
  StringForm(aDesc, 'U"', '"');
  StringForm(aDesc, 'L"', '"');
  StringForm(aDesc, 'u8''', '''', '\', False, False, shCharacters);
  StringForm(aDesc, 'u''', '''', '\', False, False, shCharacters);
  StringForm(aDesc, 'U''', '''', '\', False, False, shCharacters);
  StringForm(aDesc, 'L''', '''', '\', False, False, shCharacters);
  StringForm(aDesc, '"', '"');
  StringForm(aDesc, '''', '''', '\', False, False, shCharacters);
  aDesc.Numbers.Hex := True;
  aDesc.Numbers.Binary := True;
  aDesc.Numbers.LeadingOctal := True;
  aDesc.Numbers.HexExponent := True;
  aDesc.Numbers.Suffixes := 'uUlLfF';
end;


class function TCSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('c', CKeywords, CTypes);
  AddCForms(Result);
  Result.Directive := '#';
  Result.Numbers.Separators := True;
  Result.Numbers.Separator := '''';
end;

initialization
  TCSyntaxHighlighter.Register;
end.
