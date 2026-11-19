{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Ruby syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.ruby;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TRubySyntaxHighlighter }

  TRubySyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TRubySyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('ruby rb',
    'BEGIN END alias and begin break case class def defined? do else elsif end ensure for if in module next not or redo rescue retry return self super then undef unless until when while yield __FILE__ __LINE__ __ENCODING__',
    'true false nil');
  BlockComment(Result, '=begin', '=end');
  LineComment(Result, '#');
  StringForm(Result, '"', '"', '\', True);
  StringForm(Result, '''', '''', '\', True);
  StringForm(Result, '`', '`', '\', True);
  Result.Flavor := lfRuby;
  Result.IdentifierRest := Result.IdentifierRest + ['?', '!'];
  Result.IdentifierStart := Result.IdentifierStart + ['@', '$'];
  Result.IdentifierRest := Result.IdentifierRest + ['@', '$'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'ri';
end;

initialization
  TRubySyntaxHighlighter.Register;
end.
