{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Perl syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.perl;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TPerlSyntaxHighlighter }

  TPerlSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TPerlSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('perl pl',
    'BEGIN CHECK END INIT UNITCHECK and bless break caller continue default die do dump else elsif eval exit for foreach format given goto if last local my next no not or our package print redo require return say state sub unless until use when while xor', 'undef __FILE__ __LINE__ __PACKAGE__');
  LineComment(Result, '#');
  StringForm(Result, '"', '"', '\', True);
  StringForm(Result, '''', '''', '\', True);
  StringForm(Result, '`', '`', '\', True);
  Result.Flavor := lfPerl;
  Result.IdentifierStart := Result.IdentifierStart + ['$', '@', '%'];
  Result.IdentifierRest := Result.IdentifierRest + ['$', '@', '%'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.LeadingOctal := True;
  Result.Numbers.Separators := True;
end;

initialization
  TPerlSyntaxHighlighter.Register;
end.
