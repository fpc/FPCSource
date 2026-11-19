{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    PHP syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.php;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TPhpSyntaxHighlighter }

  TPhpSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TPhpSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('php',
    'abstract and array as break callable case catch class clone const continue declare default die do echo else elseif empty enddeclare endfor endforeach endif endswitch endwhile enum eval exit extends final finally fn for foreach function global goto if implements include include_once instanceof insteadof interface isset list match namespace new or print private protected public readonly require require_once return static switch throw trait try unset use var while xor yield', 'bool float int iterable mixed never object string void false true null');
  Result.CaseSensitive := False;
  LineComment(Result, '//');
  LineComment(Result, '#');
  BlockComment(Result, '/*', '*/');
  StringForm(Result, '"', '"', '\', True);
  StringForm(Result, '''', '''', '\', True);
  StringForm(Result, '`', '`', '\', True);
  Result.Flavor := lfPhp;
  Result.Attribute := '#[';
  Result.IdentifierStart := Result.IdentifierStart + ['$'];
  Result.IdentifierRest := Result.IdentifierRest + ['$'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.LeadingOctal := True;
  Result.Numbers.Separators := True;
end;

initialization
  TPhpSyntaxHighlighter.Register;
end.
