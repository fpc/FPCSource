{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C# syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.csharp;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TCSharpSyntaxHighlighter }

  TCSharpSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TCSharpSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('csharp cs c#',
    'abstract as base break case catch checked class const continue default delegate do else enum event explicit extern finally fixed for foreach goto if implicit in interface internal is lock namespace new operator out override params private protected public readonly ref return sealed sizeof stackalloc static struct switch this throw try typeof unchecked unsafe using virtual volatile while', 'bool byte char decimal double float int long object sbyte short string uint ulong ushort void true false null');
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/');
  StringForm(Result, '$@"', '"', #0, True, True);
  StringForm(Result, '@$"', '"', #0, True, True);
  StringForm(Result, '@"', '"', #0, True, True);
  StringForm(Result, '$"', '"');
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  Result.Flavor := lfCSharp;
  Result.Directive := '#';
  Result.IdentifierStart := Result.IdentifierStart + ['@'];
  Result.IdentifierRest := Result.IdentifierRest + ['@'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'uUlLfFdDmM';
end;

initialization
  TCSharpSyntaxHighlighter.Register;
end.
