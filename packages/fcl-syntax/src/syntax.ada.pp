{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Ada syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.ada;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TAdaSyntaxHighlighter }

  TAdaSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TAdaSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('ada',
    'abort abs abstract accept access aliased all and array at begin body case constant declare delay delta digits do else elsif end entry exception exit for function generic goto if in interface is limited loop mod new not null of or others out overriding package parallel pragma private procedure protected raise range record rem renames requeue return reverse select separate some subtype synchronized tagged task terminate then type until use when while with xor', 'boolean character duration float integer natural positive string wide_character wide_string true false');
  Result.CaseSensitive := False;
  Result.Flavor := lfAda;
  LineComment(Result, '--');
  StringForm(Result, '"', '"', #0, False, True);
  StringForm(Result, '''', '''', #0, False, False, shCharacters);
  Result.Numbers.Based := True;
  Result.Numbers.Separators := True;
end;

initialization
  TAdaSyntaxHighlighter.Register;
end.
