{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Lua syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.lua;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TLuaSyntaxHighlighter }

  TLuaSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TLuaSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('lua', 'and break do else elseif end for function goto if in local not or repeat return then until while', 'true false nil');
  LineComment(Result, '--');
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''');
  Result.Flavor := lfLua;
  Result.Numbers.Hex := True;
  Result.Numbers.HexExponent := True;
end;

initialization
  TLuaSyntaxHighlighter.Register;
end.
