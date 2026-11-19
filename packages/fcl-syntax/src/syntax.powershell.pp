{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    PowerShell syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.powershell;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TPowerShellSyntaxHighlighter }

  TPowerShellSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TPowerShellSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('powershell ps1',
    'begin break catch class clean continue data define do dynamicparam else elseif end enum exit filter finally for foreach from function hidden if in inlinescript parallel param process return sequence static switch throw trap try until using var while workflow', '$true $false $null');
  Result.CaseSensitive := False;
  Result.Flavor := lfPowerShell;
  BlockComment(Result, '<#', '#>');
  LineComment(Result, '#');
  StringForm(Result, '"', '"', '`', True, True);
  StringForm(Result, '''', '''', #0, True, True);
  Result.IdentifierStart := Result.IdentifierStart + ['$'];
  Result.IdentifierRest := Result.IdentifierRest + ['$', '-', ':'];
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Suffixes := 'uUlLdDnNkbmgtpKBMGTP';
end;

initialization
  TPowerShellSyntaxHighlighter.Register;
end.
