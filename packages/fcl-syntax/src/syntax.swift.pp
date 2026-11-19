{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Swift syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.swift;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TSwiftSyntaxHighlighter }

  TSwiftSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TSwiftSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('swift',
    'associatedtype borrowing break case catch class consuming continue default defer deinit do else enum extension fallthrough fileprivate for func guard if import in init inout internal is let nonisolated open operator precedencegroup private protocol public repeat rethrows return some static struct subscript super switch throws throw try typealias var where while as await async actor', 'Any Bool Character Double Float Int Int8 Int16 Int32 Int64 Never Self String UInt UInt8 UInt16 UInt32 UInt64 Void true false nil');
  Result.Flavor := lfSwift;
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/', True);
  StringForm(Result, '"""', '"""', '\', True);
  StringForm(Result, '"', '"');
  StringForm(Result, '`', '`', #0);
  Result.Attribute := '@';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.HexExponent := True;
  Result.Numbers.Separators := True;
end;

initialization
  TSwiftSyntaxHighlighter.Register;
end.
