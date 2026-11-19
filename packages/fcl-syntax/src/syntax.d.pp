{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    D syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.d;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TDSyntaxHighlighter }

  TDSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TDSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('d',
    'abstract alias align asm assert auto body break case cast catch class const continue debug default delegate delete deprecated do else enum export extern final finally for foreach foreach_reverse function goto if immutable import in inout interface invariant is lazy mixin module new nothrow out override package pragma private protected public pure ref return scope shared static struct super switch synchronized template this throw try typeid typeof union unittest version while with __gshared __traits __vector __parameters', 'bool byte cdouble cent cfloat char creal dchar double float idouble ifloat int ireal long real short ubyte ucent uint ulong ushort void wchar true false null');
  LineComment(Result, '//');
  BlockComment(Result, '/+', '+/', True);
  BlockComment(Result, '/*', '*/');
  StringForm(Result, 'r"', '"', #0, True, False, shRawString);
  StringForm(Result, '`', '`', #0, True, False, shRawString);
  StringForm(Result, 'x"', '"', #0, True);
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  Result.Attribute := '@';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.HexExponent := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'uULfi';
end;

initialization
  TDSyntaxHighlighter.Register;
end.
