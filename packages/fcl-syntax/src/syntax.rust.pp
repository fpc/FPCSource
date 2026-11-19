{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rust syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.rust;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TRustSyntaxHighlighter }

  TRustSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TRustSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('rust rs',
    'as async await break const continue crate dyn else enum extern fn for if impl in let loop match mod move mut pub ref return self Self static struct super trait type unsafe use where while abstract become box do final gen macro override priv try typeof unsized virtual yield', 'bool char str i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 true false');
  LineComment(Result, '//');
  BlockComment(Result, '/*', '*/', True);
  StringForm(Result, 'b"', '"');
  StringForm(Result, 'c"', '"');
  StringForm(Result, '"', '"', '\', True);
  StringForm(Result, 'b''', '''', '\', False, False, shCharacters);
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  Result.Flavor := lfRust;
  Result.Attribute := '#';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.Separators := True;
  Result.Numbers.Suffixes := 'uifsize0123456789';
end;

initialization
  TRustSyntaxHighlighter.Register;
end.
