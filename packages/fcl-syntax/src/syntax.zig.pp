{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Zig syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.zig;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TZigSyntaxHighlighter }

  TZigSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TZigSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('zig',
    'addrspace align allowzero and anyframe anytype asm async await break callconv catch comptime const continue defer else enum errdefer error export extern fn for if inline linksection noalias noinline nosuspend opaque or orelse packed pub resume return section struct suspend switch test threadlocal try union unreachable usingnamespace var volatile while', 'anyerror bool c_char c_int c_long c_longdouble c_longlong c_short c_uint c_ulong c_ulonglong c_ushort comptime_float comptime_int f16 f32 f64 f80 f128 i8 i16 i32 i64 i128 isize noreturn type u8 u16 u32 u64 u128 usize void true false null undefined');
  LineComment(Result, '//');
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', '\', False, False, shCharacters);
  StringForm(Result, '@"', '"');
  Result.Flavor := lfZig;
  Result.Attribute := '@';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.HexExponent := True;
  Result.Numbers.Separators := True;
end;

initialization
  TZigSyntaxHighlighter.Register;
end.
