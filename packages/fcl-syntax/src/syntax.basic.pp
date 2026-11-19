{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    BASIC syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.basic;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TBasicSyntaxHighlighter }

  TBasicSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TBasicSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('basic vb freebasic',
    'addhandler addressof alias and andalso as asm byref byval call case catch class common const constructor continue declare delegate delete dim do each else elseif end enum erase error event exit extern finally for friend function get global gosub goto handles if implements imports in inherits input interface is let lib loop me mod module namespace new next not on operator option or orelse overloads overrides preserve print private property protected public raiseevent read redim rem removehandler restore resume return select set shared static step stop sub synclock then throw to try type typeof union using virtual wend while with withevents write xor', 'boolean byte char currency date decimal double integer long longint longptr object sbyte short single string ubyte uinteger ulong ulongint ushort variant void true false nothing');
  Result.CaseSensitive := False;
  Result.Flavor := lfBasic;
  BlockComment(Result, '/''', '''/', True);
  LineComment(Result, '''');
  LineComment(Result, 'rem', False, True);
  StringForm(Result, '!"', '"');
  StringForm(Result, '"', '"', #0, False, True);
  Result.Directive := '#';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.Suffixes := 'uUlLfFdD%!#&';
end;

initialization
  TBasicSyntaxHighlighter.Register;
end.
