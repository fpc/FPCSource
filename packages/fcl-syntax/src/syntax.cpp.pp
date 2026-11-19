{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C++ syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.cpp;

interface

uses
  syntax.highlighter, syntax.table, syntax.c;

type

  { TCppSyntaxHighlighter }

  TCppSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TCppSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('cpp c++ cc h hpp',
    'alignas alignof and and_eq asm auto bitand bitor break case catch class co_await co_return co_yield compl concept const consteval constexpr constinit const_cast continue contract_assert decltype default delete do dynamic_cast else enum explicit export extern for friend goto if import inline module mutable namespace new noexcept not not_eq operator or or_eq private protected public register reinterpret_cast requires return sizeof static static_assert static_cast struct switch template this thread_local throw try typedef typeid typename union using virtual volatile while xor xor_eq', CTypes + ' char8_t char16_t char32_t wchar_t');
  AddCForms(Result);
  Result.Flavor := lfCpp;
  Result.Directive := '#';
  Result.Numbers.Separators := True;
  Result.Numbers.Separator := '''';
end;

initialization
  TCppSyntaxHighlighter.Register;
end.
