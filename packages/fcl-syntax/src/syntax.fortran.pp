{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Fortran syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.fortran;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TFortranSyntaxHighlighter }

  TFortranSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TFortranSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('fortran',
    'abstract allocatable allocate assignment associate asynchronous backspace bind block call case class close codimension common contains contiguous continue critical cycle data deallocate deferred dimension do elemental else elseif elsewhere end enddo endif entry enum enumerator equivalence error exit extends external final flush forall format function generic goto if implicit import in include inout intent interface intrinsic module namelist non_overridable nopass nullify only open operator optional out parameter pass pause pointer print private procedure program protected public pure read recursive result return rewind save select sequence stop submodule subroutine sync target then type use value volatile wait where while write', 'character complex double integer logical precision real true false');
  Result.CaseSensitive := False;
  Result.Flavor := lfFortran;
  LineComment(Result, '!');
  StringForm(Result, '"', '"', #0, False, True);
  StringForm(Result, '''', '''', #0, False, True);
  Result.Numbers.ExponentLetters := 'eEdDqQ';
  Result.Numbers.Suffixes := '_0123456789';
end;

initialization
  TFortranSyntaxHighlighter.Register;
end.
