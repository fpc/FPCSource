{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Windows batch file syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.batch;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TBatchSyntaxHighlighter }

  TBatchSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TBatchSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('batch bat cmd',
    'assoc break call cd chdir cls color copy date del dir do echo else endlocal erase exit for ftype goto if in md mkdir move path pause popd prompt pushd rd rem ren rename rmdir set setlocal shift start time title type ver verify vol', 'errorlevel exist defined');
  Result.CaseSensitive := False;
  Result.Flavor := lfBatch;
  LineComment(Result, 'rem', True, True);
  LineComment(Result, '::', True);
  StringForm(Result, '"', '"', '^');
  Result.Numbers.Hex := True;
end;

initialization
  TBatchSyntaxHighlighter.Register;
end.
