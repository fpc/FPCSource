{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Dockerfile syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.dockerfile;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TDockerSyntaxHighlighter }

  TDockerSyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

class function TDockerSyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('dockerfile', 'add arg cmd copy entrypoint env expose from healthcheck label maintainer onbuild run shell stopsignal user volume workdir', '');
  Result.CaseSensitive := False;
  Result.Flavor := lfDocker;
  LineComment(Result, '#', True);
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''', #0);
  Result.Numbers.Exponent := False;
end;

initialization
  TDockerSyntaxHighlighter.Register;
end.
