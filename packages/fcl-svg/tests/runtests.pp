{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Console test runner for the fcl-svg unit and golden tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program runtests;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

uses
{$IF defined(UNIX) and defined(UNICODERTL)}
  // A program of the unicode RTL converts its own strings.
  {$IFDEF FPC_DOTTEDUNITS}
     UnixApi.CWString,
  {$ELSE FPC_DOTTEDUNITS}
     cwstring,
  {$ENDIF FPC_DOTTEDUNITS}
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
     FpcUnit.Runners.Console,
{$ELSE FPC_DOTTEDUNITS}
     consoletestrunner,
{$ENDIF FPC_DOTTEDUNITS}
     svggoldens, svgstubfont, svgpixels, tcsvgtypes, tcsvgtrace,
     tcsvgread, tcsvgdom, tcsvgpath, tcsvgshapes, tcsvgstyle,
     tcsvgcascade, tcsvguse, tcsvgraster, tcsvgstroke, tcsvggradient, tcsvgpdf,
     tcsvgrender, tcsvgclip, tcsvgrefs, tcsvgtext,
{$IFDEF DARWIN}
     tcsvgcoretext,
{$ENDIF}
{$IFDEF WINDOWS}
     tcsvggdi,
{$ENDIF}
     tcsvgsvgfont, tcsvganim, tcsvgdiff;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fcl-svg test runner';
    Application.Run;
  finally
    Application.Free;
  end;
end.
