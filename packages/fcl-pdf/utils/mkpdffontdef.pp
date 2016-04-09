{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    This small program reads a TTF font file and creates a definition in a .ini file for later use

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

program mkpdffontdef;

uses sysutils, fpttfencodings, fpparsettf;

begin
  if (ParamCount<3) then
    begin
    writeln('Usage : ',ExtractFileName(paramstr(0)),' ttffilename encoding fntfilename');
    Halt(1);
    end;
  With TTFFileInfo.Create do
    try
      LoadFromFile(ParamStr(1));
      MakePDFFontDef(Paramstr(3),Paramstr(2),False)
    finally
      Free;
    end;
end.

